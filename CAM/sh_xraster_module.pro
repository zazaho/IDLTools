;+
; NAME: sh_xraster_module
; PURPOSE: this is collection of procedures (no function) to deal
;    with raster
; CATEGORY:  internal
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                              and filled in sh_ximage_load_variable
; PROCEDURES:
;   raster_extract: extract the interesting data from a raster 
;   sh_xraster_vertex: compute the vertexes of the different pointings in the mosaic image
;   sh_xraster_plot_cube: compute the points in the cube of a point in the mosaic
;                        and call the plot routine
;   sh_xraster_plot_cube_single: plot all the pixels history on a single plot
;   sh_xraster_plot_cube_multi: each pixel history has its plot 
;   sh_xraster_plot_cube_together: plot only the interesting part of  the pixels history 
;
;   sh_xraster_set_window
;   sh_xraster_new_window
;   sh_xraster_ps_title
;
;   sh_xraster_sky_views: plot the bounders of each sky pointing on the raster map
;   
; SEE ALSO:
;   sh_ximage
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
;   05-Nov-1999 : RG for idl version 5.0
;  R Gastaud March 1rst 2000 bug when out of mosaic
;  R Gastaud 17-may-2001 : add distortion 
;
; COPYRIGHT:
;-

;******************************

PRO sh_xraster_extract, raster, mydata, ack =ack

;
; NAME: sh_xraster_extract
; PURPOSE: extract the interesting data from a raster
; CATEGORY:  internal
; CALLING SEQUENCE:
;    sh_xraster_extract, raster, mydata, ack=ack
; INPUTS:
;   raster -- IDL structure : a raster
; OUTPUTS:
;   mydata -- IDL structure : what we need
; CALLED PROCEDURES AND FUNCTIONS:
;   pdsinfo2astr
; SEE ALSO:
;   sh_ximage
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
;  P Chanial  10-02-2000 : added flat field
; COPYRIGHT:
;
ack = 0              
IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $
    'sh_xraster_extract, raster, mydata, ack=ack'
   return
ENDIF 
myastr = pdsinfo2astr( raster )
; 16-may-2001
if (tag_exist(raster, 'dist_poly')) then begin
      if (tag_exist(raster, 'dist_xref')) then begin
         dist_xref = raster.calg.dist_xref
         dist_yref = raster.calg.dist_yref
      endif else begin
         dist_xref = 0  ; Aussel convention
         dist_yref = 0
      endelse
      mydata = {astr:myastr, from:raster.from, to:raster.to, cube:raster.cube,$
          mask:raster.mask, nscd:raster.nscd, flat:raster.flat,$
	  dist_poly:raster.calg.dist_poly,$
	  dist_backpoly:raster.calg.dist_backpoly,$
	  dist_xref:dist_xref, dist_yref:dist_yref }
endif else begin
      mydata = {astr:myastr, from:raster.from, to:raster.to, cube:raster.cube,$
          mask:raster.mask, nscd:raster.nscd, flat:raster.flat}
endelse	  
mydata.mask = mydata.mask and 1b  ; to get rid of complex mask

ack = 1
return

END

;******************************
PRO sh_xraster_vertex, global,x ,y , ack =ack

;
; NAME: sh_xraster_vertex
; PURPOSE: compute the vertexes of the different pointings in the mosaic image
;
; CATEGORY:  internal
; CALLING SEQUENCE:
;  sh_xraster_vertex, global,x ,y , ack =ack
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                              and filled in sh_ximage_load_variable
; OUTPUTS:
;   x -- fltarr(5,nscd) : the x coordinates of the 5 points of one sky pointing
;   y -- fltarr(5,nscd) : the y coordinates of the 5 points of one sky pointing
;
; CALLED PROCEDURES AND FUNCTIONS:
;  ad2xy, xy2ad
; SEE ALSO:
;   sh_ximage
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
; COPYRIGHT:
;
ack = 0              
IF N_PARAMS() LT 3 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $
    'sh_xraster_vertex, global,x ,y, ack =ack'
   return
ENDIF 
x = fltarr(5, (*global.raster).nscd)
y = x
xin = [-0.5,31.5,31.5,-0.5 ,-0.5]
yin = [-0.5,-0.5, 31.5,31.5,-0.5]
;toto = (*global.raster)
;save, toto, file='toto.xdr'
for i=0, (*global.raster).nscd -1 do begin     
     for j=0, 4 do begin
         if (tag_exist((*global.raster), 'dist_poly')) then begin  
             compute_distortion, xin[j], yin[j], (*global.raster).dist_poly[*,0], $
	          (*global.raster).dist_poly[*,1], xd, yd,$
	          (*global.raster).dist_xref, (*global.raster).dist_yref   
	     ;print, 'distortion'	  
         endif else begin
	     xd = xin[j] & yd = yin[j]
	     ;print, 'no distortion'
	 endelse
         xy2ad, xd, yd, (*global.raster).astr[i], ra, dec
	 ad2xy, ra, dec, *global.astr,  xx, yy
	 x[j,i] = round(10*xx) / 10.
	 y[j,i] = round(10*yy) / 10.
     endfor
endfor
ack = 1
return
END

;******************************
pro sh_xraster_plot_cube, global, print_info = print_info
;
; NAME: sh_xraster_plot_cube
; PURPOSE: compute the coordinates x, y and scd which correspond to
;    one point in the pixel map and call the function to plot
;
; CATEGORY:  internal
; CALLING SEQUENCE:
;  sh_xraster_plot_cube, global
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                              and filled in sh_ximage_load_variable
; OUTPUTS:
;
; CALLED PROCEDURES AND FUNCTIONS:
;  ad2xy, xy2ad
;  sh_xraster_plot_cube_single, sh_xraster_plot_cube_multi, sh_xraster_plot_cube_together
; SEE ALSO:
;   sh_ximage
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
; COPYRIGHT:
;

 ;t1 = systime(/second)
 t1 = systime() ; idl version 5.0

;** find the scds and x,y 
 xin = global.xcross
 yin = global.ycross
 x = fltarr((*global.raster).nscd)
 y = x
 n = 0
 scd = intarr((*global.raster).nscd)
 
 xy2ad, xin, yin, *global.astr, ra, dec
 myformat = '("x=",i2,"  y=",i2, "  scd=", i3)'
 ff = '("cube(",i2,",",i2,",",i4,":",i4,")")'
 fff ='("x=",i2,"  y=",i2, "  scd=", i3,"   cube(",i2,",",i2,",",i4,":",i4,")")' 
 for i=0, (*global.raster).nscd -1 do begin
     ad2xy, ra, dec, (*global.raster).astr[i],  xx, yy
     ; back distortion
     if (tag_exist((*global.raster), 'dist_backpoly')) then begin  
             compute_distortion, xx, yy, (*global.raster).dist_backpoly[*,0], $
	          (*global.raster).dist_backpoly[*,1], xxd, yyd, $
	          (*global.raster).dist_xref, (*global.raster).dist_yref    
	     ;print, 'back distortion'	  
     endif else begin
	     xxd = xx 
	     yyd = yy
	     ;print, 'no distortion'
     endelse
 
     xx = round(xxd)
     yy = round(yyd) ; RG 5-OCT-1999
     if (xx ge 0 and xx lt 32 and yy ge 0 and yy lt 32) then begin
	 x(n) = xx
	 y(n) = yy
	 scd(n) = i
         if keyword_set(print_info) then begin
            print, xx,yy,i,xx,yy, (*global.raster).from[i], (*global.raster).to[i] $
              ,format=fff
         endif
	 n = n+1
     endif
 endfor
 
 if keyword_set(print_info) then return
 
 fff='("pixel (",i3,",",i3,") of the
 mytitle = global.title+' pixel ('+strn(global.xcross)+','+strn(global.ycross)+') of the raster map'
 
 if (n gt 0) then begin
    x = x(0:n-1)
    y = y(0:n-1)
    scd = scd(0:n-1)

   ; plot 
   ; t2 = systime(/second)
   ;  t2 = systime()  ; idl version 5.0
   ;print, 'time for computing astrometry', t2-t1
   ;  save, global,x,y, scd, file='global.xdr'
   case 1 of
      global.Single_on   : sh_xraster_plot_cube_single, global, x, y, scd, mytitle
      global.Multi_on    : sh_xraster_plot_cube_multi, global, x, y, scd, mytitle
      global.Together_on : sh_xraster_plot_cube_Together, global, x, y, scd, mytitle
     else                : print, "Probleme de mode pour l'affichage du cube!!!"
   endcase
 endif else begin
   print,' out of mosaic'
 endelse
 
end

;******************************

pro sh_xraster_extract_cube, global, x, y, z, data, bad, badcount
; inputs: global, x, y, z
; outpus: data, bad, badcount

    data = reform((*global.raster).cube[x,y,z])
    ; look for flat correction
    if global.cube_flat_on then begin
       flat = (*global.raster).flat[x, y]
       if flat eq 0 then begin
         junk = dialog_message(['Flat value is zero !'], /error, $
           dialog_parent = global.base_sh_ximage)
         plot, [0]
         return
       endif
       data = data / flat
    endif

    mask = reform((*global.raster).mask[x,y,z])
    good = where(mask eq 0, count)
    bad  = where(mask ne 0, badcount)
    nb = n_elements(data)
    if global.cube_mask_on then begin
       case count of
          0 : data[*] = mean(data)
          1 : data[*] = data[good]
          else : if badcount gt 0 then begin
             data[bad] = interpol(data[good], good, bad)
             if good[0] gt 0 then data[0:good[0]-1] = data[good[0]]
             if good[count-1] lt nb-1 then data[good[count-1]+1:*] = data[good[count-1]]
                 endif
       endcase
    endif
    
end

;******************************

function sh_xraster_icolor, n, nmax

   if n lt 7 then begin
      color = sh_ximage_icolor(n)
   endif else begin
      color = fix(8 + float(!d.table_size) * n / nmax)
   endelse
   
   return, color
end

;******************************

 pro sh_xraster_plot_cube_single, global, x, y, scd, title
;
; NAME: sh_xraster_plot_cube_single 
; PURPOSE: plot the history of all the pixels contributing to one
;          perticular sky map.
;          here all plots are superimposed (with different colours)
;
; CATEGORY:  internal
; CALLING SEQUENCE:
;  sh_xraster_plot_cube_single, global, x, y, scd 
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                             and filled in sh_ximage_load_variable
;   x -- fltarr(nscd) : the x coordinates of sky pointing
;   y -- fltarr(nscd) : the y coordinates of sky pointing
;   scd -- intarr(nscd) : the index of the scds 
; OUTPUTS:
;
; CALLED PROCEDURES AND FUNCTIONS:
;  
; SEE ALSO:
;   sh_xraster_plot_cube
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
; COPYRIGHT:
;

 sh_xraster_ps_title, global, x, y, scd, title
 
 nscd = n_elements(scd)
 myformat = '("x=",i2,"  y=",i2, "  scd=", i5)'
 !p.multi = 0
 from =  (*global.raster).from(scd)
 to   = (*global.raster).to(scd)
 nframe = to - from + 1
 nz = (size((*global.raster).cube))[3]
 z = lindgen(nz)
 m = [0.]
 
 ; determining ranges
 if global.RasterScale_on then begin
    for i=0, nscd-1 do begin
       z = lindgen(nframe[i]) + from[i]
       sh_xraster_extract_cube, global, x[i], y[i], z, data
       m = [m, data]
    endfor	 
 endif else begin
    for i=0, nscd-1 do begin
       sh_xraster_extract_cube, global, x[i], y[i], z, data
       m = [m, data]
    endfor	 
 endelse
 
 m = m[1:*]
 ymin = min(m,max=ymax)
 
 plot, [0], xrange=[0,nz], yrange=[ymin,ymax], xstyle=3, ystyle=3, $
   title=title, charsize=0.7, background = sh_ximage_icolor('white'), $
   color = sh_ximage_icolor('black')
     
 if global.scdlim_on then begin
    ffrom = (*global.raster).from
    for i=1,(*global.raster).nscd-1 do begin
       oplot, [ffrom[i], ffrom[i]], [0, ymax], linestyle=1 
    endfor
 endif
 
 ;thin plot
 for i=0, nscd-1 do begin

     ; data extraction
     sh_xraster_extract_cube, global, x[i], y[i], z, data, bad, badcount
     
     ; data plot
     mycolor = sh_xraster_icolor(i, nscd)
     oplot, z, data, color = mycolor, thick = 0.5
     
     ; plot a cross if bad pixel
     if badcount gt 0 then begin
	oplot, bad, data[bad], psym=1, color=mycolor, thick = 0.5
     endif
     

  endfor
  
  ; thick plot
  for i=0, nscd-1 do begin

     ; data extraction
     sh_xraster_extract_cube, global, x[i], y[i], z, data, bad, badcount
     mycolor = sh_xraster_icolor(i, nscd)
     ; thick plot for the scd of interest
     oplot, z[from[i]:to[i]], data[from[i]:to[i]], thick=2, color=mycolor
     
     ; info display
     plottitle = strcompress(scd[i], /remove_all)
     ymed = median((*global.raster).cube(x[i], y[i],from[i]:to[i]))
     if (ymed gt ((ymax-ymin)/2+ymin)) then yy = 0.1*(ymax-ymin)+ymin else $
                                            yy = 0.9*(ymax-ymin)+ymin
     xx = from[i]
     xyouts,xx,yy, plottitle, color=mycolor, charthick=2

     ; plot of the limits of the scds
     oplot, [from[i], from[i]], [0, ymax], linestyle= 2, color=mycolor, thick=2
     oplot, [to[i], to[i]]    , [0, ymax], linestyle= 2, color=mycolor, thick=2
 
 endfor

 end

;******************************
pro sh_xraster_plot_cube_Together, global, x, y, scd, title
;
; NAME: sh_xraster_plot_cube_together 
; PURPOSE: plot the history of all the pixels contributing to one
;          perticular sky map.
;          here only the part of history which contributes to the sky map
;          is plotted
;
;          COMPARE is the name of the button
;
; CATEGORY:  internal
; CALLING SEQUENCE:
;  sh_xraster_plot_cube_together, global, x, y, scd 
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                             and filled in sh_ximage_load_variable
;   x -- fltarr(nscd) : the x coordinates of sky pointing
;   y -- fltarr(nscd) : the y coordinates of sky pointing
;   scd -- intarr(nscd) : the index of the scds 
; OUTPUTS:
;
; CALLED PROCEDURES AND FUNCTIONS:
;  
; SEE ALSO:
;   sh_xraster_plot_cube
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
;  P Chanial  10-02-2000 : interpolation of the bad pixels when 
;                          global.cube_mask_on=1, added flat
;                          correction, use of sh_xraster_icolor
;                          thick part displayed last
;  
; COPYRIGHT:
;

 sh_xraster_ps_title, global, x, y, scd, title
 nscd = n_elements(scd)
 MYFORMAT = '(" ",I2,",",I2, ",", I5)'
 !p.multi = 0
 from =  (*global.raster).from(scd-2)
 to   = (*global.raster).to(scd+2)
 nframe = to-from + 1
 start = 0     
 m = [0.]
 nbad = [0.]
 mbad = [0.]
 
 ; determining range and data to be plotted
 for i=0, nscd-1 do begin
    
    ;z = start+lindgen(to[i]-from[i]+1)
    z = from[i]+lindgen(to[i]-from[i]+1)  ; RG
                                ;(SH Mar 20 2003)
    data=-32000
    count_pix=0.0

    FOR jj=((x[i]-1)>0),((x[i]+1)<31) DO BEGIN
        FOR kk=((y[i]-1)>0),((y[i]+1)<31) DO BEGIN
            count_pix = count_pix+1.0
            sh_xraster_extract_cube, global,jj,kk, z, data_tmp, bad, badcount
            badcount=0
            IF data[0] EQ -32000 THEN BEGIN
                data = data_tmp 
            ENDIF ELSE BEGIN 
                data = data+data_tmp
            ENDELSE  
        ENDFOR
    ENDFOR 
    data = data/count_pix
    
    m = [m, data]
    if badcount gt 0 then begin
       nbad = [nbad, bad+start]
       mbad = [mbad, data[bad]]
    endif
    start = start +to[i]-from[i]+1
 endfor
 
 m = m[1:*]
 ymin = min(m, max = ymax)
 
 plot, [0], xrange = [0, start], yrange = [ymin, ymax], title = title, charsize = 0.7, $
   xstyle = 3, ystyle = 3, /nodata, $
   background = sh_ximage_icolor('white'), color = sh_ximage_icolor('black')
 
 start = 0
 
 for i=0, nscd-1 do begin
    
    data = m[start:start+nframe[i]-1]

    plottitle = string(x[i], y[i], scd[i], format=myformat)
    plottitle =  strcompress(plottitle, /remove_all)
    mycolor = sh_xraster_icolor(i, nscd)
    
    ; plot of the cube
    oplot, findgen(nframe[i])+start, data, color=mycolor
    
    ; plot of scd separation
    oplot, [start, start], [ymin, ymax], linestyle=2, color=mycolor
    
    ; info for each image can be displayed either on the top or
    ; on the bottom of the image
    ymed = median(data)
    xx = start
    if (ymed gt ((ymax-ymin)/2+ymin)) then begin
       yy = 0.1*(ymax-ymin)+ymin
    endif else begin
       yy = 0.9*(ymax-ymin)+ymin
    endelse
    xyouts, xx,yy, plottitle, color=mycolor
    
    ; plot a cross for bad pixels
    ibad = where(nbad ge start and nbad le start+nframe[i]-1, count)
    if count gt 0 then begin
       oplot, nbad[ibad], mbad[ibad], psym = 1, color = mycolor
    endif
    
    start = start + to[i]-from[i]+1
     
 endfor
    
end
;******************************
 pro sh_xraster_plot_cube_multi, global, x, y, scd, title
;
; NAME: sh_xraster_plot_cube_multi
; PURPOSE: plot the history of all the pixels contributing to one
;          perticular sky map.
;          here all plots are separated
;
; CATEGORY:  internal
; CALLING SEQUENCE:
;  sh_xraster_plot_cube_single, global, x, y, scd 
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                             and filled in sh_ximage_load_variable
;   x -- fltarr(nscd) : the x coordinates of sky pointing
;   y -- fltarr(nscd) : the y coordinates of sky pointing
;   scd -- intarr(nscd) : the index of the scds 
; OUTPUTS:
;
; CALLED PROCEDURES AND FUNCTIONS:
;  
; SEE ALSO:
;   sh_xraster_plot_cube
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
;  10-feb-20000 : PC expanded size of the plots, use of
;                 sh_xraster_extract_cube
 
; COPYRIGHT:
;

 sh_xraster_ps_title, global, x, y, scd, title
 
 nscd = n_elements(scd)
 ; y = x & x*y = nscd  ==> x*x = nscd ==> x = sqrt(nscd)
 x_ima = ceil( sqrt(1.*nscd))
 y_ima = ceil(nscd/float(x_ima))
 ;  we must have x_ima*y_ima >= nscd
 !p.multi = [0, x_ima, y_ima]
 !y.omargin = [1, 3]
 myformat = '("x=",i2,"  y=",i2, "  scd=", i5)'
 from = (*global.raster).from(scd)
 to   = (*global.raster).to(scd)
 nframe = to - from + 1
 nz = (size((*global.raster).cube))[3]
 z = lindgen(nz)
 m = [0.]
 
 ; determining ranges
 if global.RasterScale_on then begin
    for i=0, nscd-1 do begin
       z = lindgen(nframe[i]) + from[i]
       sh_xraster_extract_cube, global, x[i], y[i], z, data
       m = [m, data]
    endfor	 
 endif else begin
    for i=0, nscd-1 do begin
       sh_xraster_extract_cube, global, x[i], y[i], z, data
       m = [m, data]
    endfor	 
 endelse
 
 m = m[1:*]
 ymin = min(m,max=ymax)
 
 for i=0, nscd-1 do begin

    ; data extraction
    sh_xraster_extract_cube, global, x[i], y[i], z, data, bad, badcount
    
    ; data plot
    mycolor = sh_ximage_icolor('black')
    plottitle = string(x[i], y[i], scd[i], format=myformat)
    plottitle = strcompress(plottitle, /remove_all)
    plot, z, data, color = mycolor, thick = 1, xrange = [0, nz], $
      yrange = [ymin, ymax], xmargin = [3, 1], ymargin = [1, 1], $
      background = sh_ximage_icolor('white')
    
    ; plot a cross if bad pixel
    if badcount gt 0 then begin
       oplot, bad, data[bad], psym=1, color=mycolor, thick = 0.5
    endif
     
    ; plot of the limits of the scd of interest
    oplot, [from[i], from[i]], [0, ymax], linestyle= 2, color=mycolor
    oplot, [to[i], to[i]]    , [0, ymax], linestyle= 2, color=mycolor

    ; plot of all the scds limits
    if global.scdlim_on then begin
       ffrom = (*global.raster).from
       for j=1,(*global.raster).nscd-1 do begin
          oplot, [ffrom[j], ffrom[j]], [0, ymax], linestyle=1 
       endfor
    endif
 
 endfor
 
 XYOUTS, 0.5, 0.93, ALIGN=0.5, CHARSIZE=1, /NORMAL, title

end

;******************************
pro sh_xraster_ps_title, global, x, y, scd, pstitle
 MYFORMAT = '(  " (",I2,",",I2, ",", I5,")" )'
 plottitle = ''
 for i=0, n_elements(scd)-1 do begin
    plottitle = plottitle+string(x[i], y[i], scd[i], format=myformat)
 endfor
 pstitle = pstitle+strcompress(plottitle, /remove_all)
end

;******************************
pro sh_xraster_sky_views, global
;
; NAME: sh_xraster_sky_views
; PURPOSE: plot the bounders of each sky pointing on the raster map
;
; CATEGORY:  internal
; CALLING SEQUENCE:
;  sh_xraster_sky_views, global, ok = ok
; INPUTS:
;   global -- IDL structure : defined in sh_ximage_init_global
;                             and filled in sh_ximage_load_variable
; OUTPUTS:
;
; CALLED PROCEDURES AND FUNCTIONS:
;  sh_xraster_vertex
; SEE ALSO:
;   
; MODIFICATION HISTORY:
;  R Gastaud  28-SEP-1999
; COPYRIGHT:
;

 save_window, old

 widget_control, global.IMAGE_DRAW_ID, get_value=window
 wset, window
 
 sh_xraster_vertex, global,x ,y

 for i=0, (*global.raster).nscd -1 do begin
    sh_ximage_get_image_from_data, global, x(*,i), y(*,i), px, py
    plots, px, py, COL=sh_ximage_icolor('white'), /DEV, THICK=2, linestyle=0
    wset, window  ; bug idl 5.2 
 endfor 

 restore_window, old 

end

;---------------------------------------------------------------------

pro sh_ximage_cube_cleanup, id
 
 widget_control, id, get_uvalue=base_global
 valid = widget_info(base_global, /valid_id)
 if not valid then return
 widget_control, base_global, get_uvalue=global, /no_copy
 widget_control, global.cube_button_id, set_button = 0 
 global.cube_on = 0
 widget_control, base_global, set_uvalue=global, /no_copy
 
end

;----------------------------------------------------------------------

pro sh_ximage_cube_new_window, global

 save_window, old
 window, /free
 sh_xraster_plot_cube, global
 restore_window, old

end

;----------------------------------------------------------------------

pro sh_ximage_cube_event, event

 sh_ximage_subroutine_get_global, event, global
 if global.no_data then goto, closing
 save_window, old
 if tag_exist(event, 'x') then begin
    new_size = [event.x, event.y]
    nds = new_size-global.cube_draw_margin
    widget_control, global.cube_draw_id, draw_xsize = nds[0], $
      draw_ysize = nds[1]
    goto, suite
 endif

 case event.value of
   'File.New window' : sh_ximage_cube_new_window, global
   'File.Save' : sh_ximage_cube_save, global
   'File.Quit' : begin
                 base = global.base_cube
                 sh_ximage_subroutine_put_global, event, global
                 widget_control, base, /destroy
                 return
               end

   'Plot.Single'   : begin
                 global.single_on = 1
                 global.multi_on  = 0
                 global.together_on  = 0
               end
   'Plot.Multi'    :  begin
                 global.single_on = 0
         	 global.multi_on  = 1
         	 global.together_on  = 0
               end
   'Plot.Compare' :  begin
                 global.single_on = 0
         	 global.multi_on  = 0
         	 global.together_on  = 1
               end
               
   'Options.Toggle bad pixel interpolation': global.cube_mask_on = 1-global.cube_mask_on
   'Options.Toggle flat correction': global.cube_flat_on = 1-global.cube_flat_on
   'Options.Scds Limits'   : global.scdlim_on = 1-global.scdlim_on
   else : print, 'Probleme menu non trouve!'
 endcase
 
suite:
 restore_window, *global.cube_window
 sh_xraster_plot_cube, global  ; to redraw

 restore_window, old
closing:
 sh_ximage_subroutine_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_cube_save, global

 file = concat_dir(global.path, 'sh_xraster.ps')
 answer = simple_cw_form(                        $
    'Format : '     , ['PS', 'Encapsulated PS'], $
    'Colors ? '     , ['No', 'Yes'],             $
    'Orientation : ', ['Landscape', 'Portrait'], $
    'File Name : '  , file,                      $
    dialog_parent = global.base_sh_ximage,          $
    title = 'Save graph ...')
 
 if not answer.ok then return
 
 format       = answer.(1)
 encapsulated = not (format eq 'PS')
 color        = answer.(2) eq 'Yes'
 landscape    = answer.(3) eq 'Landscape'
 portrait     = answer.(3) eq 'Portrait'
 filename     = answer.(4)
 
 fdecomp, filename, disk, path, file
 global.path = path
 
 ok = can_i_write_file(filename, overwrite = file eq 'sh_xraster', group = global.base_sh_ximage)
 if not ok then return
 
 widget_control, /hourglass
 
 set_ps, filename, encapsulated = encapsulated, old = old, color = color, $
   bits = color*8, portrait = portrait, landscape = landscape
   
 sh_xraster_plot_cube, global
 end_ps, old

end

;---------------------------------------------------------------------

pro sh_ximage_cube_button_event, event

 sh_ximage_get_global, event, global 
 save_window, old
 global.cube_on = event.select

 if global.cube_on then begin

    base_cube = $
      widget_base(/floating, $
                  group_leader = global.base_sh_ximage, $
                  /column, $
                  /base_align_center, $
                  title = 'Cube Analysis', $
                  uvalue = 'base_cube', $
                  app_mbar = top_menu, $
                  /tlb_size_events)
    
    global.base_cube = base_cube
    
    base_cube_global = widget_base(base_cube)
    
    top_menu_desc = ['1\File', $
                     '0\New window', $
                     '0\Save', $
                     '2\Quit', $
                     '1\Plot', $
                     '0\Single', $
                     '0\Multi', $
                     '2\Compare', $
                     '1\Options', $
                     '0\Toggle bad pixel interpolation', $
                     '0\Toggle flat correction', $
                     '2\Scds Limits']
     
    top_menu = cw_pdmenu(top_menu, top_menu_desc, $
                     /mbar, $
                     /help, $
                     /return_full_name)
    
    
    
    global.cube_draw_id = widget_draw(base_cube, xsize = global.cube_draw_size[0], $
       ysize = global.cube_draw_size[1])

    child = widget_info(global.base_sh_ximage, /child)
    base_global = widget_info(child, /sibling)
    widget_control, top_menu, set_uvalue=base_global
   
    widget_control, top_menu, kill_notify = 'sh_ximage_cube_cleanup'
    widget_control, base_cube, /realize
    
    widget_control, base_cube, tlb_get_size = size
    global.cube_draw_margin = size - global.cube_draw_size
    
    xmanager, 'sh_ximage_cube', base_cube, /no_block
    
    widget_control, global.cube_draw_id, get_value = window_id
    wset, window_id
    save_window, window
    *global.cube_window = window
    sh_xraster_plot_cube, global
 
 endif else begin
    base = global.base_cube
    sh_ximage_put_global, event, global
    widget_control, base, /destroy
    return
 endelse
 
 restore_window, old
 sh_ximage_put_global, event, global
 
end

;---------------------------------------------------------------------

pro sh_xraster_module
end

