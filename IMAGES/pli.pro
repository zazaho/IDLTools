;+
; NAME:
; pli
;
; PURPOSE:
; plot an astronomical image in ra and dec coordinates
;
; CATEGORY:
; image display
;
; CALLING SEQUENCE:
; pli,image,header
;
; INPUTS:
; image  = 2D array with values to plot
; header = fits header that describes the coordinates
;
; KEYWORD PARAMETERS:
;  /nobox    : If set then do not plot the coordinate box
;  /overplot : If set then overplot the image on the current image
;  fill=0    : If set to 0 then do not fill the contour map
;  /correct  : If set then calculate more accurate do25 coordinates
;  /undistort: If set then make sure equal lengths ra,dec are equal
;              length on the plot
;  /help     : If set then show this help
;  boxcolor=c: If set then use this value for the box and the annotation
;  subimage=[x0,x1,y0,y1]: only use this sub region of the image
;  backgroundcolor=c: if set use this color before plotting the image
;  offset=[delta_a, delta_d]: shift RA and/or DEC by these entries
;  position=[x0,y0,x1,y1]: position of the box on the page. Calcuated
;           for undistort
;  label_precision=label_precision: number of decimals places in the
;  seconds to be used for labeling the axes [0]
;
; OPTIONAL INPUTS:
;         a=a calculated RA values
;         d=d calculated DEC values
; if these are given and of the right dimension then they are not
; recalculated. this saves a lot of time when re plotting an image
; OPTIONAL OUTPUTS:
;         a=a calculated RA values
;         d=d calculated DEC values
;
; MODIFICATION HISTORY:
;(SH Jul 10 2007) Initial version
;(TP Jul 24 2007) Addition of the keyword parameter 'offset'
;(SH Feb 24 2009) added boxcolor option
;(SH Apr  2 2012) added option to specify position of the box
;(SH Apr 19 2012) added backgroundcolor option
;corrected bug in !p.multi statement to plot box on top of existing box
;(SH Jan  9 2017): added label_precision keyword that controls the
;number of decimal places behind the seconds
;-

FUNCTION pli_ra_format,axis,index,ra
  common pli_common,pli_label_precision
;  return,string(ra,format='(f6.1)')
  h = floor(ra/15.)
  m = floor((ra/15.-h)*60.)
  str_h = n2s(h)+'!Uh!N'
  str_m = n2s(m)+'!Um!N'
  ;; do we want decimal places on the seconds
  if pli_label_precision lt 1 then begin
     s = round((ra/15.-h-m/60.)*3600.)
     IF s EQ 60 THEN BEGIN
        s=0
        m=m+1
     ENDIF 
     IF s NE 0 THEN str_s=n2s(s)+'!Us!N' ELSE str_s=''
  endif else begin
     s = floor((ra/15.-h-m/60.)*3600.)
     order = 10^round(pli_label_precision)
     d = round((ra/15.-h-m/60.-s/3600.)*3600.*order)
     str_s=n2s(s)+'!Us!N'+i2s(d,round(pli_label_precision))
  endelse
  return, str_h+str_m+str_s
END

FUNCTION pli_dec_format,axis,index,decIn
  common pli_common,pli_label_precision
;  return,string(decIn,format='(f6.1)')
  dec=decIn
  sign = dec GE 0
  dec = abs(dec)
  d = floor(dec)
  m = floor((dec-d)*60.)
  str_d = (['-','+'])[sign]+n2s(d)+'!Uo!N'
  str_m = n2s(m)+"'"
  if pli_label_precision lt 1 then begin
     s = round((dec-d-m/60.)*3600.)
     IF s EQ 60 THEN BEGIN
        s=0
        m=m+1
     ENDIF 
     IF s NE 0 THEN str_s=n2s(s)+'"' ELSE str_s=''
  endif else begin
     s = floor((dec-d-m/60.)*3600.)
     order = 10^round(pli_label_precision)
     d = round((dec-d-m/60.-s/3600.)*3600.*order)
     str_s=n2s(s)+'"'+i2s(d,round(pli_label_precision))
  endelse
  return, str_d+str_m+str_s
END

PRO pli, image, $
         header, $
         a=a, $
         d=d, $
         nobox=nobox, $
         overplot=overplot, $
         fill=fill, $
         min_value=min_value, $
         max_value=max_value, $
         nlevels=nlevels, $
         correct=correct, $
         subimage=subimage, $
         undistort=undistort, $
         offset=offset, $
         xrange=xrange, $
         yrange=yrange, $
         position=position, $
         backgroundcolor=backgroundcolor, $
         boxcolor=boxcolor, $
         help=help, $
         getcoords=getcoords, $
         path_xy=path_xy, $
         path_info=path_info, $
         path_data_coords=path_data_coords, $
         label_precision=label_precision, $
         _extra=_extra
  
  common pli_common,pli_label_precision
;; not called correctly show the help
  IF n_params() NE 2 THEN help=1
  
  ;; asked to be helped so show help file
  IF keyword_set(help) THEN BEGIN
     doc_library,'pli'
     return
  ENDIF

  ;;default values for keywords

  ;; by default do not make an overplot
  IF n_elements(overplot) EQ 0 THEN overplot = 0
  
  ;; by default do fill the contours to create an image
  IF n_elements(fill) EQ 0 THEN fill = 1
  
  ;; if overplotting do not make a box of course
  IF overplot NE 0 THEN nobox=1
  
  ;; by default use !p.color for the box
  IF n_elements(boxcolor) EQ 0 THEN boxcolor = !p.color
  
  ;; if not set min_value take the minimum data value
  IF n_elements(min_value) EQ 0 THEN BEGIN
     fin = where(finite(image) EQ 1)
     min_value=min(image[fin],max=max)
  ENDIF

  ;; if not set min_value take the minimum data value
  IF n_elements(max_value) EQ 0 THEN BEGIN
     fin = where(finite(image) EQ 1)
     max_value=max(image[fin])
  ENDIF

  ;; if not set max_value take the maximum data value
  IF n_elements(max_value) EQ 0 THEN max_value = max
  
  ;; if not set nlevels then set it to 10
  IF n_elements(nlevels) EQ 0 THEN nlevels = 10

  IF n_elements(xrange) NE 2 THEN xrange=[0,0]
  IF n_elements(yrange) NE 2 THEN yrange=[0,0]
  
  ;;by default set the offset values to [0,0]
  IF n_elements(offset) EQ 0 THEN offset = [0d0,0d0]
  
  ;;by default use no decimal places in the seconds of the labels
  IF n_elements(label_precision) EQ 0 THEN label_precision = 0
  pli_label_precision=label_precision
  
  ;; first make x y arrays to feed into the xyad routine
  nx = long(n_elements(image[*,0]))
  ny = long(n_elements(image[0,*]))
  
  IF n_elements(subimage) EQ 4 THEN BEGIN
     x1 = (subimage[1] < (nx-1L))
     x0 = (subimage[0] < (x1-1L))>0L
     y1 = (subimage[3] < (ny-1L))
     y0 = (subimage[2] < (y1-1L))>0L
  ENDIF ELSE BEGIN
     x1 = nx-1L
     x0 = 0L
     y1 = ny-1L
     y0 = 0L
  ENDELSE
  
  nx = x1-x0+1L
  ny = y1-y0+1L
  n = nx*ny
  indmax = n-1L

  ;; if a and d are given and have the right lengths do not redo the
  ;; calculation
  IF n_elements(a) NE (nx*ny) OR n_elements(d) NE (nx*ny) THEN BEGIN
     ;; The plus 1L is because the fits standard start at pixel 1,1
     ;; not 0,0
     ;; NOT TRUE xyad used IDL standard
     foo = lindgen(nx,ny)
     x = double(foo MOD nx) + x0; + 1L
     y = double(foo  /  nx) + y0; + 1L
     
     a = x
     d = y
     
     ;; cut it up into smaller chunks to avoid memory problems in xyad
     FOR i = 0L, ((n-1L) / 1000000L) DO BEGIN
        xyad, header, x[i*1000000L:(i*1000000L+999999L) < indmax], $
              y[i*1000000L:(i*1000000L+999999L) < indmax], $
              atmp, $
              dtmp, $
              _extra=_extra
        a[i*1000000L:(i*1000000L+999999L < indmax)] = atmp
        d[i*1000000L:(i*1000000L+999999L < indmax)] = dtmp
     ENDFOR
     
     ;; this is need for the large do25 map. Don't know if also
     ;; valid for other fields
     ;; What do these numbers mean anyways?
     IF keyword_set(correct) THEN BEGIN
        a = a + 0.20321932d0 -0.0020104132d0*a
        d = d -0.00094494132d0 + 0.0020149125d0*d
     ENDIF
     
  ENDIF

  ;; the keyword offset allows for a shift in the RA and DEC
  ;; directions by delta_a and delta_d
  IF (n_elements(offset) NE 0) THEN BEGIN
     delta_a = offset[0]        ;change in RA, given in degrees
     delta_d = offset[1]        ;change in DEC given in degrees
     a = a + delta_a            ;adjust the values of RA by delta_a
     d = d + delta_d            ;adjust the values of DEC by delta_d
  ENDIF
  
  IF NOT keyword_set(nobox) THEN BEGIN
     ;; determine the range for the box
     
     IF total(xrange EQ [0,0]) EQ 2 THEN BEGIN         min_a = min(a,max=max_a)
     ENDIF ELSE BEGIN
        min_a = min(xrange,max=max_a)
     ENDELSE 
     
     IF total(yrange EQ [0,0]) EQ 2 THEN BEGIN 
        min_d = min(d,max=max_d)
     ENDIF ELSE BEGIN
        min_d = min(yrange,max=max_d)
     ENDELSE 
     
     IF keyword_set(undistort) THEN BEGIN
        
        ra_range = (max_a-min_a)*cos((min_d+max_d)/2d0*!dpi/180.) ;; in arcsec not sec
        dec_range = max_d-min_d
        
        ;; make sure we have a plot window to plot in
        IF (!d.name EQ 'X') AND (!d.window EQ -1) THEN window
        
        pix_per_dev_ra = (!d.x_size/ra_range)
        pix_per_dev_dec = (!d.y_size/dec_range)
      
        IF pix_per_dev_ra LT pix_per_dev_dec THEN BEGIN
           position = [.1,.1,.9,.1+.8*dec_range/ra_range]*!d.x_size
        ENDIF ELSE BEGIN
           position = [.1,.1,.1+.8*ra_range/dec_range,.9]*!d.y_size
        ENDELSE  
        foo = convert_coord(position[[0,2]],position[[1,3]],/device,/to_normal)
        position = [foo[0,0],foo[1,0],foo[0,1],foo[1,1]]
     ENDIF ELSE BEGIN
        if n_elements(position) ne 4 then position=[0.1,0.1,0.95,0.95]
     ENDELSE

     pl,[0],[0],xrange=[max_a,min_a],yrange=[min_d,max_d], $
        /nodata,xtickformat = 'pli_ra_format',ytickformat='pli_dec_format', $
        xtit='Right Ascension (J2000)', $
        ytit='Declination (J2000)' $
        ,/noclip,position=position,_extra=_extra
     overplot=1
     
  ENDIF
   
  if n_elements(backgroundcolor) eq 1 then begin
     polyfill,[max_a,min_a,min_a,max_a,max_a],[min_d,min_d,max_d,max_d,min_d],color=backgroundcolor,fill=1
  endif
     
  ;; convert nlevels, min_value, max_value to levels
  IF (overplot NE 0) THEN BEGIN
     levels = dindgen(nlevels)*(max_value-min_value)/double(nlevels -1) + min_value
  ENDIF

  ;; and put this in a contour
  contour,image[x0:x1,y0:y1],a,d, $
;;          /irregular, $
          fill=fill, $
          min_value=min_value, $
          max_value=max_value, $
          levels=levels, $
          overplot=overplot, $
          _extra=_extra

  if keyword_set(getcoords) then begin
     contour,image[x0:x1,y0:y1],a,d, $
;;          /irregular, $
             fill=fill, $
             min_value=min_value, $
             max_value=max_value, $
             levels=levels, $
             overplot=overplot, $
             path_xy=path_xy, $
             path_info=path_info, $
             /path_data_coords, $
             _extra=_extra
  endif
  
  ;; redraw the box again to make sure nothing of the axes is lost
  ;; behind the image
  IF NOT keyword_set(nobox) THEN BEGIN
     ;; make idl believe there is a new plot to be started without
     ;; erasing the old one
     !p.multi[0]=!p.multi[0]+1
     pl,[0],[0],xrange=[max_a,min_a],yrange=[min_d,max_d], $
        /nodata, $
        xtickformat='(A1)',ytickformat='(A1)',xtitle='',ytitle='', $
        /noclip,position=position,color=boxcolor,_extra=_extra
  ENDIF
END
