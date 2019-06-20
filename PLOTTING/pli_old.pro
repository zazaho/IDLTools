FUNCTION pli_ra_format,axis,index,ra
;  return,string(ra,format='(f6.1)')
  h = floor(ra/15.)
  m = floor((ra/15.-h)*60.)
  s = round((ra/15.-h-m/60.)*3600.)
  IF s EQ 60 THEN BEGIN
      s=0
      m=m+1
  ENDIF 
  str_h = n2s(h)+'!Uh!N'
  str_m = n2s(m)+'!Um!N'
  IF s NE 0 THEN str_s=n2s(s)+'!Us!N' ELSE str_s=''
  return, str_h+str_m+str_s
END

FUNCTION pli_dec_format,axis,index,decIn
;  return,string(decIn,format='(f6.1)')
  dec=decIn
  sign = dec GE 0
  dec = abs(dec)
  d = floor(dec)
  m = floor((dec-d)*60.)
  s = round((dec-d-m/60.)*3600.)
  IF s EQ 60 THEN BEGIN
      s=0
      m=m+1
  ENDIF 
  str_d = (['-','+'])[sign]+n2s(d)+'!Uo!N'
  str_m = n2s(m)+"'"
  IF s NE 0 THEN str_s=n2s(s)+'"' ELSE str_s=''
  return, str_d+str_m+str_s
END

;; Utility to plot an image in RA, DEC coordinates. Optionally it allows to apply distortions
;; like shift and rotation to be applied before plotting.
;; It requires an image and a header which has the following keywords:
PRO pli,image,header,oplot=oplot,shift=shift,sources=sources,coords=coords, $
        keepbox=keepbox,number=number,drot=drot,autoscale=autoscale, $
        channel=channel,nlevels=nlevels,levels=levels, $
        xrange=xrange,yrange=yrange, $
        _extra=_extra

  on_error,2

  default,oplot,0
  default,shift,[0d0,0d0] ;; requested shift (ra,dec) [arcsec]
  default,keepbox,0
  shift_degr = shift/3600.
  default,drot,0d0 ;; an extra rotation term
  default,autoscale,1
  default,channel,''
  default,nlevels,25
  default,xrange,[0,0]
  default,yrange,[0,0]

  ;; remember the device settings that might be changed internally
  if !d.name eq 'X' then begin
     device,get_graphics_function=old_graphics_function
     tvlct,old_r,old_g,old_b,/get
  endif

;; now the 'hard' work convert all pixels to celestial coordinates
  nx  = n_elements(image[*,0])
  ny  = n_elements(image[0,*])

  ;; the x-values for each pixel
  x = dindgen(nx)#make_array(ny,value=1)
  y = make_array(nx,value=1)#dindgen(ny)

  XYAD, header, x, y, RA, DEC

  ;; also get the dec of the center of the image
  XYAD, header, x[nx/2], y[ny/2], CRA, CDEC
  
  IF keepbox THEN BEGIN
      min_ra = min(!x.crange,max=max_ra)
      min_dec = min(!y.crange,max=max_dec)
      position = !p.clip[0:3]
   ENDIF ELSE BEGIN 
      IF total(xrange EQ [0,0]) NE 2 THEN BEGIN
         min_ra = min(xrange,max=max_ra)
      ENDIF ELSE BEGIN
         min_ra = min(ra,max=max_ra)
      ENDELSE
      IF total(yrange EQ [0,0]) NE 2 THEN BEGIN
         min_dec = min(yrange,max=max_dec)
      ENDIF ELSE BEGIN
         min_dec = min(dec,max=max_dec)
      ENDELSE
      
      ra_range = (max_ra-min_ra)*cos(cdec*!dpi/180.) ;; in arcsec not sec
      dec_range = max_dec-min_dec
      
      ;; make sure we have a plot window to plot in
      IF (!d.name EQ 'X') AND (!d.window EQ -1) THEN window
      
      pix_per_dev_ra = (!d.x_size/ra_range)
      pix_per_dev_dec = (!d.y_size/dec_range)
      
      IF pix_per_dev_ra LT pix_per_dev_dec THEN BEGIN
         position = [.15,.1,.9,.1+.75*dec_range/ra_range]*!d.x_size
      ENDIF ELSE BEGIN
         position = [.15,.1,.1+.75*ra_range/dec_range,.9]*!d.y_size
      ENDELSE  
   ENDELSE
   
   IF oplot EQ 0 THEN BEGIN
      pl,[0],[0],xrange=[max_ra,min_ra],yrange=[min_dec,max_dec], $
         /nodata,xtickformat = 'pli_ra_format',ytickformat='pli_dec_format', $
         position=position,/white, $
         xtit='Right Ascension (J2000)', $
         ytit='Declination (J2000)',/device,/noclip,_extra=_extra
   ENDIF

   ;; Use only the part which is in the viewing window
   plottable_pixels  = where( $
                        ( ra GE min_ra) AND $
                        ( ra LE max_ra) AND $
                        ( dec GE min_dec) AND $
                        ( dec LE max_dec) $
                             )
   
   xmin = min(plottable_pixels MOD nx,max=xmax)
   ymin = min(plottable_pixels  /  nx,max=ymax)

   if !d.name eq 'X' then begin
      ;; do we want to have a rgb plot with overlays?
      IF channel NE '' THEN BEGIN
         ;; load our own color-table which is RGB colors
         tvlct,r,g,b,/get
         ;; erase the values between 1 and 255
         r[1:255] = 0
         g[1:255] = 0
         b[1:255] = 0
         ;; now fill each with an increment to 256
         r[  1: 85]    = indgen(85)*3 
         g[ 86:170]    = indgen(85)*3 
         b[171:255]    = indgen(85)*3 
         ;; load the modified table
         tvlct,r,g,b
         
         ;; now select with part of the color table to use
         CASE strupcase(strmid(strcompress(channel,/remove_all),0,1)) OF 
            'R': BEGIN
               ccolors =   1 + round(indgen(nlevels)*85./(nlevels - 1))
            END
            'G': BEGIN
               ccolors =  86 + round(indgen(nlevels)*85./(nlevels - 1))
            END
            ELSE: BEGIN
               ccolors = 171 + round(indgen(nlevels)*85./(nlevels - 1))
            END
         ENDCASE
         ;; do an or with the existing image to get the composite image
         device,set_graphics_FUNCTION = 7
      ENDIF ELSE BEGIN
         ccolors = indgen(nlevels)*!D.TABLE_SIZE/nlevels
         device,set_graphics_FUNCTION = 3
      ENDELSE
   endif
   
   plotimage = image[xmin:xmax,ymin:ymax]
   plotra    = ra   [xmin:xmax,ymin:ymax]
   plotdec   = dec  [xmin:xmax,ymin:ymax]
   
   IF n_elements(levels) EQ 0 THEN begin
;; determine the levels to plot/fill  
      min_image = min(plotimage,max=max_image)
      IF keyword_set(autoscale) THEN BEGIN
         ;; try to do an autoscaling using the median value of the image.
         ;; This works best on real images with lineair counts
         ;; However, with this hack we try to accomodate more general images like log'ed ones
         median_image = median(plotimage)
         ;; are we in a "normal" image 
         IF (median_image GT 0) AND (median_image GT abs(min_image/3.)) THEN BEGIN
           levels = dindgen(nlevels)/1d1*median_image
        ENDIF ELSE BEGIN
           levels = min_image+dindgen(nlevels)/1d1*( (max_image-min_image) < 5.0*(median_image-min_image))
        ENDELSE
     ENDIF ELSE BEGIN
        levels = min_image+dindgen(nlevels)/double(nlevels-1)*(max_image-min_image)
     ENDELSE
  ENDIF ELSE BEGIN
     nlevels = n_elements(levels)
  ENDELSE

  contour,plotimage,plotra,plotdec,nlevels=nlevels,levels=levels, $
          /fill,c_colors=ccolors,/overplot, $
          xrange=xrange,yrange=yrange, $
          _extra=_extra

  if !d.name eq 'X' then begin
     ;; reset the device settings that might have changed
     device,set_graphics_function=old_graphics_function
     tvlct,old_r,old_g,old_b
  endif
  
  IF n_elements(sources) NE 0 THEN BEGIN
     m_src = collapse(sources[0,*]) - ((nx-1)/2.)
     n_src = collapse(sources[1,*]) - ((ny-1)/2.)
     dec_src= m_pix_step*m_src*sin(angle)+n_pix_step*n_src*cos(angle)+cdec+shift_degr[1]
     ra_src = -1d0/cos(dec*!dpi/180.)*(m_pix_step*m_src*cos(angle)-n_pix_step*n_src*sin(angle))+cra+shift_degr[0]
     coords = transpose([[ra_src],[dec_src]])
     oplot,ps=5,ra_src,dec_src
     IF keyword_set(number) THEN BEGIN
        xyouts,ra_src,dec_src,align=1.2,n2s(indgen(n_elements(sources)))
     ENDIF
  ENDIF ELSE BEGIN
     IF n_elements(coords) NE 0 THEN BEGIN
        dec_src = collapse(coords[1,*])
        ra_src  = collapse(coords[0,*])
        
        ;; position wrt the center OF the image in degree
        delt_dec = (dec_src-cdec-shift_degr[1])
        delt_ra  = (ra_src -cra -shift_degr[0])*(cos(dec*!dpi/180.))
        
        n_src = (delt_ra*sin(angle) + $
                 delt_dec*cos(angle))/n_pix_step + (ny-1)/2.
        m_src = (-delt_ra*cos(angle) + $
                 delt_dec*sin(angle))/m_pix_step + (nx-1)/2.
        
        sources = transpose([[m_src],[n_src]])
        oplot,ps=5,ra_src,dec_src
        IF keyword_set(number) THEN BEGIN
           xyouts,ra_src,dec_src,align=1.2,n2s(indgen(n_elements(sources)))
        ENDIF
     ENDIF
  ENDELSE 
END
