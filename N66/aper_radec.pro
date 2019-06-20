function aper_radec,par1,center=center,radius=radius, $
                    noconvert=noconvert,header=header,_extra=_extra
  
  case size(par1,/tname) of
     'STRING': begin
        img=readfits(par1,hdr,_extra=_extra)
     end
     else: begin
        if size(par1,/n_dimensions) ne 2 then begin
           message,/info,'Input is not a filename and not a 2d array'
           return,!values.d_nan
        endif
        if n_elements(header) eq 0 then  begin
           message,/info,'Image supplied but no header'
           return,!values.d_nan
        endif
        img=double(par1)
        hdr=header
     end
  endcase

  ra = center[0]
  dec = center[1]
  nradius = n_elements(radius)
  dummy_array=make_array(nradius,val=1d0)
  
  adxy,hdr,ra,dec,x,y
  adxy,hdr,reform(ra*dummy_array),reform(dec-radius/3600d0),foo,ybottom
  adxy,hdr,reform(ra*dummy_array),reform(dec+radius/3600d0),foo,ytop

  radius_pixels = abs(ytop-ybottom)/2d0
  max_radius_pixels = max(radius_pixels)

  aper,img,x,y,meas,measerr,sky,skyerr,radius_pixels,radius_pixels, $
       setskyval=0d0,/nan,/flux,/exact,/silent,_extra=_extra
  
  if not keyword_set(noconvert) then begin
     ;; here meas is in summed MJy/sr/pixel to go Jy
     ;; we multiply by 10^6 and divide by the area of 1 pixel in steradia
     getrot,hdr,to,cdelt
     pixel_area_sr = abs(cdelt[0]*cdelt[1]) * (!pi/180.)^2
     meas = meas*1d6*pixel_area_sr
     message,/info,'Units converted from MJy/sr to Jy'
  endif

  return,meas
  
end
