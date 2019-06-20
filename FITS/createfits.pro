pro createfits,filename, $
               center=center, $
               size=size, $
               pixelsize=pixelsize, $
               epoch=epoch, $
               noise=noise, $
               _extra=_extra
  
  default,filename,'newfits.fits'
  if n_elements(center) ne 2 then center=[0.,0.] ; degrees
  default,size,60.                               ;;arcsec
  default,pixelsize,1.                           ;; arcsec
  default,epoch,2000.                            ;; year

  npixels = ceil(size/pixelsize)

  if n_elements(npixels) eq 1 then begin
     ;; square map
     npixels = [npixels,npixels]
  endif else begin
     npixels = npixels[0:1]
  endelse

  if keyword_set(noise) then begin
     image = randomu(seed,npixels[0],npixels[1])
  endif else begin
     image = make_array(npixels[0],npixels[1],value=0.0)
  endelse

  mkhdr,header,image,_extra=_extra

  sxaddpar,header,'CRPIX1',(npixels[0]+1)/2.,'Index of the reference pixel axis 1'
  sxaddpar,header,'CRPIX2',(npixels[1]+1)/2.,'Index of the reference pixel axis 2'

  sxaddpar,header,'CRVAL1',center[0],'WCS Ra-value of the reference pixel'
  sxaddpar,header,'CRVAL2',center[1],'WCS Dec-value of the reference pixel'

  sxaddpar,header,'CDELT1',-1.0*pixelsize/3600.,'Increment of WCS along axis 1 (degree/pixel)'
  sxaddpar,header,'CDELT2',+1.0*pixelsize/3600.,'Increment of WCS along axis 2 (degree/pixel)'

  sxaddpar,header,'NAXIS1',npixels[0],'Size of image along x-axis'
  sxaddpar,header,'NAXIS2',npixels[1],'Size of image along y-axis'

  sxaddpar,header,'CTYPE1','RA---TAN','WCS: Projection type axis 1'
  sxaddpar,header,'CTYPE2','DEC--TAN','WCS: Projection type axis 2'

  sxaddpar,header,'EPOCH',epoch,'Epoch of coordinate definition'
  

  writefits,filename,image,header,_extra=_extra

end
