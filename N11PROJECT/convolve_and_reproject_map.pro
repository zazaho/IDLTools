;; routine to take an image and convolve it with a psf. 
;; caveat: should only be used within conv_cube because there is
;; little proper checking on keywords etc
function convolve_and_reproject_map,imgname,convolutionkernelname,outheader=outheader

;; The main problem is taking care of the pixels sizes. The real
;; problem is the regridding. The best would be to have integer
;; regridding. For now since we are inside the conv_cube routine and
;; we know that the kernel is finely sampled, resample the img to the
;; kernel resolution. This is more calcuation intensive but safer. The
;; best is perhaps to do a hastrom on the same header with a pixel
;; size corresponding to the kernel

  ;; determine pixel size of img from himg

  img=readfits(imgname,himg)
  
  if sxpar(himg,'NAXIS') eq 3 then begin
     ;; rmove the mock third dimension
     img=reform(img)
     sxaddpar,himg,'NAXIS',2
     sxdelpar,himg,'NAXIS3'
     sxdelpar,himg,'CDELT3'
     sxdelpar,himg,'CRPIX3'
     sxdelpar,himg,'CRVAL3'
     sxdelpar,himg,'CUNIT3'
     sxdelpar,himg,'CTYPE3'
  endif
     
  if n_elements(outheader) eq 0 then outheader=himg

  convolutionkernel=readfits(convolutionkernelname,hconvolutionkernel)

  ;; first store the pix dimensions
  getrot,himg,rot,himgcdelt
  origcdelt1 = himgcdelt[0]*3600d0 ;; arcsec
  origcdelt2 = himgcdelt[1]*3600d0 ;; arcsec

  ;; other important keywords
  orignaxis1 = sxpar(himg,'naxis1')
  orignaxis2 = sxpar(himg,'naxis2')
  origcrpix1 = sxpar(himg,'crpix1')
  origcrpix2 = sxpar(himg,'crpix2')
  
  getrot,hconvolutionkernel,rot,hconvolutionkernelcdelt
  
  if abs(hconvolutionkernelcdelt[0]) ne abs(hconvolutionkernelcdelt[1]) then begin
     message,/info,'convolution kernel pixels are not square'
     message,/info,'cannot proceed'
     return,!values.d_nan
  endif else begin
     convolutionkernelpixelsize = abs(hconvolutionkernelcdelt[0])*3600d0 ;; arcsec
  endelse

  ;; extra border to be sure not to lose pixels
  extendborderfactor = 1.1d0
  imagescalefactor = ceil(abs(origcdelt1)/convolutionkernelpixelsize*extendborderfactor)
  pixelscalefactor = abs(origcdelt1)/convolutionkernelpixelsize

  htargetimg = himg

  sxaddpar,htargetimg,'CDELT1',origcdelt1/abs(origcdelt1)*convolutionkernelpixelsize/3600d0
  sxaddpar,htargetimg,'CDELT2',origcdelt2/abs(origcdelt2)*convolutionkernelpixelsize/3600d0
  sxaddpar,htargetimg,'NAXIS1',orignaxis1*imagescalefactor
  sxaddpar,htargetimg,'NAXIS2',orignaxis2*imagescalefactor

  ;; clean to be sure not to confuse hswarp
  sxdelpar,htargetimg,'CD1_1'
  sxdelpar,htargetimg,'CD1_2'
  sxdelpar,htargetimg,'CD2_1'
  sxdelpar,htargetimg,'CD2_2'
  
;; put the reference pixel in the middle of the original map in ra,dec and give
;; it half the new naxis1,naxis2 values to make sure the map extends
;; beyond the original map everywhere

  xyad,himg,orignaxis1/2d0,orignaxis2/2d0,ra,de
  
  sxaddpar,htargetimg,'CRVAL1',ra
  sxaddpar,htargetimg,'CRVAL2',de

  sxaddpar,htargetimg,'CRPIX1',orignaxis1*imagescalefactor/2.
  sxaddpar,htargetimg,'CRPIX2',orignaxis2*imagescalefactor/2.

  ;; resample on the fine grid using hastrom
  hswarp,img,himg,fineimg,hfineimg,htargetimg,/fixheader,/keepedge

  ;; (SH Oct 17 2011) (SH Apr 27 2010)
  ;; have verified that the reprojection is done correctly
  
  ; remove NANs
  NANidx = where(finite(fineimg) ne 1,NANcnt)
  if NANcnt ne 0 then fineimg[NANidx] = 0d0

  convolvedfineimg = convolve_for_conv_cube(fineimg,convolutionkernel)

  ;; resample to the original grid
  hswarp,convolvedfineimg,hfineimg,convolvedimg,hconvolvedimg,outheader,/fixheader,/keepedge

  return,convolvedimg
end
