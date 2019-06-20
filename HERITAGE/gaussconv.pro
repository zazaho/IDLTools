;+
; NAME:
; gaussconv
;
; PURPOSE:
; convolve an image with a gaussian kernel
;
; CATEGORY:
; data treatment
;
; CALLING SEQUENCE:
; convolved_image = gaussconv(image,fwhm)
;
; INPUTS:
; image: 2d array of the input image
; fwhm: the full-width-at-half-maximum in arcsec of the kernel to use
;
; KEYWORD PARAMETERS:
; header=header: header of the image to use for determining the platescale
; psf=psf: psf to use for convolving
; threshold: cut-off value (peak to wing) to determine the size of the
;            kernel image
; platescale=platescale: length of on side of a pixel in arcsec
; /noclean: if set do not remove the infinites from the input image
; :help: if set show this help
; _extra=_extra: extra keywords to pass to convolve
;
; OUTPUTS:
; convolved image
$
; EXAMPLE:
; convolved_image = gaussconv(image,fwhm)
;
;
; MODIFICATION HISTORY:
; (SH Dec 15 2009): initial version based on simple idl scripts
;-
function gaussconv,i, $
                   fwhm, $ ;; arcsec
                   header=header, $
                   psf=psf, $
                   threshold=threshold, $
                   platescale=platescale, $
                   noclean=noclean, $
                   help=help, $
                   _extra=_extra

  if n_params() eq 0 or keyword_set(help) then begin
     doc_library,'gaussconv'
     return,-1
  endif
  
  default,threshold,1d-3
  
;; no psf given so determine the psf from the given params
  if n_elements(psf) eq 0 then begin
     if n_params() ne 2 then begin
        message,/info,'no psf given and no fwhm specified'
        doc_library,'gaussconv'
        return,-1
     endif
     if n_elements(platescale) eq 0 then begin
        if n_elements(header) eq 0 then begin
           message,/info,'no platescale given and no header specified so cannot derive the platescale'
           doc_library,'gaussconv'
           return,-1
        endif
        ;; this assume square pixels 
        cdelt1 = sxpar(header,'cdelt1',count=count)
        if count ne 0 then begin
           platescale=abs(cdelt1*3600d0) ;; in arcsec
        endif else begin
           message,/info,'no platescale given and no header does not contain a cdelt keyword'
           doc_library,'gaussconv'
           return,-1
        endelse
     endif

     ;; no construct the psf based on the platescale, fwhm and
     ;; threshold values

     r_half = fwhm/platescale
     w = r_half/2.35482

     r_max = w*sqrt(-1d0*alog(threshold)*2) ;; in arcsec
     
;;;; now define the PSF centered on the middle of the image:
     Npix = 2L*ceil(r_max)+1L
     image = make_array(Npix,nPix,value=0d0)
     x = dindgen(Npix)#make_array(Npix,value=1) - Npix/2
     y = dindgen(Npix)##make_array(Npix,value=1) - Npix/2

     r = sqrt(x^2+y^2)
     PSF = exp(-1d0*r^2/(2d0*w^2))
     
     ;; normalise
     psf = psf/total(psf)
  endif else begin
     if abs(total(psf)-1d0) gt 1d-10 then begin
        message,/info,'the given PSF does not appear to be normalised'
     endif
  endelse

  if keyword_set(noclean) then begin
     ii = i
  endif else begin
     ii = finitise(i)
  endelse
  
 cii = convolve(ii,psf,_extra=_extra)

 return,cii
end
