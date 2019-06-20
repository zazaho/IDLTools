;; calculate a convolution kernel based on a psf and a target
;; (reference) psf. Following Gordon 2008 to define a cutoff
;; frequency.
function create_convolution_kernel, $
   psfname, $
   psfrefname, $
   fftpsfref=fftpsfref, $ ;; allows to store the previous calculation and not redo the work
   hanning=hannnig, $
   psfreffwhm=psfreffwhm, $
   nyquistthreshold=nyquistthreshold
  
  default,nyquistthreshold,2d0*sqrt(2d0)

  psf = readfits(psfname,hpsf)
  psf = psf/total(psf)
  fftpsf = fft(psf,-1)
  
  if n_elements(fftpsfref) eq 0 then begin
     psfref = readfits(psfrefname,hpsfref)
     psfref = psfref/total(psfref)
     fftpsfref = fft(psfref,-1)
  endif

  ;; check inputs
  if n_elements(fftpsf) ne n_elements(fftpsfref) then begin
     message,'psf and psfref do not have the same size',/info
     message,'regridding of the psfs is not implemented'
     ;; could also check headers to make sure pixel sizes correspond.
     ;; for now assume that the user is not an imbecile :)
  endif

  if n_elements(hanning) eq 0 then begin
     if n_elements(psfreffwhm) ne 1 then begin
        if n_elements(psfref) eq 0 then begin
           psfref = readfits(psfrefname)
        endif
        gaussfit = gauss2dfit(psfref,gaussfitparams,/tilt)
        psfreffwhm = (mean(gaussfitparams[2:3]))*2d0*sqrt(2d0*alog(2d0))
     endif
     
     nx = n_elements(fftpsfref[*,0])
     ny = n_elements(fftpsfref[0,*])
     
     ;; create a map with distances in pixels to the center of the map
     idx = lindgen(nx,ny)
     xxx = (idx mod nx) - (nx-1)/2 ;; the int rounding is correct because of the way fft is defined
     yyy = (idx  /  nx) - (ny-1)/2
     
     dist = sqrt(xxx^2+yyy^2)
     
     ;; shift the thing to agree with the way frequencies are defined
     ;; in fft
     dist = shift(dist,-(nx-1)/2,-(ny-1)/2)

     ;; so this is wrong unless the image is square
     freq = dist/(double(nx)) ;; in pixel^-1

     hanning = psf*0d0

     ;; the nyquist frequency of the lowest resolution psf 
     ;; we use 2.8 here for 2*sqrt(2). 2 for a gaussian line and
     ;; sqrt(2) because we are dealing with a 2D image not a 1d line.
     nyquist_freq = (psfreffwhm/nyquistthreshold)^(-1d0) ;; in pixel^-1
     
     ;; from sandstrom
     idx = where(freq lt nyquist_freq,cnt)
     if cnt ne 0 then begin
        ;; like in gordon 2008 except that he has 2*pi while I believe
        ;; it should be pi
        hanning[idx] = 0.5*(1+cos(!dpi*freq[idx]/nyquist_freq))
     endif else begin
        message,'no points in the fft domain that fulfill the nyquist criterion'
     endelse
  endif
  
  convolutionkernel = fft(hanning*fftpsfref/fftpsf,1)
  convolutionkernel = shift(convolutionkernel,nx/2,ny/2)
  convolutionkernel = convolutionkernel/total(convolutionkernel)

  return,convolutionkernel
end
