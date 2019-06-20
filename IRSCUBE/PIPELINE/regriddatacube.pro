pro regriddatacube,data,unc,brightmask, $
                   factor=factor, $
                   rebinneddata=rebinneddata, $
                   rebinnedunc=rebinnedunc, $
                   status=status

  default,factor,2
  
  status=0

  nx = n_elements(data.cube[*,0,0])
  ny = n_elements(data.cube[0,*,0])
  nz = n_elements(data.cube[0,0,*])

  datacube = data.cube
  mask = data.mask
  unccube = unc.cube

  ;; clean the cube to be sure we have no rubbish
  ;; when we and do a proper weighting
  
  ;; which are the points we want to use?
  ;; brightmask = 0 and mask = max(mask)
  ;; set everything else 0
  keepmask = (brightmask eq 0) and (mask eq !fulldatamaskvalue)
  
  if total(keepmask) ne 0 then begin
     ;; do this with an index of the points to be masked since 0*NaN = NaN
     keepmaskcube = rebin(keepmask,nx,ny,nz)
     bad_idx = where(keepmaskcube eq 0,badcount) 
     if badcount ne 0 then begin
        datacube[bad_idx] = 0d0
        unccube[bad_idx] = 0d0
     endif
     weightimage = keepmask
  endif else begin
     message,/info,'no valid pixels left after bright-mask and order selection'
     status=1
     return
  endelse

  if total(finite(datacube) eq 0) ne 0 then begin
     message,/info,'NaNs in data cube -- fixing'
     datacube = finitise(datacube)
  endif

  if total(finite(unccube) eq 0) ne 0 then begin
     message,/info,'NaNs in uncertainty cube -- fixing'
     unccube = finitise(unccube)
  endif

  ;; now define the regridding steps
  rebinnednx = ceil(nx/double(factor))
  rebinnedny = ceil(ny/double(factor))
  
  ;; check that we are not trying to rebin a cube that is too small
  if (rebinnednx lt 2) or (rebinnedny lt 2) then begin
     message,/info,'Rebinning would yield a map with dimension <2 -- bailing out'
     status=1
     return
  endif

  patchednx = rebinnednx*factor
  patchedny = rebinnedny*factor

  patcheddatacube = make_array(patchednx,patchedny,nz,value=0d0)
  patcheddatacube[0:nx-1,0:ny-1,*] = datacube

  patchedunccube = make_array(patchednx,patchedny,nz,value=0d0)
  patchedunccube[0:nx-1,0:ny-1,*] = unccube

  patchedweightimage = make_array(patchednx,patchedny)
  patchedweightimage[0:nx-1,0:ny-1,*] = weightimage

  ;; mean of the pixels * N = total
  rebinneddatacube = rebin(patcheddatacube,rebinnednx,rebinnedny,nz)*factor^2  

  ;; sqrt( mean sigma^2) = sqrt(SUM sigma^2)/sqrt(N)
  ;; total sigma = sqrt(SUM sigma^2) * N = sqrt( mean sigma^2) * sqrt(N)
  rebinnedunccube  = sqrt(rebin(patchedunccube^2,rebinnednx,rebinnedny,nz)) ;;*factor 

  ;; number of pixels contribution to 
  rebinnedweightimage = rebin(patchedweightimage,rebinnednx,rebinnedny)*factor^2

  ;; which pixels have some contribution??
  rebinnedmask = (rebinnedweightimage ne 0)*!fulldatamaskvalue

  ;; now update the header info to reflect the fact that we have
  ;; divided the number of pixels
  header = data.header
  rebinnedheader = header
  
  cdelt1 = sxpar(header,'cdelt1')
  cdelt2 = sxpar(header,'cdelt2')
  crpix1 = sxpar(header,'crpix1')
  crpix2 = sxpar(header,'crpix2')
  rebinnedcdelt1 = cdelt1*factor
  rebinnedcdelt2 = cdelt2*factor


  ;; do this carefully
  ;; Since we have rebinned map the natural coordinate system is this
  ;; where the bottom,left corner of the first pixel starts at 0,0 (CS0)
  ;; this is 0.5,0.5 less than the fits definition in which the pixel
  ;; index refers to the center of the pixel and the bottom left pixel
  ;; is called 1,1 (CSF)

  ;; The position of (0,0) origin of the original fits image in our
  ;; preferred coordinate system
  ;;   (-0.5,-0.5)/factor  (CS0)
  ;; the offset of the reference position wrt to this 0,0 is
  ;; (crpix1,crpix2)/factor (CS0)
  ;; so the reference position in our preferred coords is:
  ;; (crpix1-0.5,crpix2-0.5)/factor (CS0)
  ;; to transform this to fits coordinates:
  ;; (newcrpix1,newcrpix2) (CSF) =
  ;;   ((crpix1-0.5)/factor+0.5,(crpix2-0.5)/factor+0.5)
  
  rebinnedcrpix1 = (crpix1-0.5) / factor                   + 0.5
  rebinnedcrpix2 = (crpix2-0.5) / factor                   + 0.5

  ;; check here with respect to the reference pixel
  sxaddpar,rebinnedheader,'NAXIS1',rebinnednx
  sxaddpar,rebinnedheader,'NAXIS2',rebinnedny
  sxaddpar,rebinnedheader,'CDELT1',rebinnedcdelt1
  sxaddpar,rebinnedheader,'CDELT2',rebinnedcdelt2
  sxaddpar,rebinnedheader,'CRPIX1',rebinnedcrpix1
  sxaddpar,rebinnedheader,'CRPIX2',rebinnedcrpix2

  rebinneddata = {cube:rebinneddatacube,header:rebinnedheader,hwave:data.hwave,wave:data.wave,mask:rebinnedmask}
  rebinnedunc = {cube:rebinnedunccube,header:rebinnedheader,hwave:data.hwave,wave:unc.wave,mask:rebinnedmask}
  
end
