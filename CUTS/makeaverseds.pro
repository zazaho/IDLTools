;; this is a routine which allows to take a spectral cube and make
;; average seds. The averages are calculated based on a criterionmap
;; and binned in the criterion space.
;;
;;Typically the use is to seperate the SEDS based on an LIR map or a
;;temperature map.
FUNCTION makeaverseds,cube,waves, $ ;; basic data
                      criterionmap, $
                      nbins=nbins, $
                      binmode=binmode, $
                      minbin=minbin, $
                      maxbin=maxbin, $
                      weightmap=weightmap, $
                      normalised=normalised, $
                      minfraction=minfraction, $
                      bins=bins

  if (n_elements(cube) eq 0) or (n_elements(waves) eq 0) or (n_elements(criterionmap) eq 0) then begin
     message,/info,'need to give a spectral cube the wavelengths and the criterionmap'
     return,-1
  endif

  default,nbins,10
  default,binmode,'log'

  idx = where(finite(criterionmap) and (criterionmap gt 0d0))
  mincrit = min(criterionmap[idx],max=maxcrit)
  
  default,minbin,(0.99999*mincrit)
  default,maxbin,(1.00001*maxcrit)

  nx = n_elements(cube[*,0,0])
  ny = n_elements(cube[0,*,0])
  nw = n_elements(cube[0,0,*])

  if n_elements(waves) ne nw then begin
     message,/info,'the number of wavelengths given is not the same as the 3rd axis of the cube'
     return,-1
  endif

  if (n_elements(criterionmap[*,0]) ne nx) or (n_elements(criterionmap[0,*]) ne ny) then begin
     message,/info,'criterionmap does not have the same dimesions as the cube slices'
     return,-1
  endif

  ;; construct a map that serves to estimate the missing flux in the
  ;; NAN pixels 
  default,weightmap,make_array(nx,ny,value=1d0)

  if (n_elements(weightmap[*,0]) ne nx) or (n_elements(weightmap[0,*]) ne ny) then begin
     message,/info,'weightmap does not have the same dimensions as the cube slices'
     return,-1
  endif

  ;; fraction of pixels required at a certain wavelength for the value
  ;; to be iinclude in the average SED
  default,minfraction,0.5

  ;; construct the binning limits
  if strlowcase(binmode) eq 'log' then begin
     bins = minbin * 10.0^(alog10(maxbin/minbin)*dindgen(nbins+1)/nbins)
  endif else begin
     bins = minbin + (maxbin-minbin)*dindgen(nbins+1)/nbins
  endelse

;; To hold the average seds from those bins
  AverSeds = make_array(nbins,nw,value=!values.d_nan)

  ;; reform the cube and the maps to 2d, 1d resp to be easier to
  ;; handle
  reformattedcube         = reform(cube,nx*ny,nw)
  reformattedcriterionmap = reform(criterionmap,nx*ny)
  reformattedweightmap    = reform(weightmap,nx*ny)
  
  ;; loop over each bin
  FOR i=0,nbins-1 DO BEGIN
     idx = where( (reformattedcriterionmap GE bins[i]) AND (reformattedcriterionmap LT bins[i+1]),cnt)
     IF cnt NE 0 THEN BEGIN
        for w=0,nw-1 do begin
           pixels = reformattedcube[idx,w]
           goodpixidx = where(finite(pixels),goodpixcnt)
           case 1 of
              goodpixcnt eq cnt: begin
                 averseds[i,w] = mean(pixels)
              end
              double(goodpixcnt)/double(cnt) gt minfraction: begin
                 averseds[i,w] = mean(pixels[goodpixidx]) * total(reformattedweightmap[idx])/total(reformattedweightmap[idx[goodpixidx]])
              end
              else: begin
                 ;; dont accept too little data
              end
           endcase
        endfor
     ENDIF
  ENDFOR

  ;; also make normalise versions of the flux averaged SEDs:
  normalised = AverSeds
  FOR i=0,nbins-1 DO BEGIN
     fluxes = reform(AverSeds[i,*])
     if total(finite(fluxes)) ne 0 then begin
        int= sh_integrate(transpose([[waves],[fluxes]]),/quiet,/noplot)
        normalised[i,*] = AverSeds[i,*]/int
     endif
  endfor
  
  return,AverSeds
END
