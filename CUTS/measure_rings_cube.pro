pro measure_rings_cube,cube,inxc,inyc,radii,nbin=nbin, $
                       mean=mean,total=total,stdev=stdev, $
                       weightmap=weightmap, $
                       minfraction=minfraction, $
                       subtract=subtract,lirimage=lirimage
;+
; NAME:
;      MEASURE_RING_CUBE
; PURPOSE:
;      Compute the mean and stdev inside concentric rings around a given center
;      of each cube in a cube

; CALLING SEQUENCE:
;     MEASURE_RING_CUBE, cube, xc, yc, radii,nbin=nbin,mean=mean,total=total,stdev=stdev
; INPUTS:
;     CUBE -  input cube array
;     XC     - x coordinate 
;     YC     - y coordinate
;     RADII  - vector of radii (in pixel) of the border of the rings
;
; EXAMPLE:
;-
                                ;(SH Mar 16 2009)
                                ;added check on finiteness of pixels
  On_error,2
  
  if N_params() LT 4 then begin  ;Enough parameters supplied?
     doc_library,'measure_rings_cube'
     return
  endif 
  
  s = size(cube)
  if ( s[0] NE 3 ) then message,'ERROR - Cube array (first parameter) must be 3 dimensional'
  
  nx = s[1] & ny = s[2] & nz = s[3] ;Number of columns and rows and planes in cube array
  
  if ( N_elements(radii) LT 1 ) then begin ;Read in aperture sizes?
     doc_library,'measure_rings_cube'
     return
  endif
  
 ;; prepend a 0 to the radii
  rads = [0d0,radii]
  nrads = n_elements(rads)
  
  default,inxc,0L
  default,inyc,0L

  default,nbin,10L

  xc = long(inxc)
  yc = long(inyc)
  nx = long(nx)
  ny = long(ny)
  nbin = long(nbin)

  default,weightmap,make_array(nx,ny,value=1d0)

  if (n_elements(weightmap[*,0]) ne nx) or (n_elements(weightmap[0,*]) ne ny) then begin
     message,/info,'weightmap does not have the same dimensions as the cube slices'
     return
  endif

  ;; fraction of pixels required at a certain wavelength for the value
  ;; to be iinclude in the average SED
  default,minfraction,0.5

  ;; make coordinate images
  idx = lindgen(nx*nbin,ny*nbin)
  x = double(idx mod (nx*nbin))/nbin - xc
  y = double(idx  /  (nx*nbin))/nbin - yc
  r = sqrt(x^2+y^2)

  ;; to hold the resulting values
  stdev = make_array(nrads-1,nz,value=0d0)
  mean =  stdev
  total =  stdev

;; now  just go ahead and determine which pixels are in the annuli.
  for j=0,nz-1 do begin
     image = cube[*,*,j]

     ;; make a finer gridded image
     fineimage = rebin(image,nx*nbin,ny*nbin)
     
     IF keyword_set(subtract) THEN BEGIN
        idx = where((r GT rads[nrads-1]) AND finite(fineimage),cnt)
        IF cnt NE 0 THEN BEGIN
           ;; try to find the peak of the histogram automatically
           min = min(fineimage[idx],max=max)
           ;; bin with on avererage 10 pixel per bin
           cnt10 = cnt/10
           yhist = histogram(fineimage[idx],min=min,max=max,nbins=cnt10)
           xhist = min+(max-min)*dindgen(cnt10)/(cnt10-1d0)
           
           ;; find the peak of the histogram
           ypeak = max(yhist,xpeak)
           ;; convert to the pixel units
           xpeak = min+(max-min)*xpeak/(cnt10-1d0)
           fineimage = fineimage - xpeak
        ENDIF
     ENDIF
     
     FOR i=0,nrads-2 DO BEGIN
        idx = where((r GE rads[i]) AND (r LT rads[i+1]),cnt)
        IF cnt NE 0 THEN BEGIN
           pixels_in_annulus = fineimage[idx]
           idx = where(finite(pixel_in_annulus),cnt,complement=nidx,ncomplement=ncnt)
           
           mom = moment()
           mean[i,j] = mom[0]
           total[i,j] = mean[i]*cnt/nbin/nbin
           stdev[i,j] = sqrt(mom[1])
        ENDIF
     ENDFOR
  ENDFOR
end



;
;
;
;
;  ;; loop over each bin
;  FOR i=0,nbins-1 DO BEGIN
;     idx = where( (reformattedcriterionmap GE bins[i]) AND (reformattedcriterionmap LT bins[i+1]),cnt)
;     IF cnt NE 0 THEN BEGIN
;        for w=0,nw-1 do begin
;           pixels = reformattedcube[idx,w]
;           goodpixidx = where(finite(pixels),goodpixcnt)
;           case 1 of
;              goodpixcnt eq cnt: begin
;                 averseds[i,w] = mean(pixels)
;              end
;              double(goodpixcnt)/double(cnt) gt minfraction: begin
;                 averseds[i,w] = mean(pixels[goodpixidx]) * total(reformattedweightmap[idx])/total(reformattedweightmap[idx[goodpixidx]])
;              end
;              else: begin
;                 ;; dont accept too little data
;              end
;           endcase
;        endfor
;     ENDIF
;  ENDFOR
;
;  ;; also make normalise versions of the flux averaged SEDs:
;  normalised = AverSeds
;  FOR i=0,nbins-1 DO BEGIN
;     fluxes = reform(AverSeds[i,*])
;     if total(finite(fluxes)) ne 0 then begin
;        int= sh_integrate(transpose([[waves],[fluxes]]),/quiet,/noplot)
;        normalised[i,*] = AverSeds[i,*]/int
;     endif
;  endfor
;  
;  return
;END
