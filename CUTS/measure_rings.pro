pro measure_rings,image,inxc,inyc,radii,nbin=nbin, $
                  mean=mean,total=total,stdev=stdev, $
                  subtract=subtract,background=background
;+
; NAME:
;      MEASURE_RING
; PURPOSE:
;      Compute the mean and stdev inside concentric rings around a given center
;      of an image

; CALLING SEQUENCE:
;     MEASURE_RING, image, xc, yc, radii,nbin=nbin,mean=mean,total=total,stdev=stdev
; INPUTS:
;     IMAGE -  input image array
;     XC     - x coordinate 
;     YC     - y coordinate
;     RADII  - vector of radii (in pixel) of the border of the rings
;
; EXAMPLE:
;-
                                ;(SH Mar 16 2009)
                                ;added check on finiteness of pixels
;;  On_error,2
  
  if N_params() LT 4 then begin  ;Enough parameters supplied?
     doc_library,'measure_rings'
     return
  endif 

  s = size(image)
  if ( s[0] NE 2 ) then message,'ERROR - Image array (first parameter) must be 2 dimensional'
  
  ;; take the image dimensions 
  nx = s[1]
  ny = s[2]                     ;Number of columns and rows in image array
  
  if ( N_elements(radii) LT 1 ) then begin ;Read in aperture sizes?
     doc_library,'measure_rings'
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

;; now  just go ahead and determine which pixels are in the annuli.
  
  fineimage = rebin(image,nx*nbin,ny*nbin)
  
  ;; make coordinate images
  idx = lindgen(nx*nbin,ny*nbin)
  x = double(idx mod (nx*nbin))/nbin - xc
  y = double(idx  /  (nx*nbin))/nbin - yc
  
  ;; check here to see that we are not off by 0.5 or 1 pixel
  r = sqrt(x^2+y^2)
  
  ;; variables to hold the results
  stdev = make_array(nrads-1,value=0d0)
  mean =  stdev
  total =  stdev

  ;; now in case we want to subtract a contribution from the 'background'
  ;; this is the logic:
  ;; check all the pixels outside the largest radius
  ;; construct the histogram 
  IF keyword_set(subtract) THEN BEGIN
     print,'going into the background subtraction part'
     idx = where((r GT rads[nrads-1]) AND (fineimage NE 0d0),cnt)
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

  ;; now do the actual summation
  FOR i=0,nrads-2 DO BEGIN
     idx = where((r GE rads[i]) AND (r LT rads[i+1]) and (finite(fineimage) eq 1),cnt)
     IF cnt NE 0 THEN BEGIN
        mom = moment(fineimage[idx])
        mean[i] = mom[0]
        total[i] = mean[i]*cnt/nbin/nbin
        stdev[i] = sqrt(mom[1])
     ENDIF
  ENDFOR

  ;; determine a "background" as the median of all the valid pixels
  ;; outside the largest radius
  idx = where((r GT rads[nrads-1]) AND (finite(fineimage) eq 1),cnt)
  IF cnt NE 0 THEN BEGIN
     background = median(fineimage[idx])
  ENDIF
  
END
