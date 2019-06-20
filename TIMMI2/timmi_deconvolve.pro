FUNCTION timmi_deconvolve,img_in,psf_in,crit=crit,show=show,max=max, $
                          radius=radius,psf=psf,mask=mask
  
  ;; when to stop
  default,crit,5d-3
  default,show,0
  default,max,10
  default,radius,0 ;; Take a circle of radius around the peak of the psf

  psf = psf_in
  img = img_in
  ;; First we need to make the psf proper
  ;; Now set the base line to zero
  psf = psf-median(psf)
  ;; fit a gauss
  gpsf = gauss2dfit(psf)
  
  IF radius EQ 0 THEN BEGIN ;; Dont want radius then find peak
      ;; Normalize and throw out the data too far from the peak
      gpsf = gpsf-median(gpsf)
      gpsf = gpsf/max(gpsf)
      zero_psf = where(gpsf LT 1d-4)
  ENDIF ELSE BEGIN
      xsize = n_elements(psf_in[*,0])
      ysize = n_elements(psf_in[0,*])
      foo = lindgen(xsize,ysize)
      x = foo MOD xsize
      y = foo  /  xsize
      foo2 = max(gpsf,indmax)
      xmax = indmax MOD xsize
      ymax = indmax  /  xsize
      r = sqrt((x-xmax)^2d0+(y-ymax)^2d0)
      mask = r GT radius
      zero_psf = where(mask)
  ENDELSE 
  
  psf[zero_psf] = 0d0
  psf=psf/total(psf)
  
  ;; Now we want to deconvolve the img until the criterium is reached
  prev_deconv = img
  multipliers = 0d0
  Max_Likelihood, img, psf, deconv, FT_PSF=psf_ft
  i = 1
;;  Max_Entropy, img, psf, deconv, multipliers, FT_PSF=psf_ft
  WHILE (total(abs(deconv-prev_deconv))/total(abs(prev_deconv)) GE crit) AND (i LT max) DO BEGIN
      prev_deconv = deconv
      Max_Likelihood, img, psf, deconv, FT_PSF=psf_ft
      i=i+1
;;    Max_Entropy, img, psf, deconv, multipliers, FT_PSF=psf_ft
      IF show THEN BEGIN
          sh_tv,deconv
      ENDIF 
      print,total(abs(deconv-prev_deconv))/total(abs(prev_deconv))
  ENDWHILE
  return,deconv
END
