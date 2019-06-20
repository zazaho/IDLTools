;(SH Jan 27 1999)
;#> sh_bbfit.dc1
; Synopsis     fit = sh_bbfit( AAR 'or' wave,flux 
;                           [,temp]
;                           [,power]
;                           [,outwave]
;                           [,outwave=outwave]
;                           [,xrange=xrange]
;                           [,weight_in=weight_in]
;                           [,temp_in=temp_in]
;                           [,power_in=power_in]
;                           [,scale_in=scale_in]
;                           [,niter=niter]
;                           [,/flambda]
;                           [,/fixpower]
;                           [,relerr=relerr]
;                            )
; Arguments    name     I/O  type     description
; -------------------------------------------------------------
;              fit       O   fltarr   fitted blackbody
;              AAR       I   struct   input AAR
;              wave      I   fltarr   input wavelength array
;              flux      I   fltarr   input flux array (in Jy)
;              temp      O   float    fitted blackbody temperature
;              outwave  I   fltarr   Output wavelength array
;              weight    I   fltarr   array of same lengths as 
;                                     wave containing weights
;              temp_in   I   float    initial estimate of temperature
;                                     [default: 100]
;              power_in  I   float    estimate if the powerindex to fit
;
; Returns      Blackbody fit and temperature,power index
;
; Category     UTIL
;
; Filename     sh_bbfit.pro
;
; Author       Fred Lahuis
;              Sacha Hony added emmisivity index
;                         amoeba fitting 
; Version      0.0
;
; History      0.0  20-11-1997 FL  document created
;              1.0  04-12-1997 FL  inserted in IA
;              2.0  SH Mar 24 1999 added power index
;              3.0  SH Jul  7 1999 changed to amoeba fitting
;#<

FUNCTION mbb_funct,wave,par
  
  COMMON cm_mbb_func,doflambda
  par = double(par)
  
;(SH Jan 27 1999)
; we hebben dus :
; bb = 2h*nu^3/c^2 / (exp(h*nu/(k*T))-1)
; h = 6.6262e-34 Js
; nu in Hz
; c = 2.99792458e8 m/s
; k = 1.3807e-23 J/K
; T temperature in K
; in J/HZ/m^2/s
; om schrijven naar lambda levert:
; bb = 2*h*c/(w^3) / (exp(h*c/(w*k*T))-1)
; w = wavelength in m
; om schrijven naar Jansky-Micron levert:
; bb = 1e26*1e18*2*h*c/(l^3) / (exp(h*c/(1e-6*l*k*T))-1)
; l = wavelength in micron
; bb = a1/(l^3)/(exp(a2/(l*T))-1)
; a1 = 1e26*1e18*2*h*c = 3.97296e19
; a2 = h*c/1e-6/k = 1.43875e4
; dus de modifidied bb (mbb) heeft de volgende vorm:
; 
; mbb = a1*l^(p-3)/(exp(a2/(l*T))-1)
; en de algemene vorm geldt:
; fit = F*mbb(p,T)
; de afgeleiden leveren
; dF = mbb(p,T)
; dT = F*a1*(l^(p-3))*a2^2/l^2/T^3/(exp(a2/(l*T))-1)^2*exp(a2/(l*T) = 
; = F*mbb(p,T)*(a2)/(l*T^2)*exp(a2/(l*T)/(exp(a2/(l*T))-1)
; dp = F*mbb*alog(l)
  
; speed of light in micron/second
  c= 2.99792458d14
  l = wave
  T = par(0)
  F = par(1)
  p = par(2)
 
  a1 = 3.97296d19
  a2 = 1.43875d4
    
  IF (doflambda) THEN fct = 1d-26*c/l/l ELSE fct = 1d0
  
  noemer  = exp(a2/(l*T)) - 1d0
  mbbflux = fct * F*a1*(l^(p-3d0)) / noemer
  
  return,mbbflux
END 

FUNCTION chi2_sh_bbfit,par
  COMMON cm_chi2_sh_bbfit,w,f,s,pfixed
  IF (pfixed eq 666) THEN BEGIN 
    npar = 3
  ENDIF ELSE BEGIN 
    npar = 2
  ENDELSE
  ffit = mbb_funct(w,[par,pfixed])
;  Reduced chi squared
;  TOTAL(((y-yfit)/s)^2)/(n_elements(y)-nparam)
  return,TOTAL(((f-ffit)/s)^2)/(n_elements(f)-npar)
END

FUNCTION sh_bbfit, $
                   par1, $
                   par2, $
                   par3, $
                   par4, $
                   par5, $
                   outwave = outwave, $
                   xrange=xrange, $
                   weight_in=weight_in, $
                   temp_in=temp_in, $
                   power_in=power_in, $
                   scale_in = scale_in, $
                   flambda = flambda, $
                   fixpower=fixpower, $
                   relerr = relerr
  
  COMMON cm_mbb_func,doflambda
  COMMON cm_chi2_sh_bbfit,w,f,s,pfixed

  IF ( is_aar(par1) ) THEN isaar = 1 ELSE isaar = 0
  nparams = n_params()

  IF ( isaar ) THEN BEGIN 
    w_in = par1.data.wave
    f_in = par1.data.flux
    IF ( nparams eq 4 ) THEN outwave = par4
  ENDIF ELSE BEGIN 
    w_in = par1
    f_in = par2
    IF ( nparams eq 5 ) THEN outwave = par5
  ENDELSE
  
  w = w_in
  f = f_in

  nwave = n_elements(w)
  IF ( n_elements(weight_in) ne nwave ) THEN BEGIN 
    IF n_elements(weight_in) ne 0 THEN BEGIN 
      weight = replicate(weight_in(0),nwave)
    ENDIF ELSE BEGIN 
      IF (n_elements(relerr) ne 0) THEN BEGIN 
        weight = 1d0/(f*relerr(0))
        ENDIF ELSE BEGIN 
          weight = replicate(1d0,nwave)
        ENDELSE
    ENDELSE
  ENDIF ELSE BEGIN 
    weight = weight_in
  ENDELSE
  
  IF ( n_elements(xrange) eq 2 ) THEN BEGIN 
    index = where(w ge min(xrange) and w le max(xrange), count)
    IF ( count gt 0 ) THEN BEGIN 
      w   = w_in(index)
      f   = f_in(index)
      weight = weight(index)
    ENDIF  
  ENDIF
 
  IF ( n_elements(temp_in) eq 0 ) THEN temp = 5d2 ELSE temp = temp_in(0)
  IF ( n_elements(power_in) eq 0 ) THEN pwr = 0d0 ELSE pwr = power_in(0)
  IF ( n_elements(scale_in) eq 0 ) THEN scale = 1 ELSE scale = scale_in(0)
  
  par = double([temp,scale,pwr])
  
  doflambda = 0
  pfixed = 666
  
  IF keyword_set(fixpower) THEN BEGIN 
    pfixed = pwr
    par =  double([temp,scale])
    scl = [1d3,1d-9]
  ENDIF ELSE BEGIN 
    pfixed = 666
    par = double([temp,scale,pwr]) 
    scl = [1d3,1d-9,2d0]
  ENDELSE
      
  IF keyword_set(flambda) THEN BEGIN 
    doflambda = 1
    f = f/w/w*2.9979d-12
  ENDIF
  
; 1/weight = stdev dan geldt dus voor andere eenheden iets in de trand
; van stdev_nieuweeenheid = stdev*d/df dus
  IF (doflambda) THEN weight = w*w/2.9979256d-12*weight
  
;
; initialise scaling parameter
;
  fit = mbb_funct(w,[par,pfixed])
  par(1) = total(f/fit)/nwave

  s = 1d0/weight
  par = amoeba(1d-6,FUNCTION_NAME="chi2_sh_bbfit",P0=par,scale=scl)
  IF keyword_set(fixpower) THEN par = [par,pfixed]
  
  print,'BB TEMPERATURE: ',strtrim(par(0),2)
;;  print,'BB SCALING: ',strtrim(par(1),2)
  print,'BB POWER: ',strtrim(par(2),2)
  print,'-----------------------------'

;  scale = (par(1)/factor)/4.2545E10	; steradian == 4.2545E10 arcsec^2
; Have we selected some smaller part of the total aar ??
;; Ouput: we want the following output
;; either the fluxes at the given waves (eg no aar input)
;; or an aar with the right fluxes
;; this is called result
;; optional output:
;; par2-3/par3-4 temp,power
;; par4/par5 (outwave) a different aar with the same parameters
  
  doflambda = 0
  
  result = mbb_funct(w_in,par)
  
  IF ( isaar ) THEN BEGIN 
    par2 = par(0)
    par3 = par(2)
    fit = par1
    fit.data.flux = result
    result = fit
  ENDIF ELSE BEGIN 
    par3 = par(0)
    par4 = par(2)
  ENDELSE

  IF ( n_elements(outwave) gt 0 ) THEN BEGIN 
    IF is_aar(outwave) THEN BEGIN 
      result = outwave
      result.data.flux = mbb_funct(result.data.wave,par)
    ENDIF ELSE BEGIN 
      result = mbb_funct(outwave,par)
    ENDELSE
  ENDIF
  
  return, result

END
