FUNCTION bb,wave,T,flambda=flambda
  
;(SH Jan 27 1999)
; We have in Watt/m^2/Hz :
; bb = 2h*nu^3/c^2 / (exp(h*nu/(k*T))-1)
; h = 6.6262e-34 Js
; nu in Hz
; c = 2.99792458e8 m/s
; k = 1.3807e-23 J/K
; T temperature in K
; in J/HZ/m^2/s
; cenverting to lambda yields:
; bb = 2*h*c/(w^3) / (exp(h*c/(w*k*T))-1)
; w = wavelength in m
; converting to Jansky-Micron yields:
; bb = 1e26*1e18*2*h*c/(l^3) / (exp(h*c/(1e-6*l*k*T))-1)
; l = wavelength in micron
; bb = a1/(l^3)/(exp(a2/(l*T))-1)
; a1 = 1e26*1e18*2*h*c = 3.97296e19
; a2 = h*c/1e-6/k = 1.43875e4
  
  ;; Now just get along with it and do the calcuations
  a1 = 3.97296d19
  a2 = 1.43875d4
  bbflux = a1/(l^3)/(exp(a2/(l*T))-1)

  ;; this is in jansky
  ;; to convert to W/m^/micron do
  IF keyword_set(flambda) THEN BEGIN
      bbflux = bbflux *1d-26 * 2.99792458e8 /(wave)^2d0
  ENDIF

  return,bbflux
END 
