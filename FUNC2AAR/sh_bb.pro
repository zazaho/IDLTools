FUNCTION sh_bb,aar,T,pindex,norm=norm
  
;(SH Jan 27 1999)
;wavelength dependent emissivity powerindex:power
; fit = bb(T)*H*l**power
;   (SH Sep  7 2015) Use explicit constants to calculate factors
  
  default,pindex,0d0
  pindex = double(pindex)
  out = aar
  IF is_aar(aar) THEN BEGIN
    got_aar = 1
    l = double(aar.data.wave)
  ENDIF ELSE BEGIN
    got_aar = 0
    l = double(aar)
  ENDELSE
  
;(SH Jan 27 1999)
; We hebben in Watt/m^2/Hz :
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
; dus de modified bb (mbb) heeft de volgende vorm:
; mbb = a1*l^(p-3)/(exp(a2/(l*T))-1)

;  ;; Nu niet moeilijk doen en gewoon rekenen:
;  a1 = 3.97296d19
;  a2 = 1.43875d4

  ;; use explicit constants to win in precision
  c = 2.99792458d8                        ;; Speed of light [SI]
  h = 6.62606876d-34                      ;; Planck's constant [SI]
  K = 1.3806503d-23                       ;; Boltzmann's constant [SI]
  
  a1 = 1d26*1d18*2d0*h*c
  a2 = h*c/1d-6/K

  bbflux = a1/(l^3)/(exp(a2/(l*T))-1d0)

  ;; add in the wien part
  idx_large_argument = where(a2/(l*T) gt 1d1)
  bbflux[idx_large_argument] = a1/(l[idx_large_argument]^3)*(exp(-1d0*a2/(l[idx_large_argument]*T)))
  
;(SH Jan 27 1999) Modified bb dus nog met l^(pindex vermenigvuldigen)
  mbbflux = bbflux * l^pindex
  
  if (n_elements(norm) eq 2) then BEGIN
    mbbflux = mbbflux * norm[1]/(shc_interpol(l,mbbflux,norm[0]))[0]
  endif
  
  IF got_aar THEN BEGIN
    out.data.flux = mbbflux
  ENDIF ELSE BEGIN
    out = mbbflux
  ENDELSE
  return,out
END 
