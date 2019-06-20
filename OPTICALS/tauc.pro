FUNCTION tauc,Eg=Eg,B=B,Erange=Erange,lrange=lrange,nsteps=nsteps,tauc=tauc

  default,Eg,0.5
  default,B,3.7d4
  default,Erange,[0.3,4]
  default,nsteps,1001
  default,tauc,0
  
  ;; Constants
  c = 2.9979d8    ;;m/s 
  h = 6.62620d-34 ;; Js
  eV = 1.602189d-19 ;;J

  IF keyword_set(lrange) THEN BEGIN
      Erange = h*c/eV/(lrange*1d-6)
  ENDIF 
  
  idx = sort(Erange)
  Erange=double(Erange[idx])

  E = Erange[0]*10d0^(alog10(Erange[1]/Erange[0])*(dindgen(nsteps)/(nsteps-1)))
  l = h*c/eV*1d6/E
  
  IF tauc EQ 0 THEN BEGIN
      ;;calculate E_Tauc from E_04
      Et = Eg - (1d4*Eg/B)^0.5d0
  ENDIF ELSE BEGIN
      Et = Eg
  ENDELSE 

;; And alpha
  alpha = B*(E-Et)^2d0/E
;; make sure we don't get crap from the extrapolation in the IR
  good = where(E GT Et)

  alpha = alpha[good]
  l = l[good]
  E = E[good]
  
  k = alpha/(4d0*!dpi)*l/1d4
  
  out = define_aar(len=n_elements(l))
  out.data.wave = l
  out.data.flux = alpha
  out.data.stdev = k

  return,out

END
