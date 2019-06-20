FUNCTION sim_spec,p1_in,p2,p3,p4,cme=cme,cms=cms,temp=temp,nonorm=nonorm, $
                  fact=fact,resample=resample,tau=tau,simcmcde=simcmcde, $
                  abs=abs,_extra=_extra
  
  p1 = p1_in
  IF keyword_set(resample) THEN BEGIN
      nl = n_elements(p1[0,*])
      l = reform(p1[0,*],nl)
      n = reform(p1[1,*],nl)
      k = reform(p1[2,*],nl)

      minl = min(l,max=maxl)
      l_resamp = minl+dindgen(nl*resample)*(maxl-minl)/(nl*resample-1d0)
      n_resamp = interpol(n,l,l_resamp,/quadra) 
      k_resamp = interpol(k,l,l_resamp,/quadra) 
      p1 = make_array(3,nl*resample,value=0d0)
      p1[0,*] = l_resamp
      p1[1,*] = n_resamp
      p1[2,*] = k_resamp
  ENDIF 

  CASE (1) OF 
      ;; Try to simulate a core mantle with cde distribution by adding
      ;; a few cme's
      keyword_set(simcmcde): BEGIN
          default,p3,0.9
          out = core_mantle(p3,1d0,p1,p2,/aar,_extra=_extra)
          out = add(out,fact(core_mantle_ellipsoid([10,1,1]*p3,[10,1,1],p1,p2,/aar,_extra=_extra),1d0/10))
          out = add(out,fact(core_mantle_ellipsoid([3,1,1]*p3,[3,1,1],p1,p2,/aar,_extra=_extra),1d0/3))
          out = add(out,fact(core_mantle_ellipsoid([3,3,1]*p3,[3,3,1],p1,p2,/aar,_extra=_extra),1d0/9))
          out = add(out,fact(core_mantle_ellipsoid([10,10,1]*p3,[10,10,1],p1,p2,/aar,_extra=_extra),1d0/100))
      END
      keyword_set(cme): BEGIN
          default,p3,[1,.3,.3]*0.9
          default,p4,1d0/0.9
          out = core_mantle_ellipsoid(p3,p4,p1,p2,/aar,_extra=_extra)
      END
      keyword_set(cms): BEGIN
          default,p3,0.9
          default,p4,1d0
          out = core_mantle(p3,p4,p1,p2,/aar,_extra=_extra)
      END
      ELSE: BEGIN
          default,p2,1d0
          out= c_abs(p2,p1,/aar,_extra=_extra)
      END
  ENDCASE

  kappa = out.data.flux

  IF keyword_set(temp) THEN BEGIN
      a1 = 3.97296d19
      a2 = 1.43875d4
      bb = a1/(out.data.wave^3d0)/(exp(a2/(out.data.wave*temp))-1d0)

      IF keyword_set(tau) THEN BEGIN
          out.data.flux=bb*(1d0-exp(-tau*kappa/max(kappa)))
      ENDIF ELSE BEGIN
          out.data.flux = out.data.flux*bb
      ENDELSE
  ENDIF
  
  IF keyword_set(abs) THEN BEGIN
      out.data.flux = out.data.flux*exp(-abs*kappa/max(kappa))
  ENDIF 

  IF NOT keyword_set(nonorm) THEN BEGIN
      default,fact,1d0
      out.data.flux = fact*out.data.flux/max(out.data.flux)
  ENDIF
  
  return,out

END
