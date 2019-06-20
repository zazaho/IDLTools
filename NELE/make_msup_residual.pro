FUNCTION make_msup_residual,data,model,scaled_model=scaled_model, $
                            mrange=mrange,ebv=ebv,r_v=r_v
  
  scaled_model=model

  default,mrange,[3.5,4.5]
  default,R_V,3.6d0
  
  max_mrange = max(mrange,min=min_mrange)
  l_ref = (max_mrange+min_mrange)/2d0
  
  ;; first we treat the model
  mwave = model.full.data.wave
  mflux = model.full.data.flux

  idx_model = where( (mwave GE min_mrange) AND $
                     (mwave LE mAX_mrange), cnt)
  
  IF cnt LT 2 THEN BEGIN
     f_ref_model = interpol(mflux,mwave,l_ref,/spline)
  ENDIF ELSE BEGIN
     fitpars_model = poly_fit(mwave[idx_model],mflux[idx_model],1)
     f_ref_model = fitpars_model[0]+l_ref*fitpars_model[1]
  ENDELSE
  
  ;; next the data
  dwave = data.data.wave
  dflux = data.data.flux

  idx_data = where( (dwave GE min_mrange) AND $
                    (dwave LE max_mrange), cnt)
  
  IF cnt LT 2 THEN BEGIN
     f_ref_data = interpol(dflux,dwave,l_ref,/spline)
  ENDIF ELSE BEGIN
     fitpars_data = poly_fit(dwave[idx_data],dflux[idx_data],1)
     f_ref_data = fitpars_data[0]+l_ref*fitpars_data[1]
  ENDELSE

  ;; now determine the normalisation factor
  match_factor =  f_ref_data/f_ref_model

  scaled_model.full.data.flux  = scaled_model.full.data.flux  * match_factor
  scaled_model.full.data.stdev = scaled_model.full.data.stdev * match_factor
  scaled_model.star.data.flux  = scaled_model.star.data.flux  * match_factor
  scaled_model.star.data.stdev = scaled_model.star.data.stdev * match_factor

  ;; also treat unredding
  IF n_elements(ebv) EQ 1 THEN BEGIN
     A_V = R_V * EBV
     
     x=1/(aar.data.wave)
     
     AloAv=x*0.0d
     
     sel=where(x lt 1.1,cnt)
     if (cnt ne 0) then begin
        y=x[sel]
        a=0.574*y^1.61
        b=-0.527*y^1.61
        AloAv[sel]=a+b/R_V
     endif
     
     sel=where((x ge 1.1) and (x lt 3.3),cnt)
     if (cnt ne 0) then begin
        y=x[sel]-1.82d
        a=1 + 0.17699*y^1 - 0.50447*y^2 - 0.02427*y^3 + 0.72085*y^4 + 0.01979*y^5 $
          - 0.77530*y^6 + 0.32999*y^7
        b=    1.41338*y^1 + 2.28305*y^2 + 1.07233*y^3 - 5.38434*y^4 - 0.62251*y^5 $
              + 5.30260*y^6 - 2.09002*y^7
        AloAv[sel]=a+b/R_V
     endif
     
     sel=where((x ge 3.3) and (x lt 5.9),cnt)
     if (cnt ne 0) then begin
        y=x[sel]
        a= 1.752 - 0.316*y - 0.104/((y-4.67)^2 + 0.341)
        b=-3.090 + 1.825*y + 1.206/((y-4.62)^2 + 0.263)
        AloAv[sel]=a+b/R_V
     endif
     
     sel=where((x ge 5.9) and (x lt 8.0),cnt)
     if (cnt ne 0) then begin
        y=x[sel]
        Fa= -0.04473*(y-5.9)^2 - 0.009779*(y-5.9)^3
        Fb=  0.2130 *(y-5.9)^2 + 0.1207  *(y-5.9)^3
        a= 1.752 - 0.316*y - 0.104/((y-4.67)^2 + 0.341) + Fa
        b=-3.090 + 1.825*y + 1.206/((y-4.62)^2 + 0.263) + Fb
        AloAv[sel]=a+b/R_V
     endif
     
     sel=where(x ge 8.0,cnt)
     if (cnt ne 0) then begin
        y=x[sel]
        a= -1.073 - 0.628*(y-8) + 0.137*(y-8)^2 - 0.070*(y-8)^3
        b= 13.670 + 4.257*(y-8) - 0.420*(y-8)^2 + 0.374*(y-8)^3
        AloAv[sel]=a+b/R_V
     endif
     
     scaled_model.full.data.flux  = scaled_model.full.data.flux  * mag2flux(-AloAv*A_V,0d0)
     scaled_model.full.data.stdev = scaled_model.full.data.stdev * mag2flux(-AloAv*A_V,0d0)
  ENDIF

  residual = subtract(data,scaled_model.full)
  return, residual
END
