FUNCTION de_kurucz,aar,MODEL=model

  IF NOT is_aar(aar) THEN error,'F','No valid AAR structure specified!'

  IF NOT keyword_set(model) THEN BEGIN
    restore,'kuruczmodel_30000K_3.5g'
    model = kurlin
  ENDIF

  kurflux = spline(model.wave,model.flux,aar.data.wave)
  newaar = aar
  newaar.data.flux = newaar.data.flux - kurflux
  return,newaar
END 

