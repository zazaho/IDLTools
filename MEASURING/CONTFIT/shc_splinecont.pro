;(SH Dec 21 1999)
; Tool to make continuum from selected points
; Can do in flambda
FUNCTION shc_splinecont,in,sel,flambda=flambda
  
  tempsel = sel
  
  IF keyword_set(flambda) THEN BEGIN
    tempsel = sh_calcaar(sel,/flambda)
  ENDIF
  
  out =  in
  x = out.data.wave
  
  y = shc_spline(tempsel.data.wave,tempsel.data.flux,x)
  
  out.data.flux = y
  
  IF keyword_set(flambda) THEN BEGIN
    out = sh_calcaar(out,flambda=-1)
  ENDIF
  
  return,out
END
