PRO featurefit_funct,x,d,f,pder
  COMMON FEATUREFIT_COMMON,ftx,fty
  ON_ERROR,2                    ;Return to caller if an error occurs
  d = double(d)
  
                                ; Now we first shift the template spectrum
  sftx = ftx + d(1)
                                ;Next we interpolate data to the x
  sfty = interpol(fty,sftx,x)
                                ;and multiply by the factor
  f = d(0)*sfty

;
; Partial derivatives can easily be derived, so should be used according
; to `curvefit' instructions
;
;
                                ; The deriv we use:
  pder = FLTARR(n_elements(x),2)
  pder(*,0) = sfty
  pder(*,1) = -1d0*deriv(x,f)
  
  RETURN
END


FUNCTION featurefit, in, errory=errory,d,sigmad,feature=f,xrange=xrange,out=out
  
  COMMON FEATUREFIT_COMMON,ftx,fty
  
  on_error,2                    ;Return to caller if an error occurs
  
  a = in
  IF (n_elements(xrange) EQ 2) THEN  a = sh_select_range(a,/q,xrange=xrange)
                                ; make simple arrays for efficiency
  IF IS_AAR(f) THEN BEGIN
    ft_aar=f
  ENDIF ELSE BEGIN
    ft_aar = read_faar(f)
  ENDELSE
  
  ftx = ft_aar.data.wave
  fty = ft_aar.data.flux
  
  x = a.data.wave
  y = a.data.flux
  
  IF keyword_set(errory) THEN BEGIN
    weight = 1./errory^2 
  ENDIF ELSE BEGIN
    weight=y/y
  ENDELSE
  
  ;estimate the initial fact as the maximimum of the flux
  CASE n_elements(d) OF
    0: BEGIN
      d = [0d0,0d0]
      d(0) = max(y)
    END
    1: BEGIN
      d = [d,0d0]
    END
    ELSE: BEGIN
      d =[d(0),d(1)]
    END
  ENDCASE
  
  afit = mycurvefit(x,y,weight,d,sigmad, $
                    FUNCTION_name = "featurefit_FUNCT" $
;;                    ,/noder $
                   ) 
;call mycurvefit
;
  
  print, 'fact', d(0), 'sigma', sigmad(0)
  print, 'shift', d(1), 'sigma', sigmad(1)
  
  out = in
  out.data.flux = interpol(fty*d(0),ftx+d(1),out.data.wave)
  return,d
END
