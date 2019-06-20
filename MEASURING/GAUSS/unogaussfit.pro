PRO unogaussfit_funct,x,d,f,pder
  ;;get z
  IF d[2] NE 0.0 THEN z1 = (x-d[1])/d[2] ELSE z1= 10.
  ;;gaussian part ignore small terms
  ez1 = EXP(-4.*ALOG(2.)*z1^2)*(ABS(z1) LE 7.) 
  f = d[0]*ez1
;
; Partial derivatives can easily be derived, so should be used according
; to `curvefit' instructions
;
;
  ;;Define array of partial derivatives
  pder = FLTARR(n_elements(x),3)
  ;;Compute partials for first Gaussian
  pder[*,0] = ez1 		
  IF d[2] NE 0. THEN pder[*,1] = d[0] * ez1 * z1/d[2]
  pder[*,2] = pder[*,1] * z1
END


FUNCTION unogaussfit, x, y, errory=errory, d , sigmad
  n= n_elements(y)
  IF keyword_set(errory) THEN BEGIN
    weight = 1./errory^2 
  ENDIF ELSE BEGIN
    print, '***Warning: to get reasonable X and sigma please give errors'
    weight=y/y
  ENDELSE
  absorptionfit = mycurvefit(x,y,weight,d,sigmad, $
                             FUNCTION_name = "unogaussfit_FUNCT") 
;call mycurvefit
;
  
  print, 'depth(tau)', d(0), 'sigma', sigmad(0)
  print, 'peak pos.', d(1), 'sigma', sigmad(1)
  print, 'FWHM(tau)', d(2), 'sigma', sigmad(2)
  return, absorptionfit
END
