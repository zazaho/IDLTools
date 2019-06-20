pro	trigaussfit_funct,x,d,f,pder
  ON_ERROR,2                    ;Return to caller if an error occurs
  
                                ;get z
  IF d[2] NE 0.0 THEN z1 = (x-d[1])/d[2] ELSE z1= 10.
                                ;gaussian part ignore small terms
  ez1 = EXP(-4.*ALOG(2.)*z1^2)*(ABS(z1) LE 7.) 
                                ;get z
  IF d[5] NE 0.0 THEN z2 = (x-d[4])/d[5] ELSE z2= 10.
                                ;gaussian part ignore small terms
  ez2 = EXP(-4.*ALOG(2.)*z2^2)*(ABS(z2) LE 7.) 
                                ;get z
  IF d[8] NE 0.0 THEN z3 = (x-d[7])/d[8] ELSE z3= 10.
	;gaussian part ignore small terms
  ez3 = EXP(-4.*ALOG(2.)*z3^2)*(ABS(z3) LE 7.) 
                                ;functions.
  f = d[0]*ez1+d[3]*ez2+d[6]*ez3 
;
; Partial derivatives can easily be derived, so should be used according
; to `curvefit' instructions
;
;
                                ;Define array of partial derivatives
  pder = FLTARR(n_elements(x),9) 
                                ;Compute partials for first Gaussian
  pder[*,0] = ez1 		
  IF d[2] NE 0. then pder[*,1] = d[0] * ez1 * z1/d[2]
  pder[*,2] = pder[*,1] * z1
                                ;Compute partials for second Gaussian
  pder[*,3] = ez2
  IF d[5] NE 0. THEN pder[*,4] = d[3] * ez2 * z2/d[5]
  pder[*,5] = pder[*,4] * z2
                                ;Compute partials for third Gaussian
  pder[*,6] = ez3
  IF d[8] NE 0. THEN pder[*,7] = d[6] * ez3 * z3/d[8]
  pder[*,8] = pder[*,7] * z3
  
  RETURN
END


Function trigaussfit, x, y, errory=errory, d , sigmad
  on_error,2                    ;Return to caller if an error occurs
  n= n_elements(y)
  IF keyword_set(errory) THEN BEGIN
    weight = 1./errory^2 
  ENDIF ELSE BEGIN
    print, '***Warning: to get reasonable X and sigma please give errors'
    weight=y/y
  ENDELSE
  absorptionfit = mycurvefit(x,y,weight,d,sigmad, $
                             function_name = "trigaussfit_FUNCT") 
;call mycurvefit
;
  
  print, 'depth(tau)', d(0), 'sigma', sigmad(0)
  print, 'peak pos.', d(1), 'sigma', sigmad(1)
  print, 'FWHM(tau)', d(2), 'sigma', sigmad(2)
  
  print, 'depth(tau)', d(3), 'sigma', sigmad(3)
  print, 'peak pos.', d(4), 'sigma', sigmad(4)
  print, 'FWHM(tau)', d(5), 'sigma', sigmad(5)
  
  print, 'depth(tau)', d(6), 'sigma', sigmad(6)
  print, 'peak pos.', d(7), 'sigma', sigmad(7)
  print, 'FWHM(tau)', d(8), 'sigma', sigmad(8)
  
  return, absorptionfit
end


