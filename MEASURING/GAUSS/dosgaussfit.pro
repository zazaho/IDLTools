pro	dosgaussfit_funct,x,d,f,pder
  ON_ERROR,2                    ;Return to caller if an error occurs
  
                                ;get z
  IF d[2] NE 0.0 THEN z1 = (x-d[1])/d[2] ELSE z1= 10.
                                ;gaussian part ignore small terms
  ez1 = EXP(-4.*ALOG(2.)*z1^2)*(ABS(z1) LE 7.) 
                                ;get z
  IF d[5] NE 0.0 THEN z2 = (x-d[4])/d[5] ELSE z2= 10.
                                ;gaussian part ignore small terms
  ez2 = EXP(-4.*ALOG(2.)*z2^2)*(ABS(z2) LE 7.) 
                                ;functions.
  f = d[0]*ez1+d[3]*ez2 
;
; Partial derivatives can easily be derived, so should be used according
; to `curvefit' instructions
;
;
                                ;Define array of partial derivatives
  pder = FLTARR(n_elements(x),6)
                                ;Compute partials for first Gaussian
  pder[*,0] = ez1 		
  IF d[2] NE 0. then pder[*,1] = d[0] * ez1 * z1/d[2]
  pder[*,2] = pder[*,1] * z1
                                ;Compute partials for second Gaussian
  pder[*,3] = ez2
  IF d[5] NE 0. THEN pder[*,4] = d[3] * ez2 * z2/d[5]
  pder[*,5] = pder[*,4] * z2
  RETURN
END


Function dosgaussfit, x, y, errory=errory, d , sigmad
  on_error,2                    ;Return to caller if an error occurs
  n= n_elements(y)
  IF keyword_set(errory) THEN BEGIN
    weight = 1./errory^2 
  ENDIF ELSE BEGIN
    print, '***Warning: to get reasonable X and sigma please give errors'
    weight=y/y
  ENDELSE


  IF n_elements(d) ne 6 THEN BEGIN
    print, '***Warning: no initial params passed risk of getting bad results'
    ;; going with very basic assumptions on the initial parameters
    xmin = min(x,max=xmax)
    ymin = min(y,max=ymax)
    d=make_array(6,value=0d0)
    d[0] = xmin+(xmax-xmin)/3d0
    d[3] = xmin+(xmax-xmin)/3d0*2d0
    d[1] = (ymax-ymin)/2d0
    d[4] = (ymax-ymin)/2d0
    d[2] = (xmax-xmin)/1d1
    d[5] = (xmax-xmin)/1d1
  ENDIF
  absorptionfit = mycurvefit(x,y,weight,d,sigmad, $
                             function_name = "dosgaussfit_FUNCT") 
;call mycurvefit
;
  
  print, 'depth(tau)', d(0), 'sigma', sigmad(0)
  print, 'peak pos.', d(1), 'sigma', sigmad(1)
  print, 'FWHM(tau)', d(2), 'sigma', sigmad(2)
  
  print, 'depth(tau)', d(3), 'sigma', sigmad(3)
  print, 'peak pos.', d(4), 'sigma', sigmad(4)
  print, 'FWHM(tau)', d(5), 'sigma', sigmad(5)
  
  return, absorptionfit
end
