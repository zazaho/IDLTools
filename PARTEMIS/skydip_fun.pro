FUNCTION skydip_fun, X, APAR
;
; e.g. Result = skydip_fun(amass, apar)
;
;       Funct = C*(1.-exp(-A*x))+B
;
; X : Independent variable (vector)
; A = APAR(0): Zenith optical depth
; B = APAR(1): Offset
; C = APAR(2): Scaling factor
;
         Funct = APAR(2)*(1.-exp(-APAR(0)*X))+APAR(1)
;
         PDER0 = APAR(2)*X*exp(-APAR(0)*X)
         PDER1 = X*0.+1.
         PDER2 = -exp(-APAR(0)*X)+1.

     Result = [[Funct], [PDER0], [PDER1], [PDER2]]

     Return, Result

END 
