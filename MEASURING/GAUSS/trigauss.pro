function trigauss,w,d
; INPUTS:
;	W = VALUES OF INDEPENDENT VARIABLE.
;	A = PARAMETERS OF EQUATION DESCRIBED BELOW.
; OUTPUTS:
;	F = VALUE OF FUNCTION AT EACH W(I)
; PROCEDURE:
;	F = D(0)*EXP(-Z1^2/2)-D(3)*EXP(-Z2^2/2)-D(6)*EXP(-Z3^2/2)
;	Z1 = (W-D(1))/D(2)  Z2=(W-D(4))/D(5) Z3=(W-D(7))/D(8)
;	D0 = tau(0) (0-1), D1 = center of exp, 
;       D2 = FWHM on tau scale (2sigma^2 = FWHM^2 / (4LN(2))).
;       D3, D4, D5 analogous for 2nd line
;       D6,D7,D8 analogous for 3th line
; HISTORY:
;       8-6-1998 CAROLINE VAN KERCHOVEN

	;get z
	IF d[2] NE 0.0 THEN z1 = (w-d[1])/d[2] ELSE z1= 10.
	;gaussian part ignore small terms
	ez1 = EXP(-4.*ALOG(2.)*z1^2)*(ABS(z1) LE 7.) 
	;get z
	IF d[5] NE 0.0 THEN z2 = (w-d[4])/d[5] ELSE z2= 10.
	;gaussian part ignore small terms
	ez2 = EXP(-4.*ALOG(2.)*z2^2)*(ABS(z2) LE 7.) 
	;get z
	IF d[8] NE 0.0 THEN z3 = (w-d[7])/d[8] ELSE z3= 10.
	;gaussian part ignore small terms
	ez3 = EXP(-4.*ALOG(2.)*z3^2)*(ABS(z3) LE 7.) 
        ;functions.
        
	f = d[0]*ez1+d[3]*ez2+d[6]*ez3 

RETURN,F

end