pro	trigaussfitplus_funct,x,d,f,pder
; NAME:
;	trigaussfit_FUNCT
; PURPOSE: sum of tree aborption lines with a gaussian optical 
;	   depth, input parameters on optical depth scale
;
; CATEGORY:
;	E2 - CURVE AND SURFACE FITTING.
; CALLING SEQUENCE:
;	FUNCT,X,A,F,PDER
; INPUTS:
;	X = VALUES OF INDEPENDENT VARIABLE.
;	A = PARAMETERS OF EQUATION DESCRIBED BELOW.
; OUTPUTS:
;	F = VALUE OF FUNCTION AT EACH X(I), in most cases this will be
;           the minus logarithm of the transmission, i.e. the optical depth
;
; OPTIONAL OUTPUT PARAMETERS:
;	PDER = (N_ELEMENTS(X),9) ARRAY CONTAINING THE
;		PARTIAL DERIVATIVES.  P(I,J) = DERIVATIVE
;		AT ITH POINT W/RESPECT TO JTH PARAMETER.
; COMMON BLOCKS:
;	NONE.
; SIDE EFFECTS:
;	NONE.
; RESTRICTIONS:
;	NONE.
; PROCEDURE:
;	F = D(0)*EXP(-Z1^2/2)-D(3)*EXP(-Z2^2/2)-D(6)*EXP(-Z3^2/2)+d9+d10*x 
;	Z1 = (X-D(1))/D(2)  Z2=(X-D(4))/D(5) Z3=(X-D(7))/D(8)
;	D0 = tau(0) (0-1), D1 = center of exp, 
;       D2 = FWHM on tau scale (2sigma^2 = FWHM^2 / (4LN(2))).
;       D3, D4, D5 analogous for 2nd line
;       D6,D7,D8  analogous for 3th line
;       rechte met vergelijking d9+d10*x (baseline)
; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(2) is 0.
;	Added to Gauss_fit, when the variable function name to
;		Curve_fit was implemented.  DMS, Nov, 1990.
;       20-01-1994, ACA Boogert: first version (modification of 
;        'gau2fitthick' that has intensity scale parameters as input/output)
;       8-6-1998 modification from double to trigausfit_function
	ON_ERROR,2                        ;Return to caller if an error occurs

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
	f = d[0]*ez1+d[3]*ez2+d[6]*ez3+d[9]+d[10]*x 
;
; Partial derivatives can easily be derived, so should be used according
; to `curvefit' instructions
;
;
	;Define array of partial derivatives
	pder = FLTARR(n_elements(x),11) 
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
        pder[*,9]=1
        pder[*,10]=x

	RETURN
END


Function trigaussfit_plus, x, y, errory=errory, d , sigmad
;+
; NAME:
;	trigaussfit with gaussian optical depth +baseline.
;       
; PURPOSE:
; 	Fit y=f(x) where:
; 	F(x) = d0*exp(-z1^2/2)+d3*exp(-z2^2/2)+d6*exp(-z3^2/2)+d9+d10*x
; 		and z1=(x-d1)/d2, z2=(x-d4)/d5 z3=(x-d7)/d8
;	d0 = central optical depth of absorption, d1 = center of gaussian, 
;       d2 = width (FWHM) in optical depth
; 	Estimate of parameters d0,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10 have to be 
;	given and then call mycurvefit.
; CATEGORY:
;	curve - fitting
; CALLING SEQUENCE:
;	yfit = trigaussfit(x,y, errory=..., d,sigmad)
; INPUTS:
;	x = independent variable, must be a vector.
;	y = dependent variable, must have the same number of points
;	as x 
;	OPTIONAL INPUT:
;       errory = ... error in y values from which the weight is calculated
; OUTPUTS:
;	yfit = fitted function.
; OPTIONAL OUTPUT PARAMETERS:
;	d = coefficients. a nine element vector as described above, 
;	ON TAU SCALE!!!.
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The peak or minimum of the gaussian must be the largest
;	or respectively the smallest point in the Y vector.
; PROCEDURE:
;	If the (max-avg) of Y is larger than (avg-min) then it is assumed
;	the line is an emission line, otherwise it is assumed there
;	is an absorption line.  The estimated center is the max or min
;	element.  The height is (max-avg) or (avg-min) respectively.
;	The width is found by searching out from the extreme until
;	a point is found < the 1/e value.
; MODIFICATION HISTORY:
;	DMS, RSI, Dec, 1983.
;       20-01-1995, ACAB       : first version, like gau2fitthick but on 
;				optical depth scale
;       8-6-1998 Caroline Van Kerckhoven : double to triplegaussfit
;
on_error,2                      ;Return to caller if an error occurs
n= n_elements(y)
IF keyword_set(errory) THEN BEGIN
 weight = 1./errory^2 
ENDIF ELSE BEGIN
 print, '***Warning: to get reasonable X and sigma please give errors'
 weight=y/y
ENDELSE
absorptionfit = mycurvefit(x,y,weight,d,sigmad, $
		function_name = "trigaussfitplus_FUNCT") ;call mycurvefit
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
print, 'rechte= a+bx met a=',d(9),  'sigma', sigmad(9)
print, '                 b=',d(10),  'sigma', sigmad(10)
return, absorptionfit
end


