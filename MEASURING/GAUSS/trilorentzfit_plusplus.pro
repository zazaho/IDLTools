pro	trilorentzfitplusplus_funct,x,p,f,pder
; NAME:
;	trilorentzfit_FUNCT
; 
; CATEGORY:
;	E2 - CURVE AND SURFACE FITTING.
; CALLING SEQUENCE:
;	FUNCT,X,A,F,PDER
; INPUTS:
;	X = VALUES OF INDEPENDENT VARIABLE.
;	A = PARAMETERS OF EQUATION DESCRIBED BELOW.
; OUTPUTS:
;	F = VALUE OF FUNCTION AT EACH X(I)
;
; OPTIONAL OUTPUT PARAMETERS:
;	PDER = (N_ELEMENTS(X),6) ARRAY CONTAINING THE
;		PARTIAL DERIVATIVES.  P(I,J) = DERIVATIVE
;		AT ITH POINT W/RESPECT TO JTH PARAMETER.
; PROCEDURE:
;	f(x)=p(0)*p(2)^2/4/[(x-p(1))^2+p(2)^2/4]+p(3)*p(5)^2/4/[(x-p(4))^2+p(5)^2/4]+p(6)*p(8)^2/4/[(x-p(7))^2+p(8)^2/4]+poly(x-m,d(9:.)
;
; MODIFICATION HISTORY:
;	WRITTEN, CVanKerckhoven modification of gaussfit_funct
;
;
	ON_ERROR,2                        ;Return to caller if an error occurs
;print,1

m=mean(x) ;;;!!!!!!!!!!   x moet x zijn waarmee je poly gefit hebt


a=n_elements(p)

        p(0)=abs(p(0))
        p(3)=abs(p(3))
        p(6)=abs(p(6))

        S1=p(2)^2/4/[(x-p(1))^2+p(2)^2/4]
        f1=p(0)*S1 

        S2=p(5)^2/4/[(x-p(4))^2+p(5)^2/4]
        f2=p(3)*S2

        S3=p(8)^2/4/[(x-p(7))^2+p(8)^2/4]
        f3=p(6)*S3
        f=f1+f2+f3+poly(x-m,p(9:(a-1)))
;Function
;
;Partial derivatives can easily be derived, so should be used according
; to `curvefit' instructions
;
	;Define array of partial derivatives
	pder = DBLARR(n_elements(x),a) 
	;Compute partials
	pder[*,0] =S1	
	pder[*,1] =S1* 2*(x-p(1))/[(x-p(1))^2+p(2)^4]
	pder[*,2] =S1* [2/p(2)- p(2)/2/[(x-p(1))^2+p(2)^2/4]]
        pder[*,3] =S2	
	pder[*,4] =S2* 2*(x-p(4))/[(x-p(4))^2+p(5)^4]
	pder[*,5] =S2* [2/p(5)- p(5)/2/[(x-p(4))^2+p(5)^2/4]]
	pder[*,6] =S3	
	pder[*,7] =S3* 2*(x-p(7))/[(x-p(7))^2+p(8)^4]
	pder[*,8] =S3* [2/p(8)- p(8)/2/[(x-p(7))^2+p(8)^2/4]]
        for i=9,(a-1) do pder[*,i]=x^(i-9)
	return

end

 Function trilorentzfit_plusplus, x, y, errory=errory, p, sigmad
;+
; NAME:
;	mygaussfit with gaussian optical depth 
;
; PURPOSE:
; 	Fit y=f(x) where:
;	f(x)=p(0)*p(2)^2/4/[(x-p(1))^2+p(2)^2/4]+p(3)*p(5)^2/4/[(x-p(4))^2+p(5)^2/4]+poly(x-m,p(9:.))
; 	
; 	Estimate of parameters p0,p1,p2 have to be given and then call mycurvefit.
; CATEGORY:
;	curve - fitting
; CALLING SEQUENCE:
;	yfit = gaufitthick(x,y, errory=..., d,sigmad)
; INPUTS:
;	x = independent variable, must be a vector.
;	y = dependent variable, must have the same number of points
;		as x 
; OPTIONAL INPUT:
;	errory=... ; errors in y, used to calculate weight
; OUTPUTS:
;	yfit = fitted function.

;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
; PROCEDURE:
;	If the (max-avg) of Y is larger than (avg-min) then it is assumed
;	the line is an emission line, otherwise it is assumed there
;	is an absorption line.  The estimated center is the max or min
;	element.  The height is (max-avg) or (avg-min) respectively.
;	The width is found by searching out from the extreme until
;	a point is found < the 1/e value.
; MODIFICATION HISTORY:
; 	CVanKerckhoven 1/7/1998 modification of mygaussfit
;	, estimate of parameters needed in input
;                                
;
on_error,2                      ;Return to caller if an error occurs
n= n_elements(y)
IF keyword_set(errory) THEN weight = 1./errory^2 ELSE weight=y/y
lfit = mycurvefit(x,y,weight,p,sigmad, $
		function_name ='trilorentzfitplusplus_funct') ;call mycurvefit
;
print, ''
print, 'depth(tau)', p(0), 'sigma', sigmad(0)
print, 'peak pos.', p(1), 'sigma', sigmad(1)
print, 'gamma(tau)', p(2), 'sigma', sigmad(2)

print, 'depth(tau)', p(3), 'sigma', sigmad(3)
print, 'peak pos.', p(4), 'sigma', sigmad(4)
print, 'gamma(tau)', p(5), 'sigma', sigmad(5)

print, 'depth(tau)', p(6), 'sigma', sigmad(6)
print, 'peak pos.', p(7), 'sigma', sigmad(7)
print, 'gamma(tau)', p(8), 'sigma', sigmad(8)
print, 'baseline= poly(x,c) met = c',p(9:(n_elements(p)-1))
return, lfit
end














