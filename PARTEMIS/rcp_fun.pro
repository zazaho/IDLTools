PRO rcp_fun, X, APAR, Funct, PDER
;
; e.g. Result = rcp_fun(dchan_var, apar)
;
;	Funct = pix*[X1,X1]+pix*alpha*[X2,X2]
;
; X = [X1,X2] Independent variable (vector)
; N_el = n_elements(X) assumed to be an even number
; X1 = X(0:N_el/2-1)     ;  X2 = X(N_el/2:N_el-1)
;
; APAR(0): pix   (pixel size)
; APAR(1): alpha (rotation angle of the array in rad assumed to be small)
;

N_el = n_elements(X)
X1 = X(0:N_el/2-1)
X2 = X(N_el/2:N_el-1)

Funct = APAR(0)*[X1,X1]+APAR(0)*APAR(1)*[X2,X2]

;
;    If the procedure is called with four parameters, calculate the
;    partial derivatives.
;
  IF N_PARAMS() GE 4 THEN BEGIN

	PDER0 = [X1,X1]+APAR(1)*[X2,X2]
	PDER1 = APAR(0)*[X2,X2]	

        PDER = [[PDER0], [PDER1]]
    
  ENDIF
;
  
END
