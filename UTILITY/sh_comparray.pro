;compare array a with b and return positions where element of a is in
;b,optionally return where and which too, example:
;present = sh_comparray([3,3,2,4,2,3,1,5],[3,1],where=where,which=which)
;IDL > print,present
;   1   1   0   0   0   1   1   0
;IDL > print,where                                                            
;           0           1           5           6
;IDL > print,which
;       3       3       3       1

FUNCTION sh_comparray,a,b,where=where,which=which
  Na = n_elements(a)
  Nb = n_elements(b)
  where = -1
  which = -1
  
  ;;empty array a
  IF (Na EQ 0) THEN BEGIN
    print,'SH_COMPARRAY: no array a given'
    return,0
  ENDIF
  ;;empty array b
  IF (Nb EQ 0) THEN BEGIN
    print,'SH_COMPARRAY: no array b given'
    return,a*0
  ENDIF
  
;;Check for simple input
  IF (n_elements(a) EQ 1) THEN BEGIN
    out = total(a[0] EQ b) NE 0
    IF out THEN BEGIN
      where = 0
      which = a[0]
    ENDIF
    return,out
  ENDIF
  
  IF (n_elements(b) EQ 1) THEN BEGIN
    out = a EQ b[0]
    where = where(out)
    IF where(0) GE 0 THEN which = a[where]  
    return,out
  ENDIF
  
; stretch the values in both directions
  ; can do matrix multiplication for numeric values only
  IF is_numeric(A) AND is_numeric(B) THEN BEGIN
    AA = A#(b EQ b)
    BB = (a EQ a)#B
  ENDIF ELSE BEGIN
    l = lindgen(Na,Nb)
    AA = A[l mod Na]
    BB = B[l / Na]
  ENDELSE
  
;; Compare the two stretched matrices
  CC = (AA eq BB)
  where = where(CC) MOD Na
  which = B[where(CC) / Na]
; collapse the result
  return,total(CC,2) NE 0
END
