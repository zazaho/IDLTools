; Do an operation on 1 line of an aar. Uses sh_calcaar for the actual
; computations.
FUNCTION sh_calcline,aar,line,_extra=_extra
  
  ;; Test for good line number
  idx = aar.data.line EQ line
  IF total(idx) NE 0 THEN BEGIN
    return,sh_combine(sh_calcaar(sh_select(aar,idx),_extra=_extra), $
                      sh_select(aar,idx EQ 0))
  ENDIF ELSE BEGIN
    print,'SH_CALCLINE: the line is not present'
    return,aar
  ENDELSE
  
END
