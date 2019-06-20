FUNCTION finitise,i,value=value,median=median

  default,value,0d0
  
  o = i

  bad = where(finite(o) EQ 0,cnt)
  IF cnt NE 0 THEN BEGIN
     IF keyword_set(median) THEN BEGIN
        CASE median OF 
           1: BEGIN
              o[bad] = median(O)
           END
           ELSE: BEGIN
              o[bad] = (median(o,median))[bad]
           END
        ENDCASE
     ENDIF ELSE BEGIN
        o[bad] = value
     ENDELSE
  ENDIF
  
  return,o
END
