FUNCTION dec2s,decIn,full=full
  dec=decIn
  sign = dec GE 0
  dec = abs(dec)
  d = floor(dec)
  m = floor((dec-d)*60.)
  IF keyword_set(full) THEN BEGIN
      s = round((dec-d-m/60.)*3600.*1d2)/1d2
      IF s EQ 60.00 THEN BEGIN
          s=0.00
          m=m+1
      ENDIF 
  
      str_d = (['-','+'])[sign]+strmid('00'+n2s(d)+'d',2,3,/reverse)
      str_m = strmid('00'+n2s(m)+'m',2,3,/reverse)
      str_s = strmid('00'+f2s(s,2)+'s',5,6,/reverse)
  ENDIF ELSE BEGIN
      s = round((dec-d-m/60.)*3600.)
      IF s EQ 60 THEN BEGIN
          s=0
          m=m+1
      ENDIF 

      str_d = (['-','+'])[sign]+n2s(d)+'d'
      str_m = n2s(m)+'m'
      IF (s NE 0) THEN str_s=n2s(s)+'s' ELSE str_s=''
  ENDELSE
  
  return, str_d+str_m+str_s
END
