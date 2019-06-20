FUNCTION ra2s,ra,full=full

  IF keyword_set(full) THEN BEGIN
      h = floor(ra/15.)
      m = floor((ra/15.-h)*60.)
      s = round((ra/15.-h-m/60.)*3600.*1d2)/1d2
      IF s EQ 60.00 THEN BEGIN
          s=0.00
          m=m+1
      ENDIF 
      str_h = strmid('00'+n2s(h)+'h',2,3,/reverse)
      str_m = strmid('00'+n2s(m)+'m',2,3,/reverse)
      str_s = strmid('00'+f2s(s,2)+'s',5,6,/reverse)
  ENDIF ELSE BEGIN
      h = floor(ra/15.)
      m = floor((ra/15.-h)*60.)
      s = round((ra/15.-h-m/60.)*3600.)
      IF s EQ 60 THEN BEGIN
          s=0
          m=m+1
      ENDIF 
      str_h = n2s(h)+'h'
      str_m = n2s(m)+'m'
      IF s NE 0 THEN str_s=n2s(s)+'s' ELSE str_s=''
  ENDELSE
  
  return, str_h+str_m+str_s
END
