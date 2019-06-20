FUNCTION utc2date,s,format=frmt,julday=jd
  
  IF (N_ELEMENTS(s) EQ 0) THEN BEGIN
    print,"Usage dte = utc2date,struct or dte = utc2date,'973044324'"
    return,''
  ENDIF ELSE BEGIN
    IF (N_TAGS(s) EQ 0) THEN BEGIN ;; Just a simple string ?
      utcs = s
    ENDIF ELSE BEGIN
      IF (n_elements(s.header) EQ 0) THEN BEGIN
        print,'The strcture y passed has no valid header'
        print,"Usage dte = utc2date,struct or dte = utc2date,'973044324'"
        return,''
      ENDIF ELSE BEGIN
        status = READ_FITS_KEY( s.header, 'EOHAUTCS', utcs, comment )
        IF (status NE 0) THEN $
          AAS_ERROR,'W','Problem with header key EOHAUTCS','SAUS'
      ENDELSE
    ENDELSE
  ENDELSE
  
  default,frmt,'(C(CDI,"-",CMoA,"-",CYI))'
    
  yy = long( strmid( utcs, 0, 2 ) ) ; Year - 1900
  dd = long( strmid( utcs, 2, 3 ) ) ; Day of year (1 <= dd <= 366)
  hh = long( strmid( utcs, 5, 2 ) ) ; Hour
  mm = long( strmid( utcs, 7, 2 ) ) ; Minute
  ss = long( strmid( utcs, 9, 2 ) ) ; Second
  
  yy = 1900+yy
  
  schrik=(((yy MOD 4) EQ 0) AND (((yy MOD 100) NE 0) OR ((yy MOD 400) EQ 0)))
  IF (schrik) THEN $
    sumday = [0,31,60,91,121,152,182,213,244,274,305,335,366] $
  ELSE $
    sumday = [0,31,59,90,120,151,181,212,243,273,304,334,365]
  
  IF (dd GT sumday(12)) THEN BEGIN
    print,'The daynumber is higher than the number of days in that year'
    return,''
  ENDIF
  
  m= min(where(sumday GE dd))
  d = dd - sumday(m-1)
  jd = JULDAY(m,d,yy,hh,mm,ss)
  return,string(format=frmt,jd)
END
 
