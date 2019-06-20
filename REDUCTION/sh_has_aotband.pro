FUNCTION sh_has_aotband, structin, aotband
  
  return,sh_band(structin,aotband,/test)
;  IF is_aar(structin) THEN BEGIN
;    det=structin.data.det
;    flag=structin.data.flag
;  ENDIF ELSE BEGIN
;    print,'not an aar!'
;  ENDELSE
;  
;  CASE aotband OF
;    '1a': indx=where(test_flag(flag,order=4) EQ 1)
;    '1b': indx=where((det GE 1) AND (det LE 12) AND (test_status(structin,aperture=1) EQ 1) AND $
;                     (test_flag(flag,order=3) EQ 1))
;    '1d': indx=where((det GE 1) AND (det LE 12) AND (test_status(structin,aperture=2) EQ 1) AND $
;                     (test_flag(flag,order=3) EQ 1))
;    '1e': indx=where((det GE 1) AND (det LE 12) AND (test_status(structin,aperture=2) EQ 1) AND $
;                     (test_flag(flag,order=2) EQ 1))
;    '2a': indx=where((det GE 13) AND (det LE 24) AND (test_status(structin,aperture=2) EQ 1) AND $
;                     (test_flag(flag,order=2) EQ 1))
;    '2b': indx=where((det GE 13) AND (det LE 24) AND (test_status(structin,aperture=2) EQ 1) AND $
;                     (test_flag(flag,order=1) EQ 1))
;    '2c': indx=where((det GE 13) AND (det LE 24) AND (test_status(structin,aperture=3) EQ 1) AND $
;                     (test_flag(flag,order=1) EQ 1))
;    '3a': indx=where((det GE 25) AND (det LE 36) AND (test_status(structin,aperture=1) EQ 1) AND $
;                     (test_flag(flag,order=2) EQ 1))
;    '3c': indx=where((det GE 25) AND (det LE 36) AND (test_status(structin,aperture=2) EQ 1) AND $
;                     (test_flag(flag,order=2) EQ 1))
;    '3d': indx=where((det GE 25) AND (det LE 36) AND (test_status(structin,aperture=2) EQ 1) AND $
;                     (test_flag(flag,order=1) EQ 1))
;    '3d': indx=where((det GE 25) AND (det LE 36) AND (test_status(structin,aperture=3) EQ 1) AND $
;                     (test_flag(flag,order=1) EQ 1))
;    '4' : indx=where((det GE 37) AND (det LE 48) AND (test_status(structin,aperture=3) EQ 1) AND $
;                     (test_flag(flag,order=1) EQ 1))
;    
;  return, indx NE -1
END
