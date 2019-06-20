;; Simple function to get the value of a fitsheader in a string

function sh_getkey, header, keyword
  
;; Initialize
  result = ''

;; Allow for aa to be passed instead of the header itself
  IF is_aar(header) THEN BEGIN
    h = header.header
  ENDIF ELSE BEGIN
    h = header
  ENDELSE
  
  k = strupcase(keyword)
  ;; pad the keyword with some spaces
;  Can we find the keyword in the present header?
  k = strmid( k + '        ', 0, 8 )
  
;
; find the position of the keyword at the beginning of a line
;
  loc = 0
  REPEAT BEGIN
    loc = strpos(h,k,loc)
    IF loc EQ -1 THEN BEGIN
      message,'SH_READKEY: keyword could not be found',/INFORMATIONAL
      return,result
    ENDIF
  ENDREP UNTIL fix(loc/80) EQ (loc/80)
  
;     Get the important part of this line (from the = to the /)
  result = strmid(h,loc+9,22)
  
  loc1 = strpos(result,"'")
  loc_temp = loc1
;  find the last '
  REPEAT BEGIN
    loc2 = loc_temp
    loc_temp = strpos(result,"'",loc2+1)
  ENDREP UNTIL loc_temp EQ -1
  
  IF (loc1 NE -1) AND (loc2 NE -1) THEN BEGIN
    result = strtrim(strmid(result,loc1+1,loc2-loc1-1),2)
  ENDIF
  
  return,result
end
