FUNCTION sh_conv_coords,in,to_degree=to_degree,help=help,noprint=noprint
  
  to_degree = keyword_set(to_degree)
  default,noprint,0
  doprint = noprint EQ 0
  
  ;; First we process the input
  ;; a simple string or
  ;; a string array
  
  N_in = n_elements(in)
  out = dindgen(N_in)
  FOR i = 0, N_in-1 DO BEGIN
    
    IF to_degree THEN BEGIN
      IF strpos(in[i],'h') NE -1 THEN BEGIN
        rad = float(strsplit(in[i],'hms',/extract))
        degree = rad[0]*15d0+rad[1]*15d0/60d0+rad[2]*15d0/3600d0
        IF doprint THEN print,in[i]+' CONVERTS TO: '+n2s(degree)
      ENDIF ELSE BEGIN
        dec = float(strsplit(in[i],"d'"+'"',/extract))
        sign = ([1,-1])[( strpos(in[i],'-') NE -1)]
        print,'SH_CONV_UTILITY the sign is: ',sign
        degree = sign*(abs(dec[0])+dec[1]/60d0+dec[2]/3600d0)
        IF doprint THEN print,in[i]+' CONVERTS TO: '+n2s(degree)
      ENDELSE
      out[i] = degree
    ENDIF
    
    
  ENDFOR
  return,out
END

    
