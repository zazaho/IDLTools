; Function to take a float and return a string with that value and no
; leading or trailing spaces valus above 9999.99 or so small that they
; would be 0.00 are output in exp function. Integers are return as integers
FUNCTION f2s,f,p2,deci=deci
  ;Check params. with special attention for possible integer values
  CASE n_params() OF 
     0: return,''
     1: BEGIN
        IF (n_elements(deci) EQ 0) THEN BEGIN
           ;; Is it integer?
           s = size(f)
           IF (total(s(s[0]+1) EQ [1,2,3,12,13,14,15]) NE 0) AND (abs(f) LT 1d5) THEN $
              return,n2s(f)
           deci = 2
        ENDIF
     END
     2: deci=p2
  ENDCASE
  dec = floor(deci)
  
  f = double(f)
  
;; Allow for an array of input values by looping over the f's
;; store the outputs
  s = make_array(n_elements(f),value='')
  
  FOR idx=0,n_elements(f)-1 DO BEGIN
     CASE 1 OF 
        
        ;; If the input value equals 0 then simply return the 
        (abs(double(f[idx])) LE (machar(/double)).xmin): BEGIN
           s[idx] = string(f[idx],format='(F'+n2s(2+dec)+'.'+n2s(dec)+')')
        END

        (dec EQ 0): BEGIN
           s[idx] = n2s(round(f[idx]))
        END
        
        ELSE: BEGIN
           ;; Now we get the magnitude of f, after rounding
           ;; therefore we add 0.5 after the last decimal place in case f is
           ;; bigger than (1d-1)^double(dec)
           IF f[idx] GT (1d-1)^double(dec) THEN BEGIN 
              mag_f = floor(alog10(abs(f[idx])+0.5d0*(1d-1)^double(dec)))
           ENDIF ELSE BEGIN
              mag_f = floor(alog10(abs(f[idx])))
           ENDELSE
      
           ;; Now we have 3 possibilities:
           ;; 1) the number is too large -> make an exponent
           ;; 2) the number would round to 0 -> make an exponent
           ;; 3) make a float number
           ;; 1) and 2 have the same format so catch them in the same case
           ;; statement:
           CASE (1) OF
              (mag_f GE 5) OR (mag_f LT -dec): BEGIN
                 ;; How many characters needed for exponent (minimum is 2)?
                 width_exp = (floor(alog10(abs(mag_f)))+1)>2
                 ;; will look like (-)a.bbE-cc
                 ;;            sign    a . bb  E - cc
                 frm='(E'+n2s((f[idx] LT 0)+1+1+dec+1+1+width_exp)+'.'+n2s(dec)+'E'+n2s(width_exp)+')' 
              END
              ELSE: BEGIN
                 ;;            sign     before   . after 
                 frm='(F'+n2s((f[idx] LT 0)+((mag_f>0)+1)+1+dec)+'.'+n2s(dec)+')' 
              END
           ENDCASE
           s[idx] = string(format=frm,f)
        END
     ENDCASE
  ENDFOR
  
  return,s
END
