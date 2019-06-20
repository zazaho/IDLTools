;; (SH Apr  6 2000)
;; This function take an aar and defines an array with flatfield
;; methods for each line present in the aar. The method for each line
;; is at the index of the line. So if out is the resulting array then
;; out[7] is the method to use for line 7
;; The mothode are as follows:
;; -1: do not do anything (flux level to low (<2 Jy)
;;  0; do an offset. (2<F<20) dominated by noise and dark errors
;;  1; do a scaling (F>20) 
FUNCTION sh_def_ffmethod,a
  line = sh_uniq(a.data.line)
  met = make_array(max(line)+1,value=-1)
  FOR i = 0,n_elements(line)-1 DO BEGIN
    md = median((sh_select(a,a.data.line EQ line[i])).data.flux)
    CASE 1 OF
      (md LT 2):  met(line[i]) = -1
      (md LT 30): met(line[i]) = 0
      ELSE: met(line[i]) = 1
    END
  ENDFOR
  return,met
END
