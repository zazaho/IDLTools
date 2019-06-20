;+
; NAME: match_arrays
;
; PURPOSE: given two arrays (a1,a2) it will return the position of the
; first occurence of matching value of the elements of a1 in a2 and -1
; if it does not exist 
;
; CALLING SEQUENCE:
; ia2_into_a1 = match_arrays(a1,a2)
;
; INPUTS:
; a1: array
; a2: array
;
; OPTIONAL INPUTS:
; m0,m1,..,m9 array of dimension a2 that will be sorted according to
; the matches found and the missing elements are left to empty or NaN;
;
; KEYWORD INPUTS:
; help: if set print this help
; 
; OUTPUTS:
; array of dimension of a1 with  indices pointing to matching elements
; in a2
;
; SIDE EFFECTS:
; The arrays m0,m9 (optional ten arguments) are sorted to match a1
;
; EXAMPLE:
;a = [1,2,3,4]
;b = [0,1,1,5,4]
;names = ['aa','bc','cb','f','gh']
;idx = match_arrays(a,b,names)
;print,a[where(idx NE -1)]
;print,b[idx[where(idx NE -1)]]
;FOR i=0,n_elements(a)-1 DO print,names[i] 
;
; MODIFICATION HISTORY:
;(SH Nov 28 2003) first created
;-

FUNCTION match_arrays,a1,a2,m0,m1,m2,m3,m4,m5,m6,m7,m8,m9, $
  help=help
  

  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'match_arrays'
      return,0
  ENDIF
  

  na1 = n_elements(a1)
  na2 = n_elements(a2)

  ;; the values to use for missing points accoring to type

  zero_values={zeros, $
               undef:0, $
               byte:0B, $
               int:0, $
               long:0L, $
               float:!VALUES.F_NAN, $
               double:!VALUES.D_NAN, $
               complex:complex(0,0), $
               string:'', $
               struct:{nothing,null:0}}
  
  idx = make_array(na1,val=-1)
  FOR i=0,na1-1 DO idx[i] = (where(a1[i] EQ a2))[0]
  
  IF n_elements(m0) EQ na2 THEN begin
      mm0=make_array(n_elements(a1),value=zero_values.(size(m0,/type)))
      mm0[where(idx NE -1)] = m0[idx[where(idx NE -1)]]
      m0 = mm0
  ENDIF

  IF n_elements(m1) EQ na2 THEN begin
      mm1=make_array(n_elements(a1),value=zero_values.(size(m1,/type)))
      mm1[where(idx NE -1)] = m1[idx[where(idx NE -1)]]
      m1 = mm1
  ENDIF

  IF n_elements(m2) EQ na2 THEN begin
      mm2=make_array(n_elements(a1),value=zero_values.(size(m2,/type)))
      mm2[where(idx NE -1)] = m2[idx[where(idx NE -1)]]
      m2 = mm2
  ENDIF

  IF n_elements(m3) EQ na2 THEN begin
      mm3=make_array(n_elements(a1),value=zero_values.(size(m3,/type)))
      mm3[where(idx NE -1)] = m3[idx[where(idx NE -1)]]
      m3 = mm3
  ENDIF

  IF n_elements(m4) EQ na2 THEN begin
      mm4=make_array(n_elements(a1),value=zero_values.(size(m4,/type)))
      mm4[where(idx NE -1)] = m4[idx[where(idx NE -1)]]
      m4 = mm4
  ENDIF

  IF n_elements(m5) EQ na2 THEN begin
      mm5=make_array(n_elements(a1),value=zero_values.(size(m5,/type)))
      mm5[where(idx NE -1)] = m5[idx[where(idx NE -1)]]
      m5 = mm5
  ENDIF

  IF n_elements(m6) EQ na2 THEN begin
      mm6=make_array(n_elements(a1),value=zero_values.(size(m6,/type)))
      mm6[where(idx NE -1)] = m6[idx[where(idx NE -1)]]
      m6 = mm6
  ENDIF

  IF n_elements(m7) EQ na2 THEN begin
      mm7=make_array(n_elements(a1),value=zero_values.(size(m7,/type)))
      mm7[where(idx NE -1)] = m7[idx[where(idx NE -1)]]
      m7 = mm7
  ENDIF

  IF n_elements(m8) EQ na2 THEN begin
      mm8=make_array(n_elements(a1),value=zero_values.(size(m8,/type)))
      mm8[where(idx NE -1)] = m8[idx[where(idx NE -1)]]
      m8 = mm8
  ENDIF

  IF n_elements(m9) EQ na2 THEN begin
      mm9=make_array(n_elements(a1),value=zero_values.(size(m9,/type)))
      mm9[where(idx NE -1)] = m9[idx[where(idx NE -1)]]
      m9 = mm9
  ENDIF


  RETURN,idx
END
