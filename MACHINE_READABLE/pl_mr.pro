;; little routine to plot machine readable data structures read in
;; with read_frm
;;
;; The structure looks like:
;;   out = {type:'mr_structure',name:labels,unit:units,description:descriptions,data:data}
;; where data contains field0, field1 etc the data of the columns

;; utility function to ask for which column
FUNCTION pl_mr_ask_column,mr
  ans = 0

  max_name = strtrim(string(max(strlen(mr.name))),2)

  FOR i=0,n_elements(mr.name)-1 DO $
        print,string(format='(I4," ",A'+max_name+'," - ",A)',i $
                     ,mr.name[i],mr.description[i])
  print,'PL_MR: Which column would you like plotted'
  read,ans
  return,ans
END

PRO pl_mr,mr,c1,c2,return=return,_extra=_extra

  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return
  ENDIF

  IF n_elements(c1) NE 0 THEN BEGIN
      c1=c1[0]
      IF size(c1,/tname) EQ 'STRING' THEN BEGIN
          c1 = (where(strcmp(mr.name,c1,/fold_case) EQ 1,cnt))[0]
          IF cnt EQ 0 THEN BEGIN
              print,'PL_MR: '+c1+' does not exist'
              c1 = pl_mr_ask_column(mr)
              d1 = mr.data.(c1)
          ENDIF ELSE BEGIN
              d1=mr.data.(c1)
          ENDELSE
      ENDIF ELSE BEGIN
          IF c1 LT n_tags(mr.data) THEN d1 = mr.data.(c1)
      ENDELSE
  ENDIF
 
  IF n_elements(c2) NE 0 THEN BEGIN
      c2=c2[0]
      IF size(c2,/tname) EQ 'STRING' THEN BEGIN
          c2 = (where(strcmp(mr.name,c2,/fold_case) EQ 1,cnt))[0]
          IF cnt EQ 0 THEN BEGIN
              print,'PL_MR: '+c2+' does not exist'
              c2 = pl_mr_ask_column(mr)
              d2 = mr.data.(c2)
          ENDIF ELSE BEGIN
              d2=mr.data.(c2)
          ENDELSE
      ENDIF ELSE BEGIN
          IF c2 LT n_tags(mr.data) THEN d2 = mr.data.(c2)
      ENDELSE
  ENDIF

  has_d1 = (n_elements(d1) NE 0)
  has_d2 = (n_elements(d2) NE 0)
  
  CASE 1 OF 
      (has_d1 AND has_d2): BEGIN
          pl,d1,d2,xtit=mr.name[c1]+' ['+mr.unit[c1]+']', $
             ytit=mr.name[c2]+' ['+mr.unit[c2]+']', $
             return=return,_extra=_extra
      END
      (has_d1 AND (NOT has_d2)): BEGIN
          pl,d1,ytit=mr.name[c1]+' ['+mr.unit[c1]+']', $
             xtit='record number', $
             return=return,_extra=_extra
      END
      ELSE: BEGIN
          c1 = pl_mr_ask_column(mr)
          d1 = mr.data.(c1)
          pl,d1,ytit=mr.name[c1]+' ['+mr.unit[c1]+']', $
             xtit='record number', $
             return=return,_extra=_extra
      END
  ENDCASE
END

