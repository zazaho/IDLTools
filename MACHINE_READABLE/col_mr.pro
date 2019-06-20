;+
; NAME:
;  col_mr
;
; PURPOSE:
;  return a column from a machine readable data structure read with read_fmr
;
; CATEGORY:
;  MR manipulation
;
; CALLING SEQUENCE:
;  out = col_mr(struct,col[,help=help,select=select,_extra=_extra])
;
; INPUTS:
;  struct: idl structure with the data
;  col [STRING or INT]:  column to return
;
; KEYWORD PARAMETERS:
;  help: if set print the help screen
;  select: if set call select_mr before selecting the column.
;          use the extra keywords to apply the criterion
;
; EXAMPLE:
;  J = col_mr(all,'Jmag')
;  K = col_mr(all,'Kmag')
;  plot,j-k,j,ps=1
;
; MODIFICATION HISTORY:
;  (SH Aug 12 2005) Version 1.0
;-

FUNCTION col_mr,mr,col,help=help,select=select,_extra=_extra
  
  default,col,0
  
  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'col_mr'
      return,0
  ENDIF
  
  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return,0
  ENDIF

  IF keyword_set(select) THEN BEGIN
     work_mr = select_mr(mr,_extra=_extra)
     if size(work_mr,/tname) NE 'STRUCT' then return,0
  ENDIF ELSE BEGIN
     work_mr = mr
  ENDELSE

  IF size(col,/tname) EQ 'STRING' THEN BEGIN
      icol = (where(strcmp(work_mr.name,col,/fold_case) EQ 1,cnt))[0]
      IF cnt EQ 0 THEN BEGIN
          message,col+' does not exist', $
                  /informational
          return,0
      ENDIF
  ENDIF ELSE BEGIN
      col=fix(col[0])
      IF (col LT 0) OR (col GE n_tags(work_mr.data)) THEN BEGIN
          message,n2s(col)+' is out of range, valid range: [0,'+ $
                  n2s(n_tags(work_mr.data)-1)+']', $
                  /informational
          return,0
      ENDIF
      icol = col
  ENDELSE

  return,work_mr.data.(icol)
END
    
