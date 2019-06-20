;+
; NAME:
;  select_col_mr.pro
;
; PURPOSE:
;  Given a machine readable table and columns, this FUNCTION returns a 
;  machine readable table containing the requested columns
;
; CALLING SEQUENCE:
;  f = select_col_mr(mr_struct,columns)
;
; INPUTS:
;  struct:  [mr_structure]: the structure that holds the data and
;  columns: [(array of) integers of strings] of column(s) to be preserved
;
; KEYWORD PARAMETERS:
; -help: if set show the help
;
; EXAMPLE:
;  foo = select_col_mr(smith_data['name','ra'])
;
; MODIFICATION HISTORY:
;  Version 1:
;  (SH Aug  9 2005) Written by Sacha Hony
;-

FUNCTION select_col_mr,mr,columns,help=help

  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'select_col_mr'
      return,0
  ENDIF
  
  
  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return,0
  ENDIF

  IF NOT keyword_set(columns)  THEN BEGIN
      message,'Please supply the columns to return',/inform
      return,0
  ENDIF

;; Now convert the columns to the respective indices
  coltype = size(columns,/tname)
  ncol = n_elements(columns)

  IF coltype EQ 'STRING' THEN BEGIN
      col_idx = -1
      nms = strcompress(strupcase(mr.name),/remove_all)
      cls = strcompress(strupcase(columns),/remove_all)
      FOR i=0,ncol-1 DO BEGIN
          idx = where(cls[i] EQ nms,cnt)
          IF cnt EQ 0 THEN BEGIN
              message,'Column:'+columns[i]+' does not exist',/inform
          ENDIF ELSE BEGIN
              ;; This allows multiple matches that should of course
              ;; not occur
              ;; write a warning
              IF n_elements(idx) GT 1 THEN message,/inform,"Multiple columns have the name: "+columns[i]
              col_idx = [col_idx,idx]
          ENDELSE
      ENDFOR
      IF n_elements(col_idx) EQ 1 THEN BEGIN
          message,'The specified columns do not exist in the MR table',/inform
          return,0
      ENDIF
      col_idx = col_idx[1:*]
  ENDIF ELSE BEGIN
      max_col = n_elements(mr.name)
      valid=where((columns GE 0) AND (columns LT max_col),cnt)
      CASE 1 OF 
          (cnt EQ 0): BEGIN
              message,'None of the specified columns exist',/inform
              return,0
          END
          (cnt EQ ncol): BEGIN
              col_idx = columns
          END
          ELSE: BEGIN
              message,'Some of the specified columns do no exist',/inform
              message,'Columns:'+string(columns[where((columns LT 0) OR (columns GE max_col))]),/inform
              col_idx = columns[valid]
          END
      ENDCASE
  ENDELSE
  
  ncol = n_elements(col_idx)
  names = mr.name[col_idx]
  units = mr.unit[col_idx]
  descriptions = mr.description[col_idx]
  IF tag_exist(mr,'HEADER') THEN header = mr.header ELSE header = [""]
  IF tag_exist(mr,'HISTORY') THEN history = mr.history ELSE history = [""]
  
  C0 = mr.data.(col_idx[0])
  exec_string = 'data={C0:C0'
  
  FOR i=1,ncol-1 DO BEGIN
      foo = execute('C'+ strcompress(string(i),/remove_all)+ $
                    '=mr.data.(col_idx['+ $
                    strcompress(string(i),/remove_all)+'])')
      exec_string = exec_string+ $
                    ',C'+strcompress(string(i),/remove_all)+ $
                    ':C'+strcompress(string(i),/remove_all)
  ENDFOR
  exec_string=exec_string+'}'
  foo = execute(exec_string)
  
  out = {type:'mr_structure', $
         name:names, $
         unit:units, $
         description:descriptions, $
         header:header, $
         history:history, $
         data:data}
  return,out
  
END
