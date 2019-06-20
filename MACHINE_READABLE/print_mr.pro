;+
; NAME:
;  print_mr
;
; PURPOSE:
;  print data from a machine readable data structure read with read_fmr
;
; CATEGORY:
;  IO
;
; CALLING SEQUENCE:
;  print_mr,struct
;
; INPUTS:
;  struct idl structure with the data
;
; KEYWORD PARAMETERS:
;  constraint=constraint: (array of) string of the form:
;   'IRASNAME eq "22272+5050"' 'F25 gt 250.'
;   Where IRASNAME and F25 are names of columns as listed in
;   struct.name
;  index=index: index of the rows to be printed
;  out=out: (array of) string or number indicating the column names or
;   numbers to print on output. E.g.:
;   out=['IRASNAME','F60'] or out=[0,2,3]
;  lun=lun: the logical unit number to print to if not given print to
;   the screen
;  help: if set print the help screen
;
; EXAMPLE:
;  open,lun_file1,'selected_data.txt',/get_lun
;  print_mr,struct,lun=lun_file1,out=[0,2]
;  close,lun_file1
;  $more selected_data.txt
;
; MODIFICATION HISTORY:
;  (SH Nov 18 2003) First version
;  (SH Oct  7 2005) Added the index keyword, fixed the bug that I
;  introduced
;-


PRO print_mr,mr,constraint=constraint,out=out,lun=lun, $
             index=index,help=help

  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'print_mr'
      return
  ENDIF
  
  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return
  ENDIF

  IF NOT keyword_set(lun) THEN lun=-1

  irow = make_array(n_elements(mr.data.(0)),val=1)
  
  IF keyword_set(index) THEN BEGIN 
      irow = irow*0
      irow[index] = 1
  ENDIF
  
  IF keyword_set(constraint) THEN BEGIN
      FOR i=0,n_elements(constraint)-1 DO BEGIN
          cc = constraint[i]
          col_name = strsplit(cc,' =',/extract)

          ;; find the column number
          col = (where(strcmp(mr.name,col_name[0],/fold_case) EQ 1,cnt))[0]
          IF cnt EQ 0 THEN BEGIN
              message,col_name[0]+' does not exist', $
                      /informational
          ENDIF ELSE BEGIN

              ;; This is a special case that we define to find
              ;; substrings
              IF strupcase(col_name[1]) EQ 'CONTAINS' THEN BEGIN
                  idx = (strpos(mr.data.(col),col_name[2]) NE -1)
              ENDIF ELSE BEGIN
                  condition = strmid(cc,strlen(col_name[0]))
                  foo = execute('idx=mr.data.(col)'+condition)
              ENDELSE
              irow = irow AND idx
          ENDELSE
      ENDFOR
  ENDIF


  irow = where(irow NE 0,cnt)
  IF cnt EQ 0 THEN BEGIN
      message,/informational, $
              'There are no records where the constraints are met'
      return
  ENDIF
  
  icol=-1
  IF n_elements(out) NE 0 THEN BEGIN
      
      IF size(out,/tname) EQ 'STRING' THEN BEGIN
          FOR i=0,n_elements(out)-1 DO BEGIN
              col = (where(strcmp(mr.name,out[i],/fold_case) EQ 1,cnt))[0]
              IF cnt EQ 0 THEN BEGIN
                  message,out[i]+' does not exist', $
                          /informational
              ENDIF ELSE BEGIN
                  icol=[icol,col]
              ENDELSE
          ENDFOR
      ENDIF ELSE BEGIN
          n_cols = n_tags(mr.data)
          col = where(out LT n_cols,cnt)
          IF cnt NE n_elements(out) THEN BEGIN
              message,'maximum col#='+n2s(n_cols-1), $
                          /informational
          ENDIF
          IF cnt NE 0 THEN BEGIN
              icol=[-1,out[col]]
          ENDIF
      ENDELSE
  ENDIF
  
  IF n_elements(icol) GE 2 THEN BEGIN
      icol =icol[1:*]
  ENDIF ELSE BEGIN
      icol = indgen(n_tags(mr.data))
  ENDELSE 
  
  ;;we need this for better formatting
  width_col= make_array(n_elements(icol),value='')
  FOR i=0,n_elements(width_col)-1 DO BEGIN
      width_col[i] = n2s(max(strlen(string((mr.data.(icol[i]))[irow])))+2)
  ENDFOR

  printf,lun,format='($,A5)','#Row#'
  FOR i=0,n_elements(icol)-1 DO BEGIN
      printf,lun,format='($,A'+width_col[i]+')',mr.name[icol[i]]
  ENDFOR
  printf,lun,''

  printf,lun,format='($,A5)','#    '
  FOR i=0,n_elements(icol)-1 DO BEGIN
      printf,lun,format='($,A'+width_col[i]+')',mr.unit[icol[i]]
  ENDFOR
  printf,lun,''

  FOR j=0,n_elements(irow)-1 DO BEGIN
      printf,lun,format='($,I5)',irow[j]
      FOR i=0,n_elements(icol)-1 DO BEGIN
          printf,lun,format='($,A'+width_col[i]+')',(mr.data.(icol[i]))[irow[j]]
      ENDFOR
      printf,lun,''
  ENDFOR
  
END
