;+
; NAME:
;  add_header_mr
;
; PURPOSE:
;  Add some strings to the header of an MR
;
; CATEGORY:
;  MR manipulation
;
; CALLING SEQUENCE:
;  out = add_header_mr(mr,header)
;
; INPUTS:
;  mr: idl structure with the structure to be updated
;  header: string (array) to be added to the end of the header
;
; KEYWORD PARAMETERS:
;  replace: if set then replace the header in stead of adding to it
;  help: if set print the help screen
;
; EXAMPLE:
;  temp = add_header_mr(all,"removed spurious sources")
;
; MODIFICATION HEADER:
;   (SH Jul 20 2005) First version
;-

PRO add_header_mr,mr,header,replace=replace,help=help

  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      help = 1
  ENDIF
  
  IF size(header,/tname) NE 'STRING' THEN BEGIN
      message,'the input header is not valid',/inform
      help = 1
  ENDIF

;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'add_history_mr'
      return
  ENDIF

  tagnames = strupcase(tag_names(mr))
  
  exec = 'updated = {'
  FOR i=0,n_tags(mr)-1 DO BEGIN
      IF tagnames[i] NE 'HEADER' THEN BEGIN
          exec = exec+tagnames[i]+':mr.'+tagnames[i]+','
      ENDIF ELSE BEGIN
          IF keyword_set(replace) THEN BEGIN
              exec = exec+tagnames[i]+':[header],'
          ENDIF ELSE BEGIN
              exec = exec+tagnames[i]+':[mr.header,header],'
          ENDELSE
      ENDELSE
  ENDFOR
  exec = strmid(exec,0,strlen(exec)-1)+'}'
  foo = execute(exec)
  mr = updated
END
