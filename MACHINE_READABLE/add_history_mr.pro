;+
; NAME:
;  add_history_mr
;
; PURPOSE:
;  Add some strings to the history of an MR
;
; CATEGORY:
;  MR manipulation
;
; CALLING SEQUENCE:
;  out = add_history_mr(mr,history)
;
; INPUTS:
;  mr: idl structure with the structure to be updated
;  history: string (array) to be added to the end of the history
;
; KEYWORD PARAMETERS:
;  help: if set print the help screen
;
; EXAMPLE:
;  temp = add_history_mr(all,"removed spurious sources")
;
; MODIFICATION HISTORY:
;   (SH Jul 20 2005) First version
;-

PRO add_history_mr,mr,history,help=help

  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      help = 1
  ENDIF
  
  IF NOT (size(history,/tname) EQ 'STRING') THEN BEGIN
      message,'the input history is not valid',/inform
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
      IF tagnames[i] NE 'HISTORY' THEN BEGIN
          exec = exec+tagnames[i]+':mr.'+tagnames[i]+','
      ENDIF ELSE BEGIN
          exec = exec+tagnames[i]+':[mr.history,history],'
      ENDELSE
  ENDFOR
  exec = strmid(exec,0,strlen(exec)-1)+'}'
  foo = execute(exec)
  mr = updated
END
