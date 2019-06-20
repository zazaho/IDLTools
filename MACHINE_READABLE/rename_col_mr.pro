;+
; NAME:
;  rename_col_mr
;
; PURPOSE:
;  Change names, units or descriptions in an MR
;
; CATEGORY:
;  MR manipulation
;
; CALLING SEQUENCE:
;  rename_col_mr,mr[,oldname=on,newname=nn,
;                    oldunit=ou,newunit=nu,
;                    olddescription=od,newdescription=nd,
;                    /casesensitive,
;                    /help]
;
; INPUTS:
;  mr: idl structure with the structure to be updated
;
; KEYWORD PARAMETERS:
;  oldname=on       : [string|integer] (array of) names to replace
;  newname=nn,      : [string|integer] (array of) replacement names
;  oldunit=ou       : [string|integer] (array of) units to replace 
;  newunit=nu       : [string|integer] (array of) replacement units
;  olddescription=od: [string|integer] (array of) descriptions to replace 
;  newdescription=nd: [string|integer] (array of) replacement descriptions
;  /casesensitive   : if set do string comparisons case sensitive
;  /help            : if set show this help
;
; EXAMPLE:
;  rename_col_mr,all,oldname=["mag","flag"],newname=["K","Kflag"]
;
; MODIFICATION HEADER:
;   (SH Aug 24 2005) First version
;-


PRO rename_col_mr,struct, $
                  oldname=oldname,newname=newname, $
                  oldunit=oldunit,newunit=newunit, $
                  olddescription=olddesc,newdescription=newdesc, $
                  casesensitive=csense,help=help

;; define the default input values
  default,oldname,""
  default,newname,""
  default,oldunit,""
  default,newunit,""
  default,olddesc,""
  default,newdesc,""
  default,casesensitive,0
  default,help,0

  ;; check the inputs
  IF NOT is_mr(struct) THEN BEGIN
      message,'the input structure is not valid',/inform
      help = 1
  ENDIF
  
  ncol = n_elements(struct.name)
  
  noldname =   n_elements(oldname)
  nnewname =   n_elements(newname)
  noldunit =   n_elements(oldunit)
  nnewunit =   n_elements(newunit)
  nolddesc =   n_elements(olddesc)
  nnewdesc =   n_elements(newdesc)

;; the new and old arrays must have the same size otherwise quit
  IF noldname NE nnewname THEN BEGIN
      message,'The new and old name arrays should have the same dimensions',/inform
      help = 1
  ENDIF

  IF noldunit NE nnewunit THEN BEGIN
      message,'The new and old unit arrays should have the same dimensions',/inform
      help = 1
  ENDIF

  IF nolddesc NE nnewdesc THEN BEGIN
      message,'The new and old description arrays should have the same dimensions',/inform
      help = 1
  ENDIF

; make sure they also have the right types
; all new arrays must be strings
  IF size(newname,/tname) NE 'STRING' THEN BEGIN
      message,'The newname array must contain strings',/inform
      help = 1
  ENDIF
  IF size(newunit,/tname) NE 'STRING' THEN BEGIN
      message,'The newunit array must contain strings',/inform
      help = 1
  ENDIF
  IF size(newname,/tname) NE 'STRING' THEN BEGIN
      message,'The newdescription array must contain strings',/inform
      help = 1
  ENDIF

; all old arrays must be strings or integers
  nametype = size(oldname,/tname)
  unittype = size(oldunit,/tname)
  desctype = size(olddesc,/tname)

  IF total(nametype EQ ['STRING','INT']) EQ 0 THEN BEGIN
      message,'The oldname array must contain strings or integers',/inform
      help = 1
  ENDIF
  IF total(unittype EQ ['STRING','INT']) EQ 0 THEN BEGIN
      message,'The oldunit array must contain strings or integers',/inform
      help = 1
  ENDIF
  IF total(desctype EQ ['STRING','INT']) EQ 0 THEN BEGIN
      message,'The olddescription array must contain strings or integers',/inform
      help = 1
  ENDIF

;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'rename_col_mr'
      stop
      return
  ENDIF

;; Now do the real work

;; First the names
;; convert strings in the oldxxxx to indices
  IF nametype EQ 'STRING' THEN BEGIN
      IF total(oldname NE "") NE 0 THEN BEGIN
          idx_found = -1
          IF NOT keyword_set(casesensitive) THEN BEGIN
              tmpname = strupcase(struct.name)
              tmpoldname = strupcase(oldname)
          ENDIF ELSE BEGIN
              tmpname = struct.name
              tmpoldname = oldname
          ENDELSE
          
          tmpname = strtrim(tmpname,2)
          tmpoldname = strtrim(tmpoldname,2)
          
          FOR i=0,noldname-1 DO BEGIN
              idx = where(tmpoldname[i] EQ tmpname,cnt)
              IF cnt EQ 0 THEN BEGIN
                  message,'Name:'+oldname[i]+' does not exist',/inform
                  idx_found = [idx_found,-1]
              ENDIF ELSE BEGIN
                  ;; This allows multiple matches that should of course
                  ;; not occur
                  ;; write a warning
                  IF n_elements(idx) GT 1 THEN message,/inform,"Multiple columns match the name: "+oldname[i]
                  idx_found = [idx_found,idx[0]]
              ENDELSE
          ENDFOR
          IF n_elements(idx_found) EQ 1 THEN BEGIN
              message,'The specified oldnames do not exist in the MR table',/inform
          ENDIF
          idx_found = idx_found[1:*]
      ENDIF ELSE BEGIN
          idx_found = -1
      ENDELSE
  ENDIF ELSE BEGIN
      valid=where((oldname GE 0) AND (oldname LT ncol),cnt)
      CASE 1 OF 
          (cnt EQ 0): BEGIN
              message,'None of the specified oldnames exists',/inform
              return
          END
          (cnt EQ noldname): BEGIN
              idx_found = oldname
          END
          ELSE: BEGIN
              message,'Some of the specified oldnames do no exist',/inform
              message,'Oldnames:'+string(oldname[where((oldname LT 0) OR (oldname GE ncol))]),/inform
              idx_found = oldname*0-1
              idx_found[valid] = oldname[valid]
          END
      ENDCASE
  ENDELSE 

;; Apply the changes to those elements that where matched in the
;; oldname array
  valid = where(idx_found NE -1,cnt)
  IF cnt NE 0 THEN BEGIN
      FOR i=0,cnt-1 DO BEGIN
          struct.name[idx_found[valid[i]]] = newname[valid[i]]
      ENDFOR
  ENDIF
  
;; Now the units
;; convert strings in the oldxxxx to indices
  IF unittype EQ 'STRING' THEN BEGIN
      IF total(oldunit NE "") NE 0 THEN BEGIN
          idx_found = -1
          IF NOT keyword_set(casesensitive) THEN BEGIN
              tmpunit = strupcase(struct.unit)
              tmpoldunit = strupcase(oldunit)
          ENDIF ELSE BEGIN
              tmpunit = struct.unit
              tmpoldunit = oldunit
          ENDELSE
          
          tmpunit = strtrim(tmpunit,2)
          tmpoldunit = strtrim(tmpoldunit,2)
          
          FOR i=0,noldunit-1 DO BEGIN
              idx = where(tmpoldunit[i] EQ tmpunit,cnt)
              IF cnt EQ 0 THEN BEGIN
                  message,'Unit:'+oldunit[i]+' does not exist',/inform
                  idx_found = [idx_found,-1]
              ENDIF ELSE BEGIN
                  ;; This allows multiple matches that should of course
                  ;; not occur
                  ;; write a warning
                  IF n_elements(idx) GT 1 THEN message,/inform,"Multiple columns match the unit: "+oldunit[i]
                  idx_found = [idx_found,idx[0]]
              ENDELSE
          ENDFOR
          IF n_elements(idx_found) EQ 1 THEN BEGIN
              message,'The specified oldunits do not exist in the MR table',/inform
          ENDIF
          idx_found = idx_found[1:*]
      ENDIF ELSE BEGIN
          idx_found = -1
      ENDELSE
  ENDIF ELSE BEGIN
      valid=where((oldunit GE 0) AND (oldunit LT ncol),cnt)
      CASE 1 OF 
          (cnt EQ 0): BEGIN
              message,'None of the specified oldunits exists',/inform
              return
          END
          (cnt EQ noldunit): BEGIN
              idx_found = oldunit
          END
          ELSE: BEGIN
              message,'Some of the specified oldunits do no exist',/inform
              message,'Oldunits:'+string(oldunit[where((oldunit LT 0) OR (oldunit GE ncol))]),/inform
              idx_found = oldunit*0-1
              idx_found[valid] = oldunit[valid]
          END
      ENDCASE
  ENDELSE 

;; Apply the changes to those elements that where matched in the
;; oldunit array
  valid = where(idx_found NE -1,cnt)
  IF cnt NE 0 THEN BEGIN
      FOR i=0,cnt-1 DO BEGIN
          struct.unit[idx_found[valid[i]]] = newunit[valid[i]]
      ENDFOR
  ENDIF
  
;; convert strings in the oldxxxx to indices
  IF desctype EQ 'STRING' THEN BEGIN
      IF total(olddesc NE "") NE 0 THEN BEGIN
          idx_found = -1
          IF NOT keyword_set(casesensitive) THEN BEGIN
              tmpdesc = strupcase(struct.desc)
              tmpolddesc = strupcase(olddesc)
          ENDIF ELSE BEGIN
              tmpdesc = struct.desc
              tmpolddesc = olddesc
          ENDELSE
          
          tmpdesc = strtrim(tmpdesc,2)
          tmpolddesc = strtrim(tmpolddesc,2)
          
          FOR i=0,noldname-1 DO BEGIN
              idx = where(tmpolddesc[i] EQ tmpdesc,cnt)
              IF cnt EQ 0 THEN BEGIN
                  message,'Description:'+olddesc[i]+' does not exist',/inform
                  idx_found = [idx_found,-1]
              ENDIF ELSE BEGIN
                  ;; This allows multiple matches that should of course
                  ;; not occur
                  ;; write a warning
                  IF n_elements(idx) GT 1 THEN message,/inform,"Multiple columns match the description: "+olddesc[i]
                  idx_found = [idx_found,idx[0]]
              ENDELSE
          ENDFOR
          IF n_elements(idx_found) EQ 1 THEN BEGIN
              message,'The specified olddescriptions do not exist in the MR table',/inform
          ENDIF
          idx_found = idx_found[1:*]
      ENDIF ELSE BEGIN
          idx_found = -1
      ENDELSE
  ENDIF ELSE BEGIN
      valid=where((olddesc GE 0) AND (olddesc LT ncol),cnt)
      CASE 1 OF 
          (cnt EQ 0): BEGIN
              message,'None of the specified olddescriptions exists',/inform
              return
          END
          (cnt EQ nolddesc): BEGIN
              idx_found = olddesc
          END
          ELSE: BEGIN
              message,'Some of the specified olddescriptions do no exist',/inform
              message,'Olddescs:'+string(olddesc[where((olddesc LT 0) OR (olddesc GE ncol))]),/inform
              idx_found = olddesc*0-1
              idx_found[valid] = olddesc[valid]
          END
      ENDCASE
  ENDELSE 

;; Apply the changes to those elements that where matched in the
;; olddesc array
  valid = where(idx_found NE -1,cnt)
  IF cnt NE 0 THEN BEGIN
      FOR i=0,cnt-1 DO BEGIN
          struct.description[idx_found[valid[i]]] = newdesc[valid[i]]
      ENDFOR
  ENDIF

END
