FUNCTION select_mr_multisort,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9

  npars = n_params()
  idx = sort(p0)

  IF npars GT 1 THEN BEGIN
      p0sort = p0[idx]

      ;; extract boundaries of equal values in major index
      idx_uniq_p0 = [-1,uniq(p0sort)]

      ;; This means there are some duplicates
      IF n_elements(idx_uniq_p0) LE n_elements(p0) THEN BEGIN

          ;; sort all elements according to idx
          FOR i=1,npars-1 DO BEGIN
              foo = execute('p'+n2s(i)+'sort = p'+n2s(i)+'[idx]')
          ENDFOR

          ;; loop over each piece of same values
          FOR i=0,n_elements(idx_uniq_p0)-2 DO BEGIN 
              ;; if more than 1 element in this piece
              IF (idx_uniq_p0[i]+1) LT idx_uniq_p0[i+1] THEN BEGIN
                  ;; multisort the rest
                  recurs_command='idx_subset_p1=select_mr_multisort(subset_p1sort'
                  subset_p1sort=p1sort[idx_uniq_p0[i]+1:idx_uniq_p0[i+1]]
                  FOR j=2,npars-1 DO BEGIN
                      ;;only take the relevant part of the rest of the params
                      foo=execute('subset_p'+n2s(j)+'sort= p'+n2s(j)+'sort[idx_uniq_p0[i]+1:idx_uniq_p0[i+1]]')
                      recurs_command=recurs_command+',subset_p'+n2s(j)+'sort'
                  ENDFOR
                  recurs_command=recurs_command+')'
                  foo=execute(recurs_command)
                  ;; resort the subindex with the equal values
                  idx[idx_uniq_p0[i]+1:idx_uniq_p0[i+1]] = $
                    (idx[idx_uniq_p0[i]+1:idx_uniq_p0[i+1]])[idx_subset_p1]
              ENDIF
          ENDFOR
      ENDIF
  ENDIF
  return,idx
END

;+
; NAME:
;  select_mr
;
; PURPOSE:
;  select data from a machine readable data structure read with read_fmr
;
; CATEGORY:
;  MR manipulation
;
; CALLING SEQUENCE:
;  out = select_mr(struct[,constraint=cons,index=index,
;                          where=where,remove=remove,help=help])
;
; INPUTS:
;  struct idl structure with the data
;
; KEYWORD PARAMETERS:
;  remove=remove: if set then remove the matching columns instead of
;   keeping them
;  constraint=constraint: (array of) string of the form:
;   'IRASNAME eq "22272+5050"' 'F25 gt 250.' 'IRASNAME contains "+"'
;   Where IRASNAME and F25 are names of columns as listed in
;   struct.name
;  index=index: array of 0 and 1 to match which records to keep (remove)
;  where=where: (array of) records to keep (remove) 
;  sort=sort: STRING or INT indicating the column to sort the result by
;  help: if set print the help screen
;
; EXAMPLE:
;  bright = select_mr(all,constraint='f12 ge 100.')
;  pl_mr,bright,'f12'
;
; MODIFICATION HISTORY:
;  (SH Nov 19 2003) First version
;-


FUNCTION select_mr,mr, $
                   constraint=constraint, $
                   index=index,where=where, $
                   remove=remove,help=help, $
                   sort=srt
  
  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'select_mr'
      return,0
  ENDIF
  
  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return,0
  ENDIF

  ;; the index which keeps track of which records match the conditions
  irow = make_array(n_elements(mr.data.(0)),val=1)

  
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
  
  IF keyword_set(index) THEN BEGIN
      IF n_elements(index) NE n_elements(irow) THEN BEGIN
          message,'index array has wrong number of elements', $
                  /informational
      ENDIF ELSE BEGIN
          irow = irow AND index
      ENDELSE 
  ENDIF
  
  IF n_elements(where) NE 0 THEN BEGIN
      iwhere = LONG(where)
      invalid = (total((iwhere LT 0L) OR (iwhere GE n_elements(irow))) NE 0)
      IF invalid THEN BEGIN
          message,'where array has out of range elements', $
                  /informational
          stop
      ENDIF ELSE BEGIN
          idx = irow*0
          idx[iwhere] = 1
          irow = irow AND idx
      ENDELSE 
  ENDIF
  
  
  IF keyword_set(remove) THEN BEGIN
      irow = where(irow EQ 0,cnt)
  ENDIF ELSE BEGIN
      irow = where(irow NE 0,cnt)
  ENDELSE

  IF cnt EQ 0 THEN BEGIN
      message,/informational, $
              'There are no records left!'
      return,0
  ENDIF

  IF keyword_set(srt) THEN BEGIN
      isrts=0
      IF size(srt,/tname) EQ 'STRING' THEN BEGIN
          FOR i=0,n_elements(srt)-1 DO BEGIN
              isrt = (where(strcmp(mr.name,srt[i],/fold_case) EQ 1,cnt))[0]
              IF cnt EQ 0 THEN BEGIN
                  message,'Column: '+srt[i]+' does not exist', $
                          /informational
              ENDIF ELSE BEGIN
                  isrts = [isrts,isrt]
              ENDELSE
          ENDFOR
      ENDIF ELSE BEGIN
          isrt=fix(srt)
          FOR i=0,n_elements(isrt)-1 DO BEGIN
              IF (isrt[i] LT 0) OR (isrt[i] GE n_tags(mr.data)) THEN BEGIN
                  message,n2s(isrt[i])+' is out of range, valid range: [0,'+ $
                          n2s(n_tags(mr.data)-1)+']', $
                          /informational
              ENDIF ELSE BEGIN
                  isrts = [isrts,isrt[i]]
              ENDELSE
          ENDFOR
      ENDELSE

      isrts = isrts[1:*]

      CASE n_elements(isrts) OF
          0: BEGIN
          END
          ELSE: BEGIN
              sort_command = '(mr.data.(isrts[0]))[irow]'
              FOR i=1,n_elements(isrts)-1 DO BEGIN
                  sort_command=sort_command+',(mr.data.(isrts['+n2s(i)+']))[irow]'
              ENDFOR
              foo = execute('irow_sort=select_mr_multisort('+ $
                            sort_command+')')
              irow = irow[irow_sort]
          END
      ENDCASE
  ENDIF
  
  ;; Now we select only these records that we wish
  column_names=tag_names(mr.data)
  exec = 'data={'
  FOR i=0,n_elements(column_names)-1 DO $
     exec=exec+column_names[i]+':(mr.data.'+column_names[i]+')[irow],'
  exec = strmid(exec,0,strlen(exec)-1)+'}'
  foo = execute(exec)

  header_names = tag_names(mr)
  header_names = header_names[where(header_names NE 'DATA')]
  exec = 'out={'
  FOR i=0,n_elements(header_names)-1 DO $
        exec=exec+header_names[i]+':mr.'+header_names[i]+','
  exec = exec+'data:data}'
  foo = execute(exec)
  
  return,out

END
