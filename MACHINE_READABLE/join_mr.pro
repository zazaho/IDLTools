;+
; NAME:
;  join_fmr.pro
;
; PURPOSE:
;  Given two machine readable tables and a column of the same type in
;  each table this function makes a joined larger machine readable
;  structure that holds data from both where the elements of the two
;  columns match
;
; CATEGORY:
;  STRUCTURE MANIPULATION
;
; CALLING SEQUENCE:
;  large = join_mr(tab1,tabl [,col1[,col2]]|[index_array])
;
; INPUTS:
;  tab1,tab2: Machine readable tables.
;  col1,col2: columns from the tables that should be used to join the
;     two. Can be either INT or STRING. If col2 is not specified it is
;     assumed to be the same as col1
;  index_array = array [2,*] of indices that indicate which rows in
;    mr1 are matched with which rows in mr2. They take the form:
;    [[idx1,idx2],[-1,idx2],..]. -1 is used to indicate no match but the
;    row will still be included.
;
;
; KEYWORD PARAMETERS:
;  /keep1: if set keep the records from table 1 even if they are not matched
;  /keep2: if set keep the records from table 2 even if they are not matched
;  /keepall: if set keep the records from table 1 and 2 even if they are not matched
;  /allmatches: if set return an entry for each matching record in
;  table 2 i multiple matches are present not only the first matching record
;  /help: if set show the help
;
; OUTPUTS:
;  The ouput data structure will look like:
;    TYPE            STRING    'mr_structure'
;    NAME            STRING    Array[X]
;    UNIT            STRING    Array[X]
;    DESCRIPTION     STRING    Array[X]
;    DATA            STRUCT    -> <Anonymous> Array[1]
;  where name contains the names of each columns
;  unit contains the given units
;  description contains the short descriptions and
;  data holds the values of the separate columns are named, COLUMN0,
;  COLUMN1, ... , COLUMNX, where X stands for the total number of
;  columns read.
;
; EXAMPLE:
;  large = join_mr(tab1,tabl,'srcid','id')
;   
; MODIFICATION HISTORY:
;  Version 1:
;  (SH Jul 18 2005) 
;-

FUNCTION join_mr,mr1,mr2,col1,col2,index_array=iarr, $
                 keep1=keep1,keep2=keep2,keepall=keepall, $
                 allmatches=allmatches,help=help
  
  IF NOT is_mr(mr1) THEN BEGIN
      message,'the input structure #1 is not valid',/inform
      help = 1
  ENDIF

  IF NOT is_mr(mr2) THEN BEGIN
      message,'the input structure #2 is not valid',/inform
      help = 1
  ENDIF

  IF keyword_set(iarr) THEN BEGIN

      ;; first some error checking
      error = 0
      ;; the right type?
      error = error + (total(size(iarr,/TNAME) EQ ['INT','LONG','BYTE']) EQ 0)
      ;; the right number of dimensions?
      error = error + 2*(size(iarr,/n_dimensions) NE 2)
      ;; the right size of the first dimension?
      error = error + 4*((size(iarr,/dimensions))[0] NE 2)
      ;; the values in the index are not too high
      error = error + 8*(max(iarr[0,*]) GE n_elements(mr1.data.(0)))
      error = error + 8*(max(iarr[1,*]) GE n_elements(mr2.data.(0)))

      IF error NE 0 THEN BEGIN
          SWITCH 1 OF
              error AND 1: BEGIN
                  message,'The index array does not have the right type',/inform
              END
              error AND 2: BEGIN
                  message,'The index array does not have the right number of dimensions',/inform
                  message,'It should be [2,X]',/inform
              END
              error AND 4: BEGIN
                  message,'The index array does not have the right dimensions',/inform
                  message,'It should be [2,X]',/inform
              END
              error AND 8: BEGIN
                  message,'The values in the index array are out of bounds',/inform
              END
          ENDSWITCH
          help = 1
      ENDIF

  ENDIF ELSE BEGIN ;; No index array give so do the matching here based
      ;; on the given columns
      IF NOT keyword_set(col1) THEN BEGIN
          message,'You did not specify the column in table one to match',/inform
          help = 1
      ENDIF
      
      IF NOT keyword_set(col2) THEN BEGIN
          message,'You did not specify the column in table two to match',/inform
          message,'Assuming it is the same as in table 1',/inform
          IF keyword_set(col1) THEN col2 = col1
      ENDIF
      
      default,keep1,0
      default,keep2,0
      default,keepall,0
      default,allmatches,0
      
      IF keyword_set(keep1) THEN BEGIN
          keep1 = 1
      END
      IF keyword_set(keep2) THEN BEGIN
          keep2 = 1
      END
      IF keyword_set(keepall) THEN BEGIN
          keep1 = 1
          keep2 = 1
      END
      IF keyword_set(allmatches) THEN BEGIN
          allmatches = 1
      END
      
      
;; Now we have some decisions to make:
;; 1) What do we do with multiple matches?
;; 2) What do we do with non matching elements in table one?
;; 3) What do we do with non matching elements in table two?

;; The default is to keep only the matching records and match each
;; element in mr1 with the first matching elements in mr2

;; However this behaviour can be controlled through the use of the
;; keywords:
;; keep1  : keeps the non-matching lines from table 1
;; keep2  : keeps the non-matching lines from table 2
;; keepall: keeps the non-matching lines from table 1 and 2
;; allmatch: make a record for each match 


      c1 = col_mr(mr1,col1)
      c2 = col_mr(mr2,col2)
      
;;(SH Jul 18 2005)
;; Here we should check whether these columns can be matches......
      
      nc1 = n_elements(c1)
      nc2 = n_elements(c2)
      
;; There are elegant non loopy ways to do this but they require a lot
;; of memory in case of large arrays which is often the case for mr
;; tables.
          
;; create an index array to hold the matched elements and fill it with
;; a dummy elements 0
      iarr =[-1,-1]
      
      FOR i = 0L,nc1-1L DO BEGIN
          matches = where(c1[i] EQ c2,count)
          CASE count OF
              0: BEGIN
                  IF keep1 THEN BEGIN
                      iarr = [[iarr],[i,-1]]
                  ENDIF
              END
              1: BEGIN
                  iarr = [[iarr],[i,matches]]
              END
              ELSE: BEGIN
                  IF NOT allmatches THEN BEGIN
                      iarr = [[iarr],[i,matches[0]]]
                  ENDIF ELSE BEGIN
                      FOR j = 0,count-1 DO BEGIN
                          iarr = [[iarr],[i,matches[j]]]
                      ENDFOR
                  ENDELSE
              END
          ENDCASE
          
      ENDFOR
          
;; now make sure that the missing elements from mr2 are also include
;; incase of keep2 = 1
      IF keep2 THEN BEGIN
          all = make_array(nc2,val=0)
          matched = iarr[1,*]
          all[matched] = 1
          unmatched = where(all EQ 0)
          
          FOR i = 1,n_elements(unmatched)-1 DO BEGIN
              iarr = [[iarr],[-1,unmatched[i]]]
          ENDFOR
          
      ENDIF

      ;; strip the first bogus element
      IF n_elements(iarr[0,*]) GT 1 THEN BEGIN
          iarr = iarr[*,1:*]
      ENDIF ELSE BEGIN
          message,'Not a single match was found',/inform
          return,0
      ENDELSE
  ENDELSE 
  
;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'join_mr'
      return,0
  ENDIF

;; Now we have a proper iarr and we can start the merging of the tables
  nfield1 = n_elements(mr1.name)
  nfield2 = n_elements(mr2.name)
  nfields = nfield1+nfield2
  
;; join the names,units and descriptions
  name = [mr1.name,mr2.name]
  description = [mr1.description,mr2.description]
  unit = [mr1.unit,mr2.unit]

;; Now we have to make sure the names are unique. We use :N for the
;; duplicates. First remove any old :N tags to prevent :2:2 tags etc
  FOR i = 0, nfields-1 DO name[i] = $
    strsplit(name[i],':[0-9]*$',/regex,/extract)
  
;; Now check for duplicates except for the first element
;; Do it backwards for ease of numbering
;; Convert to upcase to do the comparison
  upname = strupcase(name)

  FOR i = nfields-1,1,-1 DO BEGIN
      numdup = floor(total(upname[i] EQ upname))
      IF numdup GT 1 THEN BEGIN
          name[i] = name[i]+':'+ strcompress(string(numdup),/remove_all)
          upname[i] = upname[i]+':'+ strcompress(string(numdup),/remove_all)
      ENDIF
  ENDFOR
  
  fields = make_array(nfields,val=0)
  FOR i=0,nfield1-1 DO BEGIN
     fields[i] = size(mr1.data.(i),/type)
  ENDFOR
  FOR i=0,nfield2-1 DO BEGIN
     fields[nfield1+i] = size(mr2.data.(i),/type)
  ENDFOR

;; make an empty mr structure
  out = make_mr(n_elements(iarr[0,*]),fields=fields,name=name,unit=unit,descr=description)

;; now fill it with the appropriate values
  
  idx1 = where(iarr[0,*] NE -1,count_idx1)
  if count_idx1 ne 0 then begin
     val1 = reform(iarr[0,idx1],count_idx1)
     
     FOR i=0,nfield1-1 DO BEGIN
        tmp = (out.data.(i))
        tmp[idx1] = (mr1.data.(i))[val1]
        out.data.(i) = tmp
     ENDFOR
  endif

  idx2 = where(iarr[1,*] NE -1,count_idx2)
  if count_idx2 ne 0 then begin
     val2 = reform(iarr[1,idx2],count_idx2)
     FOR i=0,nfield2-1 DO BEGIN
        tmp = (out.data.(nfield1+i))
        tmp[idx2] = (mr2.data.(i))[val2]
        out.data.(nfield1+i) = tmp
     ENDFOR
  endif

  tagnames = strupcase(tag_names(out))
  FOR i=0,n_tags(out)-1 DO BEGIN
      IF total(tagnames[i] EQ ['UNIT','NAME','DESCRIPTION','DATA','HEADER','HISTORY']) EQ 0 THEN BEGIN
          foo = execute('out.'+tagnames[i]+' = mr1.'+tagnames[i])
      ENDIF
  ENDFOR

  IF total((tagnames EQ 'HEADER') NE 0) THEN BEGIN
      add_header_mr,out,mr1.header
  ENDIF

  IF total((tagnames EQ 'HEADER') NE 0) THEN BEGIN
      add_history_mr,out,"History of table 1"
      add_history_mr,out,mr1.history
      add_history_mr,out,"History of table 1"
      add_history_mr,out,mr2.history
      add_history_mr,out,"Joined two tables"
  ENDIF

  return,out
  
END
