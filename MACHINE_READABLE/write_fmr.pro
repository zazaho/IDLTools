;; Routine to take a column of data and optimally format it to be
;; written to an ascii file. It takes as input the data in col. It
;; returns an array of the same length of type string.
;; it returns via fstring the format string for the header of the MR table.
;; it returns via length the maximum length of the string.
FUNCTION write_fmr_format,col,fstring=fstring,length=length,maxdecimals=maxdecimals
  
  coltype = size(col,/tname)
  CASE 1 OF 
      ;; Integer types
      total(coltype EQ ['BYTE','INT','LONG','UINT','ULONG','LONG64','ULONG64']) NE 0: BEGIN
          
          out = string(col,format='(I20)')
          out = strtrim(out,2)
          
          nans = where(strupcase(out) EQ "NAN",cnt)
          IF cnt NE 0 THEN out[nans] = ""

          length = max(strlen(out))
          
          fstring = 'I'+strcompress(string(length),/remove_all)
          
      END
      
      ;; Floating point type
      total(coltype EQ ['FLOAT','DOUBLE']) NE 0: BEGIN
          ;; check the range present if it is between -10000 and 10000 and no
          ;; values between -0.0001 and 0.0001 we use floating point
          ;; notation otherwise use exponential notation
          ;; a special exception should be 0 
          foo = abs(col)
          nonzero = where(foo NE 0,cnt)
          IF cnt NE 0 THEN BEGIN
              IF (min(foo[nonzero]) GE 1d-4) AND (max(foo) LT 1d5) THEN BEGIN
                  out = string(col,format='(F17.10)')
                  out = strtrim(out,2)
                  
                  nans = where(strupcase(out) EQ "NAN",cnt)
                  IF cnt NE 0 THEN out[nans] = ""
                  
                  signs = make_array(n_elements(out),val="")
                  numerals = signs
                  decimals = signs
                  
                  FOR i = 0L,n_elements(out)-1L DO BEGIN
                      IF out[i] NE "" THEN BEGIN
                          ;;first disect the string
                          split = stregex(out[i],'^(-*)([0-9]*)\.([0-9]*)$',/extract,/fold_case,/subexpr)
                          
                          ;; Did the matching work?
                          IF (split[0] EQ out[i]) THEN begin
                              signs[i]     = split[1]
                              numerals[i]  = split[2]
                              decimals[i]  = strsplit("."+split[3],/regex,'\.*0*$',/extract,/fold_case)
                              out[i] = signs[i]+numerals[i]+decimals[i]
                          ENDIF
                      ENDIF
                  ENDFOR
                  
                  lsigns = max(strlen(signs))
                  lnumerals = max(strlen(numerals))
                  ldecimals = max(strlen(decimals)) < maxdecimals
                  
                  length = lsigns+lnumerals+ldecimals+1
                  
                  fstring = 'F'+strcompress(string(length),/remove_all)+'.'+ $
                            strcompress(string(ldecimals),/remove_all)
                  
              ENDIF ELSE BEGIN
                  out = string(col,format='(E17.10)')
                  out = strtrim(out,2)
                  
                  nans = where(strupcase(out) EQ "NAN",cnt)
                  IF cnt NE 0 THEN out[nans] = ""
                  
                  signs = make_array(n_elements(out),val="")
                  numerals = signs
                  decimals = signs
                  esigns = signs
                  exponents = signs
                  
              ;; compact the strings by removing unneeded zeros
                  FOR i = 0L,n_elements(out)-1L DO BEGIN
                      IF out[i] NE "" THEN BEGIN
                          
                          ;;first disect the string
                          split = stregex(out[i],'^(-*)([0-9]*)\.([0-9]*)E([-+])([0-9]*)$',/extract,/fold_case,/subexpr)
                          
                          ;; Did the matching work?
                          IF (split[0] EQ out[i]) THEN BEGIN
                              signs[i]     = split[1]
                              numerals[i]  = split[2]
                              decimals[i]  = strsplit("."+split[3],/regex,'\.*0*$',/extract,/fold_case)
                              esigns[i]    = split[4]
                              exponents[i] = (stregex(split[5],'^0*(.*)',/extract,/fold_CASE,/subexpr))[1]
                              
                              ;; make sure that we have something in the
                              ;; numeral and exponent
                              IF (numerals[i] EQ "A") THEN numerals[i] = '0'
                              IF (exponents[i] EQ "") THEN exponents[i] = '0'
                              
                              out[i] = signs[i]+numerals[i]+decimals[i]+'E'+esigns[i]+exponents[i]
                              
                           ENDIF
                      ENDIF
                  ENDFOR
              
                  lsigns = max(strlen(signs))
                  lnumerals = max(strlen(numerals))
                  ldecimals = max(strlen(decimals)) < maxdecimals
                  lexponents = max(strlen(exponents))
                  
                  length = lsigns+lnumerals+ldecimals+3+lexponents
                  
                  fstring = 'E'+strcompress(string(length),/remove_all)+'.'+ $
                            strcompress(string(ldecimals),/remove_all)

              ENDELSE
          ENDIF ELSE BEGIN
              ;; Only zero values thus write 0.0
              out = string(col,format='(F3.1)')
              
              length = 3
              fstring = 'F3.1'
          ENDELSE

          ;; apply the final formatting string to all data
          out = strcompress(string(col,format="("+fstring+")"),/remove_all)
          ;; remove nans again
          nans = where(strupcase(out) EQ "NAN",cnt)
          IF cnt NE 0 THEN out[nans] = ""
                  
      END
      ;; string type
      coltype EQ 'STRING': BEGIN
          out = strtrim(col,2)
          length = max(strlen(out))
          fstring = 'A'+strcompress(string(length),/remove_all)
      END
      ELSE: BEGIN
          message,'The give column type ('+coltype+') is not supported by MR.',/error
          return,0
      END
  ENDCASE

  out = string(format='(A'+strtrim(string(length),2)+')',out)
  return,out

END


;+
; NAME:
;  write_fmr.pro
;
; PURPOSE:
;  Given a machine readable table and a file name and optionally column
;  numbers, this FUNCTION writes a formatted ascii file that contains
;  the data and can be read with read_fmr
;
; CATEGORY:
;  FILE_IO
;
; CALLING SEQUENCE:
;  write_fmr,mr_struct,filename
;
; INPUTS:
;  struct [mr_structure]: the structure that holds the data and
;  records .name .unit .description.
;  filename [STRING]: the name of the file to be written
;
; KEYWORD PARAMETERS:
; -help: if set show the help
; -filename [STRING]: name of file to use as output file
; -columns: [(array of) integers of strings] of column(s) to be written.
;  If columns is of type integer they represent indices for which
;  column numbers to write, if they are strings the columns with the
;  corresponding names will be written in the order as given.
; -title [STRING]: If given this string will be used as the title of
;  the output table
; -maxdecimals: [(array of) integers] indicating per each column of
;  numeric data the maximum number of decimal places to be written.
;  If one value is given this value is use for all columns. If an
;  array is given each value is applied to its correspoinding column.
;  In the (bug prone) case of an array with less elements than the
;  number of columns to be written the last value will be repeated.
;  Use a large integer to obtain automatic determinations.
; 
; OUTPUTS:
;  No output
;
; EXAMPLE:
;  write_fmr,smith_data,'smith.dat'
;
; MODIFICATION HISTORY:
;  Version 1.1:
;  (SH Aug  9 2005) Written by Sacha Hony (ESA) 
;  (SH Sep 17 2015) Add optional maxdecimals parameter
;  (SH Jan 19 2017) fixed formatting of exponential values
;-

PRO write_fmr,mr,file,filename=fname,columns=columns,title=title, $
              maxdecimals_in=maxdecimals_in, $
              help=help

  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'write_fmr'
      return
  ENDIF
  
  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return
  ENDIF

  IF (NOT keyword_set(file)) THEN BEGIN
      IF keyword_set(fname) THEN BEGIN
          file = fname
      ENDIF ELSE BEGIN
          message,'Please supply an output filename',/inform
          return
      ENDELSE
  ENDIF

  ;; take all columns if none are specified
  default,columns,mr.name
  ncolumns = n_elements(columns)
  
  ;; use some huge value if nothing is specified
  default,maxdecimals_in,666
  ;; protect again changing the input
  maxdecimals=round(maxdecimals_in)
  nmaxdecimals = n_elements(maxdecimals)
  
  ;; array-ify the maxdecimal value to have te same number of elements
  ;; as columns
  case 1 of
     nmaxdecimals eq 1: begin
        maxdecimals=make_array(ncolumns,value=maxdecimals)
     end
     nmaxdecimals ge ncolumns: begin
        ;; do nothing
     end
     nmaxdecimals lt ncolumns: begin
        ;; use the maxdecimal array but repeat the last value until
        ;; the end of an array the size of columns
        maxdecimals=[maxdecimals,make_array(ncolumns-nmaxdecimals,value=maxdecimals[nmaxdecimals-1])]
     end
  endcase
  
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
          message,'The specified columns do not exist in the MR table'
          return
      ENDIF
      col_idx = col_idx[1:*]
  ENDIF ELSE BEGIN
      max_col = n_elements(mr.name)
      valid=where((columns GE 0) AND (columns LT max_col),cnt)
      CASE 1 OF 
          (cnt EQ 0): BEGIN
              message,'None of the specified columns exist',/inform
              return
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

  name = mr.name[col_idx]
  unit = mr.unit[col_idx]
  description = mr.description[col_idx]
  maxdecimals = maxdecimals[col_idx]

;; Make sure there are no empty units  
  bad = where(unit EQ "",cnt)	
  IF cnt NE 0 THEN unit[bad] = "---"
  
  openw,lun,file,/get_lun
  
  IF keyword_set(title) THEN BEGIN
      printf,lun,title
  ENDIF

;; Now write the header  
  printf,lun,"================================================================================"
  printf,lun,"Byte-by-byte Description of table"
  printf,lun,"--------------------------------------------------------------------------------"
  printf,lun,"   Bytes Format Units  Label   Explanations"
  printf,lun,"--------------------------------------------------------------------------------"

;; The column in ascii characters where the next data column will start
  starting_col = 1

  max_name = strtrim(string(max(strlen(name))),2)
;; Now write the field descriptions  
  FOR i = 0,n_elements(col_idx)-1 DO BEGIN
      fmt = write_fmr_format(mr.data.(col_idx[i]),fstring=fstring,length=length,maxdecimals=maxdecimals[i])
      printf,lun,format='(" ",I3,"-",I3," ",A6," ",A6," ",A'+max_name+'," ",A)',starting_col, $
             starting_col+length,fstring,unit[i],name[i],description[i]
      foo = execute('c'+strtrim(string(i),2)+'=fmt')
      starting_col = starting_col+length+1
  ENDFOR
  printf,lun,"--------------------------------------------------------------------------------"

  tmpstr = "c0"
  FOR i = 1,n_elements(col_idx)-1 DO BEGIN
      tmpstr=tmpstr+'+" "+c'+strtrim(i,2)
  ENDFOR
  
  foo = execute("printf,lun,format='(A)',"+tmpstr)

  close,lun
  free_lun,lun

END
