;+
; NAME:
;  read_fmr.pro
;
; PURPOSE:
;  Given a machine readable table name and optionally column
;  numbers, this FUNCTION reads the format information in the
;  meta-header and outputs a IDL function containing either the
;  complete table or only the requested columns.
;
; CATEGORY:
;  FILE_IO
;
; CALLING SEQUENCE:
;  data = read_fmr(filename)
;
; INPUTS:
;  filename [STRING]: the name of the file containing the machine
;  readable table. If filename is missing a dialog to select the
;  filename will be presented
;
; KEYWORD PARAMETERS:
; -help: if set show the help
; -columns: [(array of) integers of strings] of column(s) to be returned.
;  If columns is of type integer they represent indices for which
;  column numbers to return, if they are strings the columns with the
;  corresponding names will be returned in the order as given.
; -missingvalue [float]: value with which to replace the missing
;  values in the table default is NaN.
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
; RESTRICTIONS:
;  The file to be read should be formatted as a machine readable datafile.
;
; EXAMPLE:
;  meas = read_fmr('smith.dat',col=[2,5,6])
;   plot,meas.data.column1,ytitle=meas.name[1]+' ('+meas.unit[1]+')'
;
;  and
;  data = read_fmr('smith.dat',col=['Name','Date'])
;   print,meas.data.column0
;   
; MODIFICATION HISTORY:
;  Version 1:
;  Written by Sacha Hony (ESA) Nov 14 2003
;   Based heavily on mrcolextract by Greg Schwarz (AAS Journals
;   staff scientist) on 8/16/00.
;
;  Version 1.1:
;  Updated to be compatible with older versions of IDL.
;    Removed keyword /read from file_exist
;    Replaced strplit with str_sep
;    Fixed bug where column=[3,4] always returned the first few
;    columns
;(SH Aug 11 2008)- Added shell expansion

FUNCTION read_fmr,filename_in, $
                  columns=columns, $
                  missingvalue=missingvalue, $
                  help=help
  

  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'read_fmr'
      return,0
  ENDIF
  
  ;; If no filename is given then pop-up the dialog_pickfile dialog
  IF NOT keyword_set(filename_in) THEN BEGIN
      filename_in =dialog_pickfile(filter=['*.dat;*.asc*;*.txt','*'], $
                                /must_exist)
  ENDIF

  filename = expand_string(filename_in)

  ;; Check that file exists and is readable otherwise bail-out
  IF NOT FILE_TEST(filename) THEN BEGIN
      message,'The file: '+filename+' does cannot be found or read', $
              /informational
      return,0
  ENDIF
  
  IF n_elements(missingvalue) EQ 0 THEN missingvalue=!VALUES.F_NAN
  
;; Variables needed to read single lines of the file
  dumI=' '
  tmp=''
  irow=0L ;; Make sure it can hold a lot of lines
  startpos=' '
  endpos=' '

;; Variable in which the total information of the files is collected  
  names=''
  units=''
  descriptions=''
  startposs=0
  idltypes=0
  
  openr,lun,filename,/get_lun
  
;; Read the first few lines into a dummy variable
;; because this info is not needed.  However, keep
;; track of the number of lines.
  WHILE (stregex(dumI,'Bytes\ *Format',/boolean) EQ 0) DO BEGIN
      readf,lun,dumI
      irow=irow+1L
  END 
  
  readf,lun,dumI
  irow=irow+1L
  
;; Read until you reach a '------' line terminator
  WHILE (strpos(tmp,'-----------------') EQ -1) DO BEGIN
      irow=irow+1L
      
;; Extract out the 6-8th positions.  
;; If there is a number you have a column
      readf,lun,f='(1X,A3,1X,A3,1X,A80)',startpos,endpos,tmp
      
;; If startpos is --- then you are at the end 
;; so set the 9999 flag so it isn't counted
      IF (startpos EQ '---') THEN startpos = '9999'

;; If starpos is blank then this is either a continuation
;; line or a column that is only one digit wide.  You can
;; tell by checking if endpos is also blank.  If it is a 
;; column then set startpos and endpos to the same value
      IF (startpos EQ '   ') THEN BEGIN
          startpos = endpos
          IF (endpos EQ '   ') THEN startpos = '9999'
      ENDIF
      IF (fix(startpos) GE 1 AND fix(startpos) LE 999) THEN BEGIN
          
;; Squeeze out the blanks.
          less_blanks = strcompress(strtrim(tmp,2))
          
;; Separate the non-location info by sorting into an array that is 
;; delimited by blank spaces.  The first position is the format,
;; the second is the units, the third is the name, and the last
;; positions are the short description of the column
          
;;(SH Nov 18 2003) strsplit is not available in older versions of IDL
          components=strsplit(less_blanks,' ',/extract)
;;          components=str_sep(less_blanks,' ')

;; Determine the column type (A|I|F|E)
          vtype = strmid(components[0],0,1)
          CASE vtype OF
              'A': idltype = 7
              'I': BEGIN
                  vlength = fix(strmid(components[0],1))
                  CASE 1 OF
                      (vlength LT 5): BEGIN
                          idltype = 2
                      END
                      (vlength GE 5): BEGIN
                          idltype = 3
                      END
                  ENDCASE
              END
              'F': idltype = 5
              'E': idltype = 5
          ENDCASE
          
          ;; Add the collected data to the lists
          names=[names,components(2)]
          units=[units,components(1)]
          ;; Take the rest of the strings a description
          description=''
          FOR i=3,n_elements(components)-1 DO description=description+ $
            components[i]+' '
          descriptions=[descriptions,description]
          startposs=[startposs,startpos-1]
          idltypes=[idltypes,idltype]
      ENDIF 
  ENDWHILE

;; iskip is the end (maybe see below) of the meta-header 
  iskip=irow
  
;; Continue reading the file to get the number of lines
  lastdash=0L
  WHILE NOT eof(lun) DO BEGIN
      readf,lun,dumI
      irow=irow+1L
;; If you encounter another '--------' (e.g. the end of a
;; notes subsection) mark it because you don't want to 
;; read the previous information as data!
      IF (strmid(dumI,0,6) EQ '------') THEN BEGIN
          lastdash=irow
      ENDIF
  ENDWHILE
  
  ;; Make sure we close the file and free the lun
  close,lun
  free_lun,lun
  
;; If you found a '-------' line then set iskip to the last dash
;; line so not to read any extra headers
  IF (lastdash NE 0L) THEN BEGIN
      iskip=lastdash
  ENDIF

;; Clean the arrays from the first dummy element
  names=names[1:*]
  units=units[1:*]
  descriptions=descriptions[1:*]
  startposs=startposs[1:*]
  idltypes=idltypes[1:*]
  ncolumns = n_elements(startposs)
  idx_columns = indgen(ncolumns)


  header = make_array(iskip,val="")
  
  openr,lun,filename,/get_lun
  readf,lun,header
  close,lun
  free_lun,lun

  ;; now fill the template stuff for read_ascii
  template = {VERSION:1.00000, $
              DATASTART:iskip, $
              DELIMITER:0B, $
              MISSINGVALUE:missingvalue, $
              COMMENTSYMBOL:'', $
              FIELDCOUNT:ncolumns, $
              FIELDTYPES:idltypes, $
              FIELDNAMES:'COLUMN'+strcompress(sindgen(ncolumns),/remove_all), $
              FIELDLOCATIONS:startposs, $
              FIELDGROUPS:indgen(ncolumns)}
  
  data = read_ascii(filename,template=template)
  
  ;; This is all. if the columns keyword is given then
  ;; only certain columns are requested. So do the selections here
  IF keyword_set(columns) THEN BEGIN

      ncolumns = n_elements(columns)

      ;; are they strings?
      IF size(columns,/tname) EQ 'STRING' THEN BEGIN
          
          ;; first convert the columns and the output names to uppercase
          ;; to be able to compare them directly without strcmp
          names_up   = strupcase(names)
          columns_up = strupcase(columns)

          ;; create an array to hold the requested column numbers set
          ;; these to -1
          idx_columns = make_array(ncolumns,value=-1)

          ;; Now match each string with the names
          FOR i=0,ncolumns-1 DO BEGIN
              ;; take the first instance where the uppercase name and
              ;; uppercase column match
              idx_columns[i] = ( where(names_up EQ columns_up[i]) )[0]
          ENDFOR

          ;; Are there elements which did not find a match?
          idx_missing_columns = where(idx_columns EQ -1,cnt)

          ;; All the elements of idx_columns are -1
          IF (cnt EQ ncolumns) THEN BEGIN
              message,'None of the column names could be found in the table', $
                      /informational
              return,0
          ENDIF

          ;; Some elements are matched but some are missing
          IF (cnt NE 0) THEN BEGIN
              message,'The following columns are not present in the table:', $
                      /informational
              message,columns[idx_missing_columns], $
                      /informational
              ;; Only take the valid columns and still continue
              idx_columns =idx_columns[where(idx_columns NE -1)]
          ENDIF

      ENDIF ELSE BEGIN
          ;; Assume the columns are numbers which indicate the
          ;; requested column numbers

          max_column=n_tags(data)-1
          columns = fix(columns)
          ;; make sure they are not higher than the available number
          ;; of columns and not negative
          idx_columns = columns[where( (columns LE max_column) AND $
                                       (columns GE 0) ,cnt)]

          IF (cnt EQ 0) THEN BEGIN
              message,'The requested columns are not present in the file', $
                      /informational
              return,0
          ENDIF

          ;; Some elements are matched but some are too high
          IF cnt NE ncolumns THEN BEGIN
              message,'Some column numbers are out of range.'+ $
                      ' Valid range=[0,'+ $
                      strcompress(string(max_column),/remove_all)+']', $
                      /informational
          ENDIF
      ENDELSE

;; now take only the requested columns and data
      names=names[idx_columns]
      units=units[idx_columns]
      descriptions=descriptions[idx_columns]
      ncolumns = n_elements(names)

      exec_string = 'data={COLUMN0:data.('+string(idx_columns[0])+')'
      FOR i=1,ncolumns-1 DO BEGIN
          exec_string = exec_string+',COLUMN'+ $
                        strcompress(string(i),/remove_all)+ $
                        ':data.('+string(idx_columns[i])+')'
      ENDFOR
      exec_string=exec_string+'}'
      foo = execute(exec_string)

  ENDIF
  
  ;; derive the field types
  fields = make_array(ncolumns,val=0)
  FOR i=0,ncolumns-1 DO BEGIN
      fields[i] = size(data.(i),/type)
  ENDFOR
      
  ;; Make an empty structure to hold the data
  out = make_mr(n_elements(data.(0)),fields=fields,names=names,units=units,descriptions=descriptions)
  
  FOR i=0,ncolumns-1 DO BEGIN
      out.data.(i) = data.(i)
  ENDFOR

  add_header_mr,out,header

  message,"Read "+strcompress(ncolumns)+" columns from "+ $
          filename,/informational
  
  return,out
  
END
