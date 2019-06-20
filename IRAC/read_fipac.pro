;+
; NAME:
;  read_fipac.pro
;
; PURPOSE:
;  Read IPAC tables into IDL structure
;  Given a IPAC table name and optionally column
;  numbers, this FUNCTION reads the format information in the
;  meta-header and outputs a IDL function containing either the
;  complete table or only the requested columns.
;
; CATEGORY:
;  FILE_IO
;
; CALLING SEQUENCE:
;  data = read_fipac(filename)
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
; /use_position: if set use the position of the | to separate the
;   columns rather than assuming that they are seperatated yb white space
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
;  meas = read_fipac('smith.dat',col=[2,5,6])
;   plot,meas.data.column1,ytitle=meas.name[1]+' ('+meas.unit[1]+')'
;
;  and
;  data = read_fipac('smith.dat',col=['Name','Date'])
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
;  (SH Aug  3 2009) added option to extract by fixed width column which
;  helps for character arrays with spaces in them
;
;-

function read_fipac,filename, $
                   columns=columns, $
                    help=help, $
                    use_position=use_position
  
  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'read_fmr'
      return,0
  ENDIF
  
  ;; If no filename is given then pop-up the dialog_pickfile dialog
  IF NOT keyword_set(filename) THEN BEGIN
      filename =dialog_pickfile(filter=['*.tbl','*.dat;*.asc*;*.txt','*'], $
                                /must_exist)
  ENDIF
  
  ;; Check that file exists and is readable otherwise bail-out
  IF NOT FILE_TEST(filename) THEN BEGIN
      message,'The file: '+filename+' does cannot be found or read', $
              /informational
      return,0
  ENDIF
  
;; This part taken from  sm_read_ipac_body, filename, outbody

; Initialize fields 

  got_tags   = 0
  got_type   = 0
  type_error = 0

; Open file for read
  
  openr, lun1, filename, /get_lun

; Loop to read file line by line

  line1 = ' '

  idltypes = 0

;; Use this to only read the header and then use read_ascii
  do_read = 1
  iskip = -1

  while ( (NOT eof(lun1)) AND (do_read EQ 1)) do begin 
      readf, lun1, line1
      iskip = iskip + 1
      
; End processing at once if type error found
      
      if (type_error eq 1) then break
      
; Ignore blank lines 
      
      if stregex(line1, '^ *$', /BOOLEAN) then continue 
      
; Check first character of each line
; \ - designates header
; | - designates type line
; all else - data line 

      firstchar = strmid(line1, 0, 1)

; Completly ignore lines begining with "\" - IE. part of header
; Header is processed seperately in program "sm_read_ipac_header.pro"
   
      case firstchar of
          '\': begin
          end
          
; Lines begining with "|" designate column/structure tags

          '|': begin
              
; First "|" line lists column names 
; Find positions and names of column names 
              
              posnarr = [strsplit(line1,'|'), strlen(line1)-1]
              namearr = strtrim(strsplit(line1,'|', /EXTRACT), 2) 
              
; Store names in "names" for use as data structure tags
; Then by-pass rest of processing and read next record

              if (got_tags eq 0) then begin 
                  names = namearr
                  got_tags = 1
                  break
              endif 
              
; Second "|" line lists column data types  
; Find positions and names of column data types  

; By-pass any "|" line beyond the type fields line
              
              if (got_type eq 1) then break 

; Loop to check through name array 
              
              for i = 0, n_elements(namearr)-1 do begin 

; Set format type
                  
                  formattype = ''
                  if(strmatch(namearr[i],'r*') eq 1) then formattype = 'r'
                  if(strmatch(namearr[i],'i*') eq 1) then formattype = 'i'
                  if(strmatch(namearr[i],'l*') eq 1) then formattype = 'i'
                  if(strmatch(namearr[i],'d*') eq 1) then formattype = 'd'
                  if(strmatch(namearr[i],'c*') eq 1) then formattype = 'c'

; Assemble output format 

                  CASE formattype OF
                      'c': idltype = 7
                      'i': idltype = 3
                      'r': idltype = 5
                      'd': idltype = 5
                  ENDCASE
                  
                  idltypes = [idltypes,idltype]
                  
                  if(type_error eq 1) then break              
                  
              endfor
        
; Set flag to bypass structure set up processing once structure is set up 
              
              got_type = 1

; Endcase of "firstchar" = "|"

          end

; Process data 
; "else" statement - lines not begining with 
; "\" or "|" are data to be output 

          else: begin
              ;; Stop reading as we have arrived at the data part
              do_read = 0

          end
      endcase
      
  endwhile
  

  ;; Make sure we close the file and free the lun
  close,lun1
  free_lun,lun1
  
;; Clean the arrays from the first dummy element
  idltypes=idltypes[1:*]
  ncolumns = n_elements(names)
  
  if not keyword_set(use_position) then begin
     ;; now fill the template stuff for read_ascii
     template = {VERSION:1.00000, $
                 DATASTART:iskip, $
                 DELIMITER:32B, $
                 MISSINGVALUE:!VALUES.F_NAN, $
                 COMMENTSYMBOL:'', $
                 FIELDCOUNT:ncolumns, $
                 FIELDTYPES:idltypes, $
                 FIELDNAMES:'COLUMN'+strcompress(sindgen(ncolumns),/remove_all), $
                 FIELDLOCATIONS:indgen(ncolumns), $
                 FIELDGROUPS:indgen(ncolumns)}
  endif else begin
     ;; now fill the template stuff for read_ascii
     template = {VERSION:1.00000, $
                 DATASTART:iskip, $
                 DELIMITER:0B, $
                 MISSINGVALUE:!VALUES.F_NAN, $
                 COMMENTSYMBOL:'', $
                 FIELDCOUNT:ncolumns, $
                 FIELDTYPES:idltypes, $
                 FIELDNAMES:'COLUMN'+strcompress(sindgen(ncolumns),/remove_all), $
                 FIELDLOCATIONS:posnarr[0:ncolumns-1], $
                 FIELDGROUPS:indgen(ncolumns)}
  endelse
  
  data = read_ascii(filename,template=template)

;; Now we have every thing cast it into the proper IDL structure
;; We have the following info in outbody:
;; tag_names(outbody) = names

  units = make_array(ncolumns,val="")
  descriptions = units
      
  ;; We need this to restructure the data structure to have tag names
  ;; like column0, column1, etc
  exec_string = 'data={COLUMN0:data.(0)'
  FOR i=1,n_elements(names)-1 DO BEGIN
      exec_string = exec_string+',COLUMN'+ $
                    strcompress(string(i),/remove_all)+ $
                    ':data.('+string(i)+')'
  ENDFOR
  exec_string=exec_string+'}'
  foo = execute(exec_string)

;    TYPE            STRING    'mr_structure'
;    NAME            STRING    Array[X]
;    UNIT            STRING    Array[X]
;    DESCRIPTION     STRING    Array[X]
;    DATA            STRUCT    -> <Anonymous> Array[1]

  ;; This is all if the columns keyword is given then
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

;; now take the requested columns
      names=names[idx_columns]
      units=units[idx_columns]
      descriptions=descriptions[idx_columns]
      ncolumns = n_elements(names)

      ;; We need this to restructure the data structure to hold only
      ;; the requested columns
      exec_string = 'data={COLUMN0:data.('+string(idx_columns[0])+')'
      FOR i=1,ncolumns-1 DO BEGIN
          exec_string = exec_string+',COLUMN'+ $
                        strcompress(string(i),/remove_all)+ $
                        ':data.('+string(idx_columns[i])+')'
      ENDFOR
      exec_string=exec_string+'}'
      foo = execute(exec_string)
  ENDIF
  
  out = {type:'mr_structure', $
         name:names, $
         unit:units, $
         description:descriptions, $
         header:[""], $
         history:[""], $
         data:data}
  
  message,"Read "+strcompress(ncolumns)+" columns from "+ $
          filename,/informational
  
  return,out

end
