;+
; NAME:
;  make_mr
;
; PURPOSE:
;  create a new empty machine readable data structure with the same
;  structure as the input but with a given length or from a given
;  array of field types and optionally using an array of column names,
;  and units
;
; CATEGORY:
;  MR manipulation
;
; CALLING SEQUENCE:
;  out = make_mr(length,struct)
;  or 
;  out = make_mr(length,fields=fieldarr[,names=names,units=units,description=description])
;
; INPUTS:
;  length: [integer]: number of records (required)
;  struct: idl structure with the structure to be copied
;  fields: array of strings or integers denoting the types of the
;          columns

; OPTIONAL KEYWORD INPUT:
; names: string array of columns names. If not specified default to
;        col0 ..colN
; units: string array of units per column. If not specified default to
;        ---
; descriptions: string array of description per column. If not
;        specified default empty 
;
; KEYWORD PARAMETERS:
;  help: if set print the help screen
;
; EXAMPLE:
;  temp = make_mr(all,200)
;
; MODIFICATION HISTORY:
;  (SH Jun 23 2005) First version
;-


FUNCTION make_mr,length,mr,fields=in_fields,names=in_names, $
                 units=in_units,descriptions=in_descriptions,help=help
  
  ;; Check the input
  CASE n_params() OF 
      2: BEGIN
          IF keyword_set(fields) THEN BEGIN
              message,'You specified both a MR structure and the field types',/inform
              message,'Using the MR as a template',/inform
          ENDIF
          IF NOT is_mr(mr) THEN BEGIN
              message,'the input structure is not valid',/inform
              help=1
          ENDIF ELSE BEGIN
              names = mr.name
              units = mr.unit
              descriptions = mr.description
              
              nfields = n_elements(names)
              fields = make_array(nfields)
              FOR i=0,nfields-1 DO BEGIN
                  fields[i] = size((mr.data.(i))[0],/type)
              ENDFOR
          ENDELSE
      END
      1: BEGIN
          ;; Check the field types
          IF NOT keyword_set(in_fields) THEN begin
              message,'You did not specify either a mr structure nor the field types',/inform
              help=1
          ENDIF ELSE BEGIN
              ;; Now make sure the field types are converted to integers
              ;; denoting the field type
              CASE 1  OF
                  (size(in_fields,/tname) EQ 'STRING'): BEGIN
                      sss = ['INT','LONG','DOUBLE','FLOAT','STRING','BYTE','UINT','ULONG','LONG64','ULONG64']
                      iii = [2,3,5,5,7,2,2,3,3,3]
                      
                      fields = make_array(n_elements(in_fields),val=0)
                      tmp = strupcase(in_fields)
                      FOR i = 0,n_elements(fields)-1 DO BEGIN
                          m = where(tmp[i] EQ sss,cnt)
                          IF cnt EQ 0 THEN BEGIN
                              message,'The element: '+strcompress(string(i),/remove_all)+ $
                                      ' in the fields specification is '+in_fields[i],/inform
                              message,'The only supported types are: INT,LONG,DOUBLE,FLOAT,STRING',/inform
                              return,0
                          ENDIF ELSE BEGIN
                              fields[i] = iii[m]
                          ENDELSE
                      ENDFOR
                  END
                  (total(size(in_fields,/tname) EQ ['BYTE','INT','LONG']) NE 0): BEGIN
                      fields = FLOOR(in_fields)
                  END
                  ELSE: BEGIN
                      message,'The specified field-types array should be either strings or integers',/inform
                      return,0
                  END
              ENDCASE
              
              nfields = n_elements(fields)

              ;; Check the names array
              IF keyword_set(in_names) THEN BEGIN
                  CASE 1 OF
                      (size(in_names,/tname) EQ 'STRING'): BEGIN
                          names = in_names
                      END
                      (total(size(in_names,/tname) EQ ['BYTE','INT','LONG','FLOAT','DOUBLE']) NE 0): BEGIN
                          message,'The specified names array has non-string (numerical) elements',/inform
                          message,'Trying to convert',/inform
                          names =strcompress(string(in_names),/remove_all)
                      END
                      ELSE: BEGIN
                          message,'The specified names array has non-string (complex) elements',/inform
                          message,'They will be ignored and default names will be used',/inform
                      END
                  ENDCASE
              ENDIF
          
              IF keyword_set(names) THEN BEGIN
                  nnames = n_elements(names)
                  CASE 1 OF
                      (nnames EQ nfields): BEGIN
                      END
                      (nnames LT nfields): BEGIN
                          message,'Too few names specified',/inform
                          message,'The other columns will get default names.',/inform
                          tmp = names
                          names = 'col'+strcompress(sindgen(nfields),/remove_all)
                          names[0:nnames-1] = tmp
                      END
                      (nnames GT nfields): BEGIN
                          message,'Too many names specified',/inform
                          message,'Using only the first '+ $
                                  strcompress(string(nfields),/remove_all)+ $
                                  ' given names',/inform
                          names = names[0:nfields-1]
                      END
                  ENDCASE
              ENDIF ELSE BEGIN
                  message,'No names specified, using default names',/inform
                  names = 'col'+strcompress(sindgen(nfields),/remove_all)
              ENDELSE
          

              ;; Check the units array
              IF keyword_set(in_units) THEN BEGIN
                  IF (size(in_units,/tname) EQ 'STRING') THEN BEGIN
                      units = in_units
                  ENDIF ELSE BEGIN
                      message,'The specified units array has non-string elements',/inform
                      message,'They will be ignored and default units will be used',/inform
                  ENDELSE
              ENDIF
              
              IF keyword_set(units) THEN BEGIN
                  nunits = n_elements(units)
                  CASE 1 OF
                      (nunits EQ nfields): BEGIN
                      END
                      (nunits LT nfields): BEGIN
                          message,'Too few units specified',/inform
                          message,'The other columns will get ---  as units.',/inform
                          tmp = units
                          units = make_array(nfields,val='---')
                          units[0:nunits-1] = tmp
                      END
                      (nunits GT nfields): BEGIN
                          message,'Too many units specified',/inform
                          message,'Using only the first '+ $
                                  strcompress(string(nfields),/remove_all)+ $
                                  ' given units',/inform
                          units = units[0:nfields-1]
                      END
                  ENDCASE
              ENDIF ELSE BEGIN
                  message,'No units specified, using default units',/inform
                  units = make_array(nfields,val='---')
              ENDELSE
              
              
              ;; Check the descriptions array
              IF keyword_set(in_descriptions) THEN BEGIN
                  IF (size(in_descriptions,/tname) EQ 'STRING') THEN BEGIN
                      descriptions = in_descriptions
                  ENDIF ELSE BEGIN
                      message,'The specified descriptions array has non-string elements',/inform
                      message,'They will be ignored and empty descriptions will be used',/inform
                  ENDELSE
              ENDIF
              
              IF keyword_set(descriptions) THEN BEGIN
                  ndescriptions = n_elements(descriptions)
                  CASE 1 OF
                      (ndescriptions EQ nfields): BEGIN
                      END
                      (ndescriptions LT nfields): BEGIN
                          message,'Too few descriptions specified',/inform
                          message,'The other columns will get an empty descriptions.',/inform
                          tmp = descriptions
                          descriptions = make_array(nfields,val='')
                          descriptions[0:ndescriptions-1] = tmp
                      END
                      (ndescriptions GT nfields): BEGIN
                          message,'Too many descriptions specified',/inform
                          message,'Using only the first '+ $
                                  strcompress(string(nfields),/remove_all)+ $
                                  ' given descriptions',/inform
                          descriptions = descriptions[0:nfields-1]
                      END
                  ENDCASE
              ENDIF ELSE BEGIN
                  message,'No descriptions specified, using empty descriptions',/inform
                  descriptions = make_array(nfields,val='')
              ENDELSE
          ENDELSE
      END
      0: BEGIN
          message,'Please supply either a MR structure of an array field names',/inform
          help=1
      END
  ENDCASE

  IF (NOT keyword_set(help)) AND(NOT keyword_set(length)) THEN BEGIN
      message,'Please supply a length of the required empty structure',/inform
      help=1
  ENDIF
      
  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'make_mr'
      return,0
  ENDIF
  
  emptys = {sundef:0,sbyte:0B,sint:0,slong:0L,sfloat:!values.f_nan,sdouble:!values.d_nan, $
            scomplex:COMPLEX(!values.f_nan,!values.f_nan),sstring:'',sstruct:{null:0}, $
            sdcomplex:DCOMPLEX(!values.d_nan,!values.d_nan), $
            spointer:0,sobjectref:0,suint:0,sulong:0L,slong64:LONG64(0),sulong64:ULONG64(0)}
  
  C0 = make_array(length,val=emptys.(fields[0]))
  exec_string = 'data={C0:C0'
  
  FOR i=1,nfields-1 DO BEGIN
      foo = execute('C'+ strcompress(string(i),/remove_all)+ $
                    '=make_array(length,val=emptys.(fields['+ $
                    strcompress(string(i),/remove_all)+']))')
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
         header:[""], $
         history:[""], $
         data:data}
  return,out

END
