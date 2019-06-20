;+
; NAME:
; add_column_mr
;
; PURPOSE:
; Add a column of data to an existing MR table
;
; CATEGORY:
;  STRUCTURE MANIPULATION
;
; CALLING SEQUENCE:
; new_mr=add_column_mr(old_mr,array[,name=name,unit=unit,description=description,help=help])
;
; INPUTS:
; old_mr, original MR structure
; array: array of data to be added to the MR structure
;
; OPTIONAL INPUTS:
; name(string): name of the column in the new table ['name']
; unit(string): unit of the data column in the new table ['---]
; desciption(string): description of the column in the new table ['']
;
; KEYWORD PARAMETERS:
; help: if set show this help
;
; OUTPUTS:
; an updated MR stucture
; 
; EXAMPLE:
; mr_with_flag = add_column_mr(table,flags_array,name='Qflag')
;
;
; MODIFICATION HISTORY:
; (SH Mar  6 2013): Initial version
;-

function add_column_mr,mr,data,name=name,unit=unit,description=description,help=help

  case 1 of
     keyword_set(help): wanthelp=1
     n_params() ne 2: wanthelp=1
     else: wanthelp=0
  endcase

  if wanthelp then begin
     doc_library,'add_column_mr'
     return,0
  endif
  
  default,name,'name'
  default,unit,'---'
  default,description,''
  
  ;; length check
  
  ndata=n_elements(data)
  if n_elements(mr.data.(0)) ne ndata then begin
     message,/info,'The length of the supplied array does not match the existing table'
     return,-1
  endif
  
  case size(data,/tname) of
     'BYTE': data_type='INT'
     'INT': data_type='INT'
     'UINT': data_type='INT'
     'LONG': data_type='LONG'
     'ULONG': data_type='LONG'
     'FLOAT': data_type='DOUBLE'
     'DOUBLE': data_type='DOUBLE'
     'STRING': data_type='STRING'
     else:  data_type=''
  endcase
  
  ;; create an empty mr structure
  column_mr = make_mr(ndata,fields=data_type,name=name,unit=unit,descr=description)
  ;; copy the data into place
  column_mr.data.(0) = reform(data)
  
  ;; the matching columns are simply each row should be matched
  tiarr = lindgen(ndata)##[1L,1L]
  return,join_mr(mr,column_mr,index_array=tiarr)

end
