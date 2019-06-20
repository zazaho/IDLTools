;S Hony
; Read/Write aar fluxes to/from ascii-file 
;(SH May  4 1999) added error to table

FUNCTION read_ftbl,fname,help=help,skip=skip,comment=comment, $
                   sort=sort,tabel=tabel,_extra=_extra

;no parameters or help requested
  if ((n_params() eq 0) or keyword_set(help)) then begin
    print,'function read_fdat:'
    print,'usage:'
    print,'test = read_fdat(filename[,/help,comment=comment,skip=skip]'
    print,'filename: name of ascii file to read from'
    print,'/help provides this help'
    print,'skip=skip number of lines to skip before the data'
    print,'sort=column: sort the tabel according to column'
    print,'/tabel: output a table instead of an aar-struct'
    return,0
  endif
  
  default,comment,'\'
  default,skip,254
  default,sort,0
  default,tabel,0
    
;make sure the file exists
;;  foo = findfile(fname,count=count)
  foo = file_search(fname,count=count)
  if (count eq 0) then begin
    print,'the file ',fname,' does not exist'
    return,0
  endif
  
  data = read_ascii(fname,data_start=skip,comment=comment,_extra=_extra)
  data = data.(0)
  s = size(data)
  ntags = s[1]
  ndata = s[2]
  
  IF sort GT 0 THEN BEGIN 
    ;; Sort everything according to the requested column
    ;; Just check to make sure the requested column is present
    IF sort GT ntags THEN BEGIN
      print,'READ_FDAT: requested column for sorting does not exist'
      print,'READ_FDAT: No sorting done.'
      idx_order = indgen(ndata)
    ENDIF ELSE BEGIN
      idx_order = sort(reform(data[sort-1,*],ndata))
    ENDELSE
  ENDIF ELSE BEGIN
    idx_order = indgen(ndata)
  ENDELSE
    
  IF tabel THEN BEGIN
    out = data[*,idx_order]
  ENDIF ELSE BEGIN
    ;;store in an aar
    tags = ['line','wave','flux','stdev','flag']
    out = sh_define_aar(length=ndata)
    FOR i = 0, (ntags < n_elements(tags)) -1 DO BEGIN
      f = execute('out.data.'+tags[i]+' = reform(data[i,idx_order],ndata)')
    ENDFOR
  ENDELSE
  
  return,out
END
