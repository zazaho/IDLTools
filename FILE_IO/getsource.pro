; Simple tool to get lines from ~/IA_BATCH/readfits.idl/readlws2.idl
; concerning source named name (wildcard are allowed)
PRO getsource,name,list,out=out
  command = 'grep -i '+name+' ~/IA_BATCH/READING/readf*.idl |grep -i read_f'
  spawn,command,list
;  command = 'grep -i '+name+' ~/IA_BATCH/READING/readfits.idl |grep read_faar'
;  spawn,command,list1
;  command = 'grep -i '+name+' ~/IA_BATCH/READING/readlws2.idl |grep read_flws'
;  spawn,command,list2
;  list= [list1,list2]
  idx = where(list NE '',count)
  IF count EQ 0 THEN return
  list= list[idx]
  listlow = strlowcase(list)
  nlist = n_elements(list)
  pos = strpos(listlow,'then ')
  FOR i=0,n_elements(pos)-1 DO BEGIN
    IF pos[i] NE -1 THEN BEGIN
      list[i] = strmid(list[i],pos[i]+5)
    ENDIF
  ENDFOR
  list = strtrim(strcompress(list,/remove_all),2)
  
  IF arg_present(out) THEN BEGIN
    nums = strtrim(sindgen(nlist),2)
    FOR i=0,nlist-1 DO BEGIN
      print,nums[i]+': '+list[i]
    ENDFOR
    print,'Which one should be returned? type number'
    resp = ''
    read,resp
    resp = strtrim(strlowcase(resp),2)
    foo = execute('idx='+resp)
    IF (idx GE 0) AND (idx LT nlist) THEN BEGIN
      line = list[idx]
      foo = execute(line)
      varname = strmid(line,0,strpos(line,'='))
      foo = execute('out='+varname)
    ENDIF
  ENDIF ELSE BEGIN
;;  nums = sindgen(nlist)
    FOR i=0,nlist-1 DO BEGIN
;;    print,nums[i]+': '+list[i]
      print,list[i]
    ENDFOR
  ENDELSE
  
  
  ;; Keep this out until we can add variable to main level
;  IF NOT keyword_set(nochoose) THEN BEGIN
;    print,'Which one(s)? type 1 or [1,3,4] or all or none'
;    resp = ''
;    read,resp
;    resp = strtrim(strlowcase(resp),2)
;    CASE resp OF
;      'none': return
;      'q': return
;      '': return
;      'all': idx = indgen(nlist)
;      ELSE: foo = execute('idx=['+resp+']')
;    ENDCASE
;    FOR i=0,n_elements(idx)-1 DO BEGIN
;      IF (idx[i] GE 0) AND (idx[i] LT nlist) THEN BEGIN
;        line = list[idx[i]]
;        foo = execute(line)
;        varname = strmid(line,0,strpos(line,'='))
;        foo = execute('struct='+varname)
;        foo = routine_names(store=1,varname,struct)
;      ENDIF
;    ENDFOR
;  ENDIF
END
