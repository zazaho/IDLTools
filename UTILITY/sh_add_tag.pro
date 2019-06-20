function sh_add_tag,old,name,value,replace=replace
  
  ustring='Usage: newstruct=sh_add_tag(oldstruct,newtagname,newtagcontent)'
  
  if n_params() ne 3 then begin
     message,/info,'This routine takes 3 input parameters'
     message,/info,ustring
     message,/info,'returning without doing anything'
     if n_elements(old) ne 0 then return,old else return,-1
  endif
  
  if size(old,/tname) ne 'STRUCT' then begin
     message,/info,'The input parameter is not a structure'
     message,/info,ustring
     message,/info,'returning without doing anything'
     return,old
  endif
  
  if (size(name,/tname) ne 'STRING') or (n_elements(name) ne 1) then begin
     message,/info,'The second parameter (tagname) is not a string'
     message,/info,ustring
     message,/info,'returning without doing anything'
     return,old
  endif

  tagnames = tag_names(old)

  identitag = where(strlowcase(name) eq strlowcase(tagnames),identicnt,complement=difftag,ncomplement=diffcnt)
  if identicnt ne 0 then begin
     if not keyword_set(replace) then begin
        message,/info,'a tag with the name: '+name+' already exists'
        message,/info,'returning without doing anything'
        return,old
     endif else begin
        message,/info,'a tag with the name: '+name+' already exists'
        message,/info,'replacing the existing tag with the new content'
        if diffcnt eq 0 then begin
           foo=execute('new={name:value}')
           return,new
        endif else begin
           tagnames=tagnames[difftag]
        endelse
     endelse
  endif
  
  cmd = 'new={'
  for i=0,n_elements(tagnames)-1 do begin
     cmd=cmd+tagnames[i]+':old.'+tagnames[i]+','
  endfor
  cmd=cmd+name+':value}'

  foo=execute(cmd)
  return,new
  
end
