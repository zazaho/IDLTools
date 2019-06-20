pro pro_add_tag,struct,name,value,replace=replace
  
  ustring='Usage: pro_add_tag,struct,newtagname,newtagcontent'
  
  if n_params() ne 3 then begin
     message,/info,'This routine takes 3 input parameters'
     message,/info,ustring
     message,/info,'returning without doing anything'
     if n_elements(struct) ne 0 then return
  endif
  
  if size(struct,/tname) ne 'STRUCT' then begin
     message,/info,'The input parameter is not a structure'
     message,/info,ustring
     message,/info,'returning without doing anything'
     return
  endif
  
  if (size(name,/tname) ne 'STRING') or (n_elements(name) ne 1) then begin
     message,/info,'The second parameter (tagname) is not a string'
     message,/info,ustring
     message,/info,'returning without doing anything'
     return
  endif

  tagnames = tag_names(struct)

  identitag = where(strlowcase(name) eq strlowcase(tagnames),identicnt,complement=difftag,ncomplement=diffcnt)
  if identicnt ne 0 then begin
     if not keyword_set(replace) then begin
        message,/info,'a tag with the name: '+name+' already exists'
        message,/info,'returning without doing anything'
        return
     endif else begin
        message,/info,'a tag with the name: '+name+' already exists'
        message,/info,'replacing the existing tag with the new content'
        if diffcnt eq 0 then begin
           foo=execute('struct={name:value}')
           return
        endif else begin
           tagnames=tagnames[difftag]
        endelse
     endelse
  endif
  
  cmd = 'struct={'
  for i=0,n_elements(tagnames)-1 do begin
     cmd=cmd+tagnames[i]+':struct.'+tagnames[i]+','
  endfor
  cmd=cmd+name+':value}'

  foo=execute(cmd)
end
