pro showtags,struct,prefix=prefix

  default,prefix,''
  tagnames = tag_names(struct)
  for i =0,n_elements(tagnames)-1 do begin
     foo = execute(tagnames[i]+'=struct.(i)')
     foo = execute('help,'+tagnames[i]+',output=helpoutput')
     print,prefix+helpoutput
     if size(struct.(i),/tname) eq 'STRUCT' then begin
        showtags,struct.(i),prefix=prefix+'  '
     endif
  endfor
end
