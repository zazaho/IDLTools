;; stupid function to return the last file extension of a given
;; filename defined by a .

function file_extension,fullname,lowercase=lowercase,uppercase=uppercase,rootname=rootname
  
  fn=fullname
  if keyword_set(lowercase) then begin
     fn = strlowcase(fn)
  endif

  if keyword_set(uppercase) then begin
     fn = strupcase(fn)
  endif

  lastdotpos = strpos(fn,'.',/reverse_search)
  if lastdotpos eq -1 then return,''

  ext=strmid(fn,lastdotpos+1)
  rootname=strmid(fn,0,lastdotpos)

  return,ext

end
