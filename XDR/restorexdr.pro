;;restore from a file which can be bzip2'ed
function restorexdr,filename,status=status

  status=0

  found = file_search(filename,count=count)

  if count ne 1 then begin
     message,'file not found or not unique',/info
     status=1
     return,-1
  endif

  ;; check for compressed files
  ext=file_extension(found,/lower,rootname=rootname)
  if ext eq 'bz2' then begin
     randomfile=strcompress('torestore'+string(floor(randomu(time)*1d6)),/remove_all)
     spawn,'bunzip2 -c '+found+' > '+randomfile
     restore,randomfile
     file_delete,randomfile
  endif else begin
     restore,found
  endelse

  return,p0

end
