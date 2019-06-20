function restorefitxdr,filename,varnames=varnames,status=status

  status=0

  default,varnames,['NX','NY','NOBS','WAVOBS','NUOBS','FNUOBS','DFNUOBS','WEIGHTS','MASK','PARSTR','ERRPARSTR','PARINFO','STATUS','CHI2','FIT','INSTRUMENT','TIMIN','CONT','LINES','BANDS','EXTINCT']
  
  found = file_search(filename,count=count)

  if count ne 1 then begin
     message,'file not found or not unique',/info
     status=1
     return,-1
  endif

  tmpfile = file_decompress(found,status=status)
  if status eq 0 then begin
     restore,tmpfile
     file_decompress,/delete_last
  endif else begin
     message,'An error occured during decompression',/info
     status=1
     return,-1
  endelse
  
  fitsstring = ''
  for iii=0,n_elements(varnames)-1 do begin
     fitsstring=fitsstring+varnames[iii]+':'+varnames[iii]
     if iii ne n_elements(varnames)-1 then begin
        fitsstring=fitsstring+','
     endif
  endfor
  
  foo=execute('out={'+fitsstring+'}')
  return,out
end
