; stupid wrapper around save for one variable which uses xdr as default and can bzip2
; the output
pro savexdr, $
   p0,$
   filename=filename, $
   bzip2=bzip2, $
   _extra=_extra
  
  default,filename,"idlsave.dat"

  if n_params() eq 0 then return
  
  save,filename=filename,_extra=_extra,/xdr,p0
  
  if keyword_set(bzip2) then begin
     spawn,"bzip2 -9f "+filename
  endif

end
