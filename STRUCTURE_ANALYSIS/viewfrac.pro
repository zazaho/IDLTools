pro viewfrac,file,_extra=_extra

  if n_params() eq 0 then begin
     file = dialog_pickfile(/MUST_EXIST)
  endif

;  tmpfile = file_decompress(file)
;  frac_data =(read_ascii(tmpfile)).(0)
;  file_decompress,/cleanup

  frac_data =(read_ascii(file)).(0)

  p=plot(frac_data,linestyle="none",symbol="dot",_extra=_extra)
  p.Scale,1,1,2
end
