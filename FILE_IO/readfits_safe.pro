function readfits_safe,filename,header,heap,_extra=_extra

  exten_no = 0
  fits = readfits(filename,header,heap,exten_no=exten_no,_extra=_extra)
  while n_elements(fits) eq 1 do begin
     exten_no = exten_no+1
     fits = readfits(filename,header,heap,exten_no=exten_no,_extra=_extra)
  endwhile

  return,fits
end

