;; stupid wrapper around readfits.pro which allows for compressed
;; files using file_decompress

function readfitz,filename,header,heap,_extra=_extra

  tmp = file_decompress(filename,status=status)
  if status eq 0 then begin
     fitz = readfits(tmp,header,heap,_extra=_extra)
  endif
  file_decompress,/delete_last
  return,fitz

end
