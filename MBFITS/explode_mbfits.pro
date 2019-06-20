;; function to create an exploded directory structure from a mbfitsfile

pro explode_mbfits,filename,struct=struct,_extra=_extra
  
  if n_elements(struct) eq 0 then begin
     struct = readmbfits(filename)
  endif
  ;; name is not used because we use the header info
  ;; WARNING: any old directory will be erased unless you pass
  ;; keepold to writembfits
  writembfits,'exploded_mbfits_dir',struct,/explode,_extra=_extra

end
