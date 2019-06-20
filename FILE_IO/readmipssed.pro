function readmipssed, file
  cube = mrdfits(file,0,hdr)
  
  ;; reference values
  crval3  = sxpar(hdr,'CRVAL3' )
  cdelt3  = sxpar(hdr,'CDELT3')
  crpix3  = sxpar(hdr,'CRPIX3' )
  nwave = n_elements(cube[0,0,*])

  wave = crval3+(dindgen(nwave)-crpix3)*cdelt3

  return, {cube:cube,wave:wave,header:hdr}
end
