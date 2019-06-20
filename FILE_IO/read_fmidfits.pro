FUNCTION read_fmidfits,fname

  flux = mrdfits(fname,0,header)
  nsiz = fxpar(header,'NAXIS1')
  wave0 = fxpar(header,'CRVAL1')
  dwave = fxpar(header,'CDELT1')

  wave = dindgen(nsiz)*dwave+wave0
  
  out = sh_define_aar(length=nsiz)
  out.data.wave = wave
  out.data.flux = flux
  return,out
END 
