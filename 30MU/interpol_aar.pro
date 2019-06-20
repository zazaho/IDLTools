FUNCTION interpol_aar,aar_in,step=step

  default,step,0.1
  wmin = min(aar_in.data.wave,max=wmax)
  
  nwave = floor((wmax-wmin)/step)+1
  aar_out = sh_define_aar(len=nwave)
  wave = findgen(nwave)*step+wmin
  lflux = alog10(aar_in.data.flux)
  flux = 10d0^(shc_interpol(aar_in.data.wave,lflux,wave))
  out=sh_define_aar(len=nwave)
  out.data.wave=wave
  out.data.flux=flux
  return,out
END

