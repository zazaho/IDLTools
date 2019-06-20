function fitslope,wave,flux
  lw = alog10(wave)
  lf = alog10(flux)

  r = poly_fit(lw,lf,1)
  return,r[1]
end
