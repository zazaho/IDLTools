FUNCTION ines2aar,inines
  ines=inines
  aar = sh_define_aar(len=n_elements(ines.wavelength))
  aar.data.wave = ines.wavelength*1d-4
  aar.data.flux = ines.flux*(ines.wavelength)^2d0/3d18*1d23
  aar.data.stdev= ines.sigma*(ines.wavelength)^2d0/3d18*1d23
  aar.data.status= ines.quality
  
  aar= sh_select(aar,ines.quality EQ 0)
;  aar= sh_select(aar,aar.data.flux GT 3d0*aar.data.stdev)
  return,aar
END
