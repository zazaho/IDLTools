FUNCTION aar_clean, aar
  
  aarin = aar
  aarin.data=aarin.data(sort(aarin.data.wave))
  w=aarin.data.wave
  f=aarin.data.flux

  scaleplot
  x0=!x.range(0)
  x1=!x.range(1)
  y0=!y.range(0)
  y1=!y.range(1)
  bad=where((w GE x0) AND (w LE x1) AND (f GE y0) AND (f LE y1), count_bad)
  aarin.data(bad).wave=0.
  good=where(aarin.data.wave GT 0., count_good)
  newaar=define_aar(length=count_good,header=aarin.header)
  newaar.data=aarin.data(good)
  return, newaar
END
