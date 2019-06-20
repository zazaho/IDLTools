FUNCTION measure_continuum,cont
  foo=sh_bbfit(cont,temp,power)
  return,[temp,power]
END
