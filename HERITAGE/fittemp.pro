function fittemp,wave,flux,_extra=_extra
  r = sh_bbfit(wave,flux,temperature,_extra=_extra)
  return,temperature
end
