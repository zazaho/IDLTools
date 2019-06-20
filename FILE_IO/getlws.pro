function getlws,fn,lwsdir=lwsdir,_extra=_extra
  
  if keyword_set(lwsdir) then fn = shell_expand('$DATA/LWS/'+fn)
  sh_aar2ascii,t,fn,/back,_extra=_extra
  t.data.line = 1000
  t = sh_calcaar(t,fl=-1,fac=1e-15)
  return,t
  
end
