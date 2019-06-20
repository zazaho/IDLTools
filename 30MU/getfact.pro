FUNCTION getfact,lnk,temp,fct
  out=median((div(fact(sim_spec(lnk,/cde,temp=temp),fct,/quiet),sim_spec(lnk,/cde,temp=temp,/nonorm),/quiet)).data.flux) 
  print,out
  return,out
END 
