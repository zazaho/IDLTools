FUNCTION int_qab,temp,lnk,xrange=xrange

  default,xrange,[10,50]
  
  q = c_abs(1d6,lnk,/cde)/(!dpi*1d6^2d0)
  l = lnk[0,*]
  nl = n_elements(l)
  l = reform(l,nl)

  a1 = 3.97296d19
  a2 = 1.43875d4
  bb = a1/(l^3d0)/(exp(a2/(l*temp))-1d0)
  
  qab = sh_define_aar(len=nl)
  qab.data.wave=l
  qab.data.flux = q*bb

  int_qab = sh_integrate(qab,xrange=xrange,/nopl,/quiet)
  
  return,int_qab
END
