PRO fill_between,ain,bin,x,xrange=xrange,_extra=_extra
  
  COMMON COMMON_PLS, sh_plotcount,sh_plotflambda,sh_plotiras,sh_plotcm, $
    sh_plottime,sh_plotnu,sh_plotplambda
  
  IF (n_elements(x) EQ 2) THEN BEGIN
    xrange = x
  ENDIF
  
  a = ain
  b = bin
  
  IF (n_elements(xrange) EQ 2) THEN BEGIN
    a = sh_select_range(a,xr=xrange,/q)
    b = sh_select_range(b,xr=xrange,/q)
  ENDIF
  
  if (sh_plotflambda) then begin
    a.data.flux = 3e-12*a.data.flux/a.data.wave^2d0*1d13
    b.data.flux = 3e-12*b.data.flux/b.data.wave^2d0*1d13
  endif
  
  if (sh_plotiras) then begin
    a.data.flux = 3e-12*a.data.flux/a.data.wave*1d12
    b.data.flux = 3e-12*b.data.flux/b.data.wave*1d12
  endif
  
  if (sh_plotcm) then begin
    a.data.wave = 1d4/a.data.wave
    b.data.wave = 1d4/b.data.wave
  endif
  
  ax = a.data.wave
  ay = a.data.flux
  
  bx = b.data.wave
  by = b.data.flux
  
  cx = [ax,reverse(bx)]
  cy = [ay,reverse(by)]
  
  polyfill,cx,cy,_extra=_extra
END
