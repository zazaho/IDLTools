FUNCTION sh_inttab,wave,flux
  
  flx = 1d14*(flux*3d14/wave^2d0*1d-26)
  wv = wave

  npoints=n_elements(flx)
  intval=fltarr(npoints)
  
; now calculate the line properties  
;(SH Jan  5 1999) gejat van getlineflux.pro
  intval(1:npoints-2)=0.5*(wv(2:npoints-1)-wv(0:npoints-3))
  intval(0)=0.25*(wv(1)-wv(0))
  intval(npoints-1)=0.25*(wv(npoints-1)-wv(npoints-2))
  totalflux=total(flx*intval)

  return,totalflux*1e-14
END
