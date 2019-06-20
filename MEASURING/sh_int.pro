FUNCTION sh_int,in,xrange=xrange,quiet=quiet
  
    a = in

  ; Take only the part of interest
  IF keyword_set(xrange) THEN BEGIN
    a = select( a,(a.data.wave GE xrange(0)) AND $
                      (a.data.wave LE xrange(1)) ) 
  ENDIF

;get dimensions and wavelengths from a
  flx=a.data.flux
  wv =a.data.wave

;; sort according to flux
  idx = sort(wv)
  flx = flx[idx]
  wv  = wv[idx] 

  npoints=n_elements(flx)
  intval=fltarr(npoints)
    
; now calculate the line properties  
;(SH Jan  5 1999) gejat van getlineflux.pro
  intval(1:npoints-2)=0.5*(wv(2:npoints-1)-wv(0:npoints-3))
  intval(0)=0.25*(wv(1)-wv(0))
  intval(npoints-1)=0.25*(wv(npoints-1)-wv(npoints-2))
  integr=total(flx*intval)

  IF NOT keyword_set(quiet) THEN print,'Integrated',integr
  return,integr
END
