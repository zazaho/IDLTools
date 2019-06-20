;+
; NAME: sh_integrate
;
; PURPOSE: take an aar and integrate the values
;
; CATEGORY: Data reduction
;
; CALLING SEQUENCE:
;    sh_integrate,aar
;              [,cont=cont]
;              [,xrange=xrange]
;              [,/noplot]
;              [,/noconvert]
;              [,/quiet]
;              [,/help]
;
; INPUTS:
;    aar input aar structure can optionally be a simple 2d table
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;    cont: structure (table) to be subtracted as cont prior to integration
;    xrange=xrange: range to integrate between
;    /noplot: if set do not plot the result
;    /noconvert:  by default the units are assumed to be Jy - mum.
;                 Prior to integration the y values are converterd to F_lambda to yield
;                 watt/m^2 as. With this set this conversion if not done
;    /quiet: Do not print any thing
;    /help: if set show this help
;
; OUTPUTS:
;  The return value contain the resulting value from the integration 
;
; EXAMPLE:
;   inti = sh_integrate(hd56,xrange=[10,30])
;
; MODIFICATION HISTORY:
;
;-

FUNCTION sh_integrate,in,cont=cont,xrange=xrange,noplot=noplot, $
                      noconvert=noconvert,quiet=quiet,help=help, $
                      _extra=_extra
  

  type_in  = size(in,/tname)
  IF type_in NE 'STRUCT' THEN BEGIN
     ndim_in = size(in,/n_dim)
     IF ndim_in GE 2 THEN BEGIN
        a = sh_define_aar(len=n_elements(in[0,*]))
        a.data.wave = reform(in[0,*])
        a.data.flux = reform(in[1,*])
     ENDIF ELSE BEGIN
        help = 1
     ENDELSE
  ENDIF ELSE BEGIN
     a = in
  ENDELSE

  IF (n_elements(cont) GT 0) THEN BEGIN

     type_cont  = size(cont,/tname)
     IF type_cont NE 'STRUCT' THEN BEGIN
        ndim_cont = size(cont,/n_dim)
        IF ndim_cont GE 2 THEN BEGIN
           c = sh_define_aar(n_elements(cont[0,*]))
           c.data.wave = cont[0,*]
           c.data.flux = cont[1,*]
        ENDIF ELSE BEGIN
           help = 1
        ENDELSE
     ENDIF ELSE BEGIN
        c = cont
     ENDELSE
     
     IF n_elements(c) NE 0 THEN BEGIN
        a = subtract(a,c,quiet=quiet)
     ENDIF
  ENDIF
  
  a = sh_select(a,finite(a.data.flux))

  IF keyword_set(help) THEN BEGIN
     doc_library,'sh_integrate'
     return,0
  ENDIF

  IF NOT keyword_set(noconvert) THEN BEGIN
;(SH Feb  4 1999)
;Use factor of 1d14 to avoid float problems
     a = sh_calcaar(a,/fl,fact=1d14,quiet=quiet)
  ENDIF

  ;;; Take only the part of interest
  IF keyword_set(xrange) THEN BEGIN
     a = sh_select( a,(a.data.wave GE xrange[0]) AND $
                    (a.data.wave LE xrange[1]) )
  ENDIF
  
  IF (NOT keyword_set(noplot)) THEN pl,a,_extra=_extra
  
;get dimensions and wavelengths from a
  flx=a.data.flux
  wv =a.data.wave

  if (n_elements(wv) lt 3) then begin
     if NOT keyword_set(quiet) then begin
        message,/info,'not enough points to do an integration'
     endif
     return,!values.d_nan
  endif

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
  totalflux=total(flx*intval)

  IF NOT keyword_set(noconvert) THEN BEGIN
     totalflux = totalflux*1d-14
     units = ' W/m/m'
  ENDIF ELSE BEGIN
     units = ''
  ENDELSE
  
  IF NOT keyword_set(quiet) THEN BEGIN
     print,'Integrated Flux',totalflux,units
  ENDIF

  return, totalflux
END
