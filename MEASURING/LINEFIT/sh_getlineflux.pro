;#> sh_getlineflux.dc3
; Identifier   sh_getlineflux
;
; Purpose      Quick and dirty tool to measure line fluxes in aars
;
; Synopsis     lli=sh_getlineflux(aar [,xrange=xrange] [,velzero=velzero])
;
; Arguments    Name   I/O Type:     Description:
;              ----------------------------------------------------------
;              aar    I   aar       aar with line to be measured
;              xrange I   fltarr(2) plot x range to isolate lines out of spectra
;              velzero I   float    zero point for velocity scale
;
; Returns      lli with line flux, center, and continuum value at center
;              
; Description  This is a quick and dirty tool to measure a line flux in an aar.
;              There is no assumption about line shape made.
;              The aar is internally converted to a flambda scale.
;              Then a plot of the aar piece is created. The user has to
;              identify by cursor clicks two continuum segments left and right
;              of the line. The aar is then replotted with continuum
;              subtracted, as derived from a linear fit to the data points
;              enclosed by these two ranges.
;              The line range has then to be identified by two more cursor 
;              clicks, and the chosen range is replotted for information.
;              Line flux and line center (=center of gravity) are then
;              derived by integration, dealing with nonequidistant sampling 
;              in the spirit of IA integrate_flux. The continuum (at the line 
;              center) is determined and all values stored in a linelist.
;
;              At the top of the plot, a velocity scale is shown for 
;              convenience and to assist in consistent treatment of several
;              lines. By default, its zero point is in the middle of the
;              range, but it can also be enforced using the velzero
;              keyword 
;              
; Comment      
;
; Example      lli=getlineflux(aar,xr=[14,14.5],velzero=14.3217)
;
; Category     UTIL
;
; Filename     getlineflux.pro
;
; Author       D.Lutz (DL)
;
; Version      1.5
;
; History      1.0 15-02-96 DL  first version
;              1.1 15-10-96 DL  xrange option for use with SWS01
;              1.2 30-08-97 DL  velocity scale plotted
;              1.3 04-05-98 DL  changed continuum fit
;              1.4 19-08-98 EkW resolve ISAP (skip wave_form, skip spect_form) 
;              1.5 04-12-98 DL  SPR 451: rename wave array,
;                               make work again with input in
;                               w/cm^2/um ;-)
; sh_getlineflux simply removed /double from poly_fit call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright (C) 1996, Max-Planck-Institut fuer extraterrestrische Physik (MPE);
;                    Garching, Germany                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#<
function sh_getlineflux,aar,xrange=xrange,velzero=velzero
;
;aar1=wave_form(aar,'um')
;aar2=spect_form(aar1,'w/cm^2/um')
;
status = read_fits_key( aar.header,'TUNIT1',unit,comment)
if strpos(strlowcase(unit),'um') ne 0 then error,'F',$
   ' Your input wave unit is not um !'
;
status = read_fits_key( aar.header,'TUNIT2',unit,comment)
if strpos(strlowcase(unit),'jy') eq 0 then begin 
  aar2           = aar
  aar2.data.flux = aar.data.flux * 2.997925E-16 / aar.data.wave^2
endif else if strpos(strlowcase(unit),'w/cm^2/um') eq 0 then begin
  aar2           = aar
endif else error,'F',' Your input flux unit is neither Jy nor w/cm^2/um!'
;
if keyword_set(xrange) then aar2=select(aar2,aar2.data.wave ge xrange(0) and $
  aar2.data.wave le xrange(1))
;
if not keyword_set(velzero) then $
  velzero=(min(aar2.data.wave)+max(aar2.data.wave))/2.
;
plot,aar2.data.wave,aar2.data.flux,linestyle=0,xtitle='Wavelength [um]',$
ytitle='Flux density [W/cm^2/um]',/ynozero,xstyle=8
axis,xaxis=1,xrange=300000.*(!x.crange-velzero)/velzero,/xstyle,$
xtitle='Velocity [km/s]'
;
print,' '
print,'Definition of left and right continuum ranges:'
print,'click to left edge of left continuum'
cursor,xll,y,/down
print,'click to right edge of left continuum'
cursor,xlr,y,/down
print,'click to left edge of right continuum'
cursor,xrl,y,/down
print,'click to right edge of right continuum'
cursor,xrr,y,/down
index = where((aar2.data.wave ge xll and aar2.data.wave le xlr) $
           or (aar2.data.wave ge xrl and aar2.data.wave le xrr))
coeff=poly_fit(aar2.data(index).wave,1.e18*aar2.data(index).flux,1)
coeff=coeff/1.e18
aar3=aar2
aar3.data.flux=aar3.data.flux-coeff(0)-coeff(1)*aar3.data.wave
plot,aar3.data.wave,aar3.data.flux,linestyle=0,xtitle='Wavelength [um]',$
ytitle='Flux density [W/cm^2/um]',xstyle=8
axis,xaxis=1,xrange=300000.*(!x.crange-velzero)/velzero,/xstyle,$
xtitle='Velocity [km/s]'
print,' '
print,'Definition of line range:'
print,'click to left edge of line'
cursor,xl,y,/down
print,'click to right edge of line'
cursor,xr,y,/down
aar4=select(aar3,aar3.data.wave ge xl and aar3.data.wave le xr) 
plot,aar4.data.wave,aar4.data.flux,xtitle='um',$
ytitle='Flux density [W/cm^2/um]',xstyle=8
axis,xaxis=1,xrange=300000.*(!x.crange-velzero)/velzero,/xstyle,$
xtitle='Velocity [km/s]'
;
flux=aar4.data.flux
wavele=aar4.data.wave
npoints=n_elements(flux)
intval=fltarr(npoints)
intval(1:npoints-2)=0.5*(wavele(2:npoints-1)-wavele(0:npoints-3))
intval(0)=0.25*(wavele(1)-wavele(0))
intval(npoints-1)=0.25*(wavele(npoints-1)-wavele(npoints-2))
;
lflux=total(flux*intval)
cent=total(flux*intval*wavele)/total(flux*intval)
;
cont=coeff(0)+coeff(1)*cent
print,' '
print,'Flux =     ',lflux,' W/cm^2'
print,'Center =   ',cent,' um'
print,'Continuum =',cont,' W/cm^2/um'
;
lli=define_lli(1,1)
lli.data.source='getlineflux'
lli.data.list(0).flux=lflux
lli.data.list(0).wave=cent
lli.data.list(0).cont=cont
return,lli
end
