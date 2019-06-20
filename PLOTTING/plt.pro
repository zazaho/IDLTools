;PRO plt, arb, AFF=aff, BAND=band, AOT_BANDS=aot_bands, $
;      RANGE=range, XRANGE=xrange, YRANGE=yrange, WNUM=wnum, $
;      TITLE=title, XTITLE=xtitle, YTITLE=ytitle, $
;      PSFILE=psfile, OPLOT=oplot, NOPLOT=noplot, $
;      THICK=thick, YZERO=yzero, YNOZERO=ynozero, NOSTAMP=nostamp, $
;      UNIT=unit, XLOG=xlog, YLOG=ylog, $
;      XOFFSET=xoffset, YOFFSET=yoffset, YSCALE=yscale, $
;      PSYM=psym, COLOR=color, FPSYM=fpsym, FCOLOR=fcolor, AOT_COLOR=aot_color
;
;  97/02/06 I.Yamamura
;  97/04/08 area redefined
;  97/04/28 plot username/date for PS output by default. NOSTAMP added
;  97/05/07 use style parameters to plot in exactly specified range.
;  97/05/07 add PSFILE to output to PostScript directly.
;  97/05/07 add OPLOT.
;  97/05/22 add NOPLOT.
;  97/05/22 change wavelength range corresponding to band option
;  97/05/22 idx_aot_band => indband.pro
;  97/05/22 can treat merged spectrum correctly
;  97/05/26 add XOFFSET and YOFFset
;  97/06/10 add YSCALE
;  97/07/15 avoid error when no data selected.
;  97/10/02 avoid to allocate the same color for bands when !P.MULT != 0
;  98/01/15 yoffset must be applied after yscale

;
; -------------------------------------------------------------------
;  subroutines
; -------------------------------------------------------------------
Function in_range, aot_band, range, WNUM=wnum
; return TRUE if aot_band is included in the (wavelength) range
; aot_band : AOT_BAND
; range: wavelength range [from, to]
; WNUM: in wavenumber

case strupcase(aot_band) of
   '0'  : ara = [2.35, 46.0]
   '1A' : ara = [2.35, 2.65]
   '1B' : ara = [2.55, 3.10]
   '1D' : ara = [2.90, 3.60]
   '1E' : ara = [3.40, 4.20]
   '2A' : ara = [3.90, 5.70]
   '2B' : ara = [5.20, 7.10]
   '2C' : ara = [6.80, 13.0]
   '3A' : ara = [11.5, 17.0]
   '3C' : ara = [15.5, 20.0]
   '3D' : ara = [19.0, 28.0]
   '3E' : ara = [27.0, 30.0]
   '4'  : ara = [28.0, 46.0]
endcase

if (keyword_set(WNUM)) then begin
   tmp = ara(0)
   ara(0) = 10000.0 / ara(1)
   ara(1) = 10000.0 / tmp
endif

if (ara(0) gt range(1) OR ara(1) lt range(0)) then return, 0 $
else                                               return, 1

end

; ======================================================================
;  Here comes main routne!
; ======================================================================
PRO plt, arb, AFF=aff, BAND=band, AOT_BANDS=aot_bands, $
      RANGE=range, XRANGE=xrange, YRANGE=yrange, WNUM=wnum, $
      TITLE=title, XTITLE=xtitle, YTITLE=ytitle, $
      THICK=thick, YZERO=yzero, YNOZERO=ynozero, NOSTAMP=nostamp, $
      PSFILE=psfile, OPLOT=oplot, NOPLOT=noplot, $
      UNIT=unit, XLOG=xlog, YLOG=ylog, $
      XOFFSET=xoffset, YOFFSET=yoffset, YSCALE=yscale, $
      PSYM=psym, COLOR=color, FPSYM=fpsym, FCOLOR=fcolor, AOT_COLOR=aot_color
; YNOZERO : for compatibility. not used. (is default).

C = 2.99792458e10

pmulti=!P.MULTI         ; 97/10/02 avoid uni-colored
!P.MULTI = [0,0,0,0,0]
aot_band_color=color_selection(/color, det=indgen(12))
!P.MULTI=pmulti

; make internal copy of data
a = arb

; change wavelength to wavenumber
if (keyword_set(WNUM)) then a.data.wave = 10000.0 / a.data.wave

; set unit (default = Jy)
if (not keyword_set(UNIT)) then UNIT = 'JY'
case strupcase(unit) of
   'JY'   : 
   'FLMD' : a.data.flux = a.data.flux / a.data.wave^2 * C * 1.0e-22
   'LFL'  : a.data.flux = a.data.flux / a.data.wave   * C * 1.0e-22
   'L4F'  : a.data.flux = a.data.flux * a.data.wave^2 * C * 1.0e-22
    else  : print, "Allowed UNITs are, 'JY'(default), 'FLMD'(W m-2 s-1), 'LFL'(W m-1 s-1), 'L4F'(W m2 s-1)"
endcase

; set display X-range
if (n_elements(xrange) ne 2) then begin
   if (!x.range(0) ne 0 AND !x.range(1) ne 0) then begin
      xrange = !x.range
   endif else if (keyword_set(band)) then begin
           if (band eq 1) then xrange = [ 2.3,  4.3] $
      else if (band eq 2) then xrange = [ 3.5, 12.5] $
      else if (band eq 3) then xrange = [11.0, 30.0] $
      else if (band eq 4) then xrange = [25.0, 50.0] $
      else if (band eq 9) then xrange = [ 7.5, 24.0] $ ; LRS range
      else                     xrange = [ 2.0, 50.0]
      if (keyword_set(WNUM)) then begin
         xtmp = xrange(0)
         xrange(0) = 10000.0 / xrange(1)
         xrange(1) = 10000.0 / xtmp
      endif
   endif
endif

; if aot_bands are not given, all bands are assumed first
if (n_elements(aot_bands) eq 0) then $
   aot_bands = ['1a','1b','1d','1e','2a','2b','2c','3a','3c','3d','3e','4']

; merged data cannot separete into bands
idx=where(a.data.det ne 0, count)
if (count eq 0) then aot_bands = '0'

; select specified aot_bands data
a = selbands(a, aot_bands, count=count)
if (count eq 0) then goto, EXIT

if (keyword_set(XOFFSET)) then a.data.wave = a.data.wave + xoffset
if (keyword_set(YSCALE))  then a.data.flux = a.data.flux * yscale
if (keyword_set(YOFFSET)) then a.data.flux = a.data.flux + yoffset

; set range
if (n_elements(range) ne 2) then begin
   if (n_elements(xrange) eq 2) then range = xrange $
   else begin
      idx = indband(a, aot_bands(0), count=count)
      for i = 1, n_elements(aot_bands)-1 do $
         idx = [idx, indband(a, aot_bands(i), count=count)]
      mm = min(a.data(idx).wave, MAX=mx)
      dd = mx - mm
      if (keyword_set(XLOG)) then range = [mm * 0.7, mx * 1.3] $
      else                        range = [mm - dd * 0.1, mx + dd * 0.1]
   endelse
endif

; range overridges above setting for X-range
if (n_elements(range) eq 2) then xrange = range

; automatic Y-range setting
if (n_elements(yrange) ne 2) then begin
   if (!y.range(0) ne 0 AND !y.range(1) ne 0) then begin
      yrange = !y.range
   endif else begin
      idx = where(a.data.wave ge range(0) and a.data.wave le range(1),count)
      if (count gt 0) then begin
         mm = min(a.data(idx).flux, MAX=mx)
         dd = mx - mm
         if (keyword_set(YLOG)) then yrange = [mm * 0.7, mx * 1.3] $
         else                        yrange = [mm - dd * 0.1, mx + dd * 0.1]
      endif else yrange = [0.0, 1.0]
   endelse
endif

; select aot_bands in the given wavelength range
for i = 0, n_elements(aot_bands)-1 do begin
   if (in_range(aot_bands(i), range, WNUM=wnum)) then $
      if (n_elements(s_aot_bands) eq 0) then s_aot_bands = aot_bands(i) $
      else                     s_aot_bands = [s_aot_bands, aot_bands(i)]
endfor
if (n_elements(s_aot_bands) eq 0) then begin
   print, 'No data in the range!!'
   goto, EXIT
endif else aot_bands = s_aot_bands

; set keywords for plot
if ( not keyword_set(TITLE))   then title   = ' '
if (n_elements(xtitle) eq 0)  then begin
   if (keyword_set(WNUM)) then xtitle  = 'Wavenumber (cm-1)' $
                          else xtitle  = 'Wavelength (!4l!3m)'
endif
if (n_elements(ytitle) eq 0)  then begin
   case strupcase(unit) of
      'JY'   : ytitle  = 'Flux Density (Jy)'
      'FLMD' : ytitle  = 'W m-2 s-1'
      'LFL'  : ytitle  = 'W m-1 s-1'
      'L4F'  : ytitle  = 'W m2 s-1'
       else  : ytitle  = ' '
   endcase
endif

if (keyword_set(XLOG))   then xtype = 1 else xtype = 0
if (keyword_set(YLOG))   then ytype = 1 else ytype = 0
if ( not keyword_set(THICK))   then thick = 1
if ( not keyword_set(YZERO))   then ynozero = 1 else ynozero = 0
if ( not keyword_set(COLOR))   then color = !p.color
if ( not keyword_set(FCOLOR))  then fcolor = !p.color
if ( not keyword_set(PSYM))    then psym = 0
if ( not keyword_set(FPSYM))   then fpsym = 3

if (keyword_set(OPLOT)) then goto, DOOPLOT

; set style 97/05/07
!X.STYLE = 1
!Y.STYLE = 1

; if output to PostScrit file 97/05/07.
if (keyword_set(PSFILE) and !D.NAME eq 'X') then begin
   if (n_elements(psfile) ge 1) then pson, file=psfile $
   else                              pson
endif

; Let's open the graphic window
plot, xrange, yrange, /NODATA, YNOZERO=ynozero, $
      XRANGE=xrange, YRANGE=yrange, $
      XTYPE=xtype, YTYPE=ytype, $
      XTITLE=xtitle, YTITLE=ytitle, TITLE=title
if (keyword_set(NOPLOT)) then goto, DATE

DOOPLOT:
; Now, plot the data
for i = 0, n_elements(aot_bands)-1 do begin
   idx = indband(a, aot_bands(i), count=count)
   if (keyword_set(AOT_COLOR)) then color=aot_band_color(i)
   if (count gt 0) then $
      oplot, a.data(idx).wave, a.data(idx).flux, $
      THICK=thick, PSYM=psym, COLOR=color
endfor

; if aff data is given, plot overlay
if (keyword_set(AFF)) then begin
   if (n_elements(aff) eq 1) then af = aff
   if (keyword_set(WNUM)) then af.data.wave = 10000.0 / af.data.wave

   for i = 0, n_elements(aot_bands)-1 do begin
      idx = indband(af, aot_bands(i), count=count)
      if (keyword_set(AOT_COLOR)) then fcolor=aot_band_color(11-i)
      if (count gt 0) then $
         oplot, af.data(idx).wave, af.data(idx).flux, $
         THICK=thick, PSYM=fpsym, COLOR=fcolor
   endfor
endif
if (keyword_set(OPLOT)) then goto, EXIT

DATE:
;plot time- + user-stamp, down/unther the hardcopy
;taken from Nico Sijm's plot_info
 if (not keyword_set(NOSTAMP)) then begin
  if !d.name ne 'X' then begin
    stamp='!8'+getenv('USER')+' / !3!N'+'!3'+strmid(!stime,0,20)
    xyouts,1.04,-0.04, stamp, size=0.75,/norm,orient=0,alig=1.0
  endif
 endif

EXIT:

; close PostScrit file 97/05/07.
if (keyword_set(PSFILE) and !D.NAME eq 'PS') then psoff

end
