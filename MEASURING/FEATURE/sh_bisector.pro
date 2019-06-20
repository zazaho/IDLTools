;+
; NAME:
; sh_bisector
; PURPOSE:
;Determine the central peak of an assymetric feature using
; the bisector methode. Using a polynomial extrapolation we measure
; the peak.
; CALLING SEQUENCE:
; c = sh_bisector(aar[,p])
; INPUTS:
; aar = feature spectrum to be measured
; KEYWORD PARAMETERS:
; xrange:      range to be used for measurements
; numlevel:    number of levels to use for bisectoring default 100
; rangelevels: height range of levels to fit default (.5-.95)
; levels:      which levels to fit overwrites numlevels and rangelevels
; order:       polynomial order to fit to the center points  default 1
; /cursor:     use cursor to indicate the peak position
; /noplot:     do not produce plots
; /help:       show usage parameters
; OUTPUTS: 
; c center position
; OPTIONAL OUTPUTS:
; p polynomial parameters y = p(0)+p(1)*x+...
; RESTRICTIONS:
; Use with care on very noisy data especially with much noise in the peak
; EXAMPLE:
; center = sh_bisector(aar)
; center = sh_bisector(aar,xr=[10,12],num=200,rang=[.3,.95],order=2,poly)
; EXTERNAL MODULES USED:
; shc_interpol
; MODIFICATION HISTORY:
; (SH Jun 23 1999) version 1
;-
function sh_bisector,in,p,xrange=xr,numlevels=nl,rangelevels=rl, $
                     levels=levels,order=o,cursor=cursor,noplot=nop, $
                     help=help
  
  printhelp=0
  if n_params() eq 0 then printhelp = 1
  if keyword_set(help) then printhelp = 1
  if (printhelp) then begin
    print,'sh_bisector'
    print,'usage:'
    print,'c = sh_bisector(a[,p])'
    print,'keywords:'
    print,'xrange:      range to be used for measurements'
    print,'numlevel:    number of levels to use for bisectoring default 100'
    print,'rangelevels: height range of levels to fit default (.5-.95)'
    print,'levels:      which levels to fit overwrites numlevels and rangelevels'
    print,'order:       polynomial order to fit to the center points  default 1'
    print,'/cursor:     use cursor to indicate the peak position'
    print,'/noplot:     do not produce plots'
    print,'/help:       show usage parameters'
    return,0
  endif
    
;; specific xrange
  if (n_elements(xr) eq 2) then begin
    a = sh_select_range(in,xr=xr,/q)
  endif else begin
    a =in
  endelse
;; want plots  
  plt = not keyword_set(nop)
;; Number of levels to determine the bisection
  case (n_elements(nl)) of
    0: nl = 10
    1: if (nl lt 2) then nl = 100
    else: if (nl(0) lt 2) then nl = 100 else nl = nl(0)
  endcase
;; the range in levels normalized to the peak  
  if (n_elements(rl) ne 2) then rl = [.5,.95]
  if (n_elements(levels) gt 0) then begin
    levels = levels
  endif else begin
    levels = rl(0)+(rl(1)-rl(0))/(nl-1)*indgen(nl)
  endelse
;; Polynomial order  
  case (n_elements(o)) of
    0: o = 1
    1: o = o
    else: o = o(0)
  endcase
;; Use cursor the deterimine the peak otherwise the highest point is
;; the peak
  if keyword_set(cursor) then begin
    pl,a
    print,'indicate peak position'
    Cursor,x,y,2,/down
  endif else begin
    y = max(a.data.flux,x)
    x = a.data(x).wave
  endelse
  
;; Scale the normalized levels   
  levels = y*levels
;; Get the two halves
  a1=  sh_select_range(a,xr=[0.,x],/q)
  a2=  sh_select_range(a,xr=[x,200.],/q)
;; Interpolate to the levels  
  x1=  shc_interpol(a1.data.flux,a1.data.wave,levels)
  x2=  shc_interpol(a2.data.flux,a2.data.wave,levels)
;; get the center position  
  c = (x1+x2)/2d0
;; Fit a polynomial to the center positions  
  p = poly_fit(levels,c,o,yfit)
;; Determine the polynomial at the peak(y)  
  r = poly(y,p)
  if (plt) then begin
    pl,a
    oplot,x1,levels,col=60
    oplot,x2,levels,col=60
    oplot,c,levels
    oplot,yfit,levels,color=100
  endif
  return,r
end
