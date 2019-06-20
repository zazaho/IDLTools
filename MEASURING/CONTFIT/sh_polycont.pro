; sh_polycont
;make ploynmial continuum fit
function sh_polycont,in,sel,ZOOM=zoom,flambda=flambda,param=param, $
                     _extra=_extra
  
  has_sel = ((n_params() eq 2))
  a = in
  if (has_sel) then begin
    if (is_aar(sel)) then begin 
      insel = sel 
    endif else begin
      insel = a
    endelse
  endif
  
  x1=0
  x2=0
    
;(SH Feb  4 1999)
;Use factor of 1e14 to avoid float problems
  if (keyword_set(flambda)) then begin
    a = sh_calcaar(a,/fl,fact=1e14)
    insel = sh_calcaar(insel,/fl,fact=1e14)
    ytt = 'Flux density [W/m/m/micron]*1e14'
  endif else begin
    ytt = 'Flux density [Jy]'
  endelse
  
start:
  if keyword_set(zoom) then begin
    pl,ytit=ytt,a,_extra=_extra
zoom:    
    print, 'Select the waverange, long to short to zoom.'
    veri,x1,/nostatus
    wait,0.2
    veri,x2,/nostatus
    wait,0.2
    if x1 gt x2 then begin
      print,'Zooming: '
      pl,ytit=ytt,a,_extra=_extra,xr=[x2,x1]
      goto,zoom
    endif
  endif
  
  if (has_sel) then begin
    xc = insel.data.wave
    yc = insel.data.flux
    i = n_elements(xc)
    pl,ytit=ytt,a,_extra=_extra,xr=[x1,x2]
    oplot,ps=4,xc,yc,col=160
    goto,polynome
  endif
  
 continuum:
  
  xc =fltarr(100)
  yc =fltarr(100)
  i = 0
  corder = 0
  !err = 0
    
; plot the data end let user indicate the continuum
  pl,ytit=ytt,a,_extra=_extra,xr=[x1,x2]
  print,'Please indicate the continuum with the left button,', $
    'stop=right button'
  while (!err ne 4) AND (i lt 99)  DO begin
    cursor,x,y,2,/down
    if (!err ne 4) then begin
      xc(i) = x
      yc(i) = y
      print,xc(i),yc(i)
      oplot,ps=4,[xc(i)],[yc(i)],col=160
      i = i + 1
    endif
  EndWhile
    
polynome:
    Print,'Which order of polynome shall I fit (negative for spline)?'
    Read,corder
    
    c=a
    if (corder lt 0) then begin
      c.data.flux = shc_spline(xc(0:i-1),yc(0:i-1),c.data.wave)
      fit = -1
    endif else begin
      fit = poly_fit(xc(0:i-1),yc(0:i-1),corder,outY)
      c.data.flux = poly(c.data.wave,fit)
    endelse
    pl,c,/o
    answer = 'a'
    print,'What next: accept,change order, restart (a/o/r)?'
    read,answer
    case 1 of
      (answer eq 'r') OR (answer eq 'R'): goto,continuum
      (answer eq 'o') OR (answer eq 'O'): goto,polynome
      else :
    endcase
    
    param = fit
    if (keyword_set(flambda)) then c = sh_calcaar(c,fl=-1,fact=1e-14)
    if (has_sel) then begin
      sel = sh_define_aar(length=i)
      sel.header = a.header
      sel.data.wave = xc(0:i-1)
      sel.data.flux =yc(0:i-1)
       if (keyword_set(flambda)) then sel = sh_calcaar(sel,fl=-1,fact=1e-14)
    endif
    return,c
end 
