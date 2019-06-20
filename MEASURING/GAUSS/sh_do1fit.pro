function sh_do1fit,in,d,sigmad,estim=estim,xrange=xrange,restart=restart, $
                   spec=spec,errory=errory,_extra=_extra
  a = in
  if keyword_set(xrange) then BEGIN
    IF keyword_set(errory) THEN BEGIN
      idx = where( (a.data.wave GT xrange(0)) AND (a.data.wave LT xrange(1)) )
      errory = errory(idx)
    ENDIF
    a = sh_select_range(a,xr=xrange,/q)
  endif
  x = a.data.wave
  y = a.data.flux
  if ((n_elements(estim) eq 3) and (not keyword_set(restart))) then begin
    d = estim
  endif else begin
    pl,a
    print,'please indicate an estimate of the peak'
    Cursor,px1,py1,2,/down
    oplot,ps=4,[px1],[py1],col=160
    print,' now indicate the widths'
    Cursor,wx1a,wy1a,2,/down
    oplot,ps=4,[wx1a],[wy1a],col=160
    Cursor,wx1b,wy1b,2,/down
    oplot,ps=4,[wx1b],[wy1b],col=160
    
    estim = [py1,px1,abs(wx1a-wx1b)/2.35482]
    d = estim
  endelse
  
  IF keyword_set(errory) THEN BEGIN
    yfit = unogaussfit(x,y,d,sigmad,errory=errory,_extra=_extra)
  ENDIF ELSE BEGIN
    yfit = unogaussfit(x,y,d,sigmad,_extra=_extra)
  ENDELSE
  
  d[2] = abs(d[2])
  pl,a
  g = sh_gauss(in,cent=d(1),heig=d(0),width=abs(d(2))/2.35482,/ja)
  pl,g,/o
  spec=g
  return,yfit
end
