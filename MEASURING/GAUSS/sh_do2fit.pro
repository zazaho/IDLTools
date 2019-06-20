function sh_do2fit,in,d,sigmad,estim=estim,xrange=xrange,restart=restart, $
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
  if ((n_elements(estim) eq 6) and (not keyword_set(restart))) then begin
    d = estim
  endif else begin
    pl,a
    print,'please indicate an estimate of the 2 peaks'
    Cursor,px1,py1,2,/down
    oplot,ps=4,[px1],[py1],col=160
    Cursor,px2,py2,2,/down
    oplot,ps=4,[px2],[py2],col=160
    print,' now indicate the widths'
    
    Cursor,wx1a,wy1a,2,/down
    oplot,ps=4,[wx1a],[wy1a],col=160
    Cursor,wx1b,wy1b,2,/down
    oplot,ps=4,[wx1b],[wy1b],col=160
    
    Cursor,wx2a,wy2a,2,/down
    oplot,ps=4,[wx2a],[wy2a],col=160
    Cursor,wx2b,wy2b,2,/down
    oplot,ps=4,[wx2b],[wy2b],col=160
    
    estim = [py1,px1,abs(wx1a-wx1b)/2.35482,py2,px2,abs(wx2a-wx2b)/2.35482]
    d = estim
  endelse
  
  IF keyword_set(errory) THEN BEGIN
    yfit = dosgaussfit(x,y,d,sigmad,errory=errory,_extra=_extra)
  ENDIF ELSE BEGIN
    yfit = dosgaussfit(x,y,d,sigmad,_extra=_extra)
  ENDELSE
  pl,a
  g1 = sh_gauss(a,cent=d(1),heig=d(0),width=abs(d(2))/2.35482,/ja)
  g2 = sh_gauss(a,cent=d(4),heig=d(3),width=abs(d(5))/2.35482,/ja)
  g = add(g1,g2)
  pl,g1,/o
  pl,g2,/o
  pl,g,/o
  spec=g
  return,yfit
end
