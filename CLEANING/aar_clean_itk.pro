FUNCTION aar_clean_itk, in,xrange=xrange,yrange=yrange,_extra=_extra,name=name
  
;  This program presents the aar data in itk order.
;  this way the (mini-)glitches and other correlated noise event
;  can be easily spotted and removed.
;  it does the following things:
;  - present you with the data itk-flux
;  - allow you to select data to be thrown out
;  - use the r-mouse-button to zoom
  
;  after selection the result is plotted and one is given the choice to apply
;  or cancel ( left button/right button )
;  Next to continu or quit (l/r)
  

; Check for valid input
  IF NOT is_aar(in) THEN error,'F','No valid AAR structure specified!'
  
; Initialise, remember structures are passed by reference so alway
; keep the original input.
  aar = in
  
  w1 = !d.window
  IF (w1 EQ 0 ) THEN w2 = 1 ELSE w2 = 0 
  
  default,xrange,[0,0] 
  xl = xrange
  
  default,yrange,[0,0] 
  yl = yrange
  
  IF (NOT keyword_set(name)) THEN name = 'aci'
  
  zero = sh_itk0(aar)
  wegitks = replicate({l1:0L,l2:0L},100)
  itki = 0
  
jump1:
  
; first plot the data itk-flux
  wset,w1
  pl,aar,/time,/det,ps=1,tit='aar_clean_itk screen',xr=xl,yr=yl,_extra=_extra
;  plot,aar.data.itk-zero,aar.data.flux,xr=xl,yr=yl,/ys,ps=1,/xs,$
;    tit='aar_clean_itk screen',_extra=_extra
  print,'select the itk-limits to be excluded'
  print,'use the right mouse button to zoom'
  
  !err = 0
  zooming = 0
  Cursor,x1,y,2,/down
  IF (!err EQ 4) THEN zooming = 1
  !err = 0
  cursor,x2,y,2,/down
  IF (x1 GT x2) THEN BEGIN
    x3 = x1
    x1 = x2
    x2 = x3
  ENDIF
  
  IF (zooming EQ 1) THEN BEGIN
    xl = [x1,x2]
    print,'zooming between: ',long(xl(0))+zero,' and ',long(xl(1))+zero
    GOTO,jump1
  ENDIF
  
  out = sh_select(aar,(aar.data.itk - zero LT x1) OR $
                   (aar.data.itk - zero GT x2))
  
;  oplot,out.data.itk-zero,out.data.flux,ps=1,col=160
  pl,/opl,out,/white,ps=1
  
  window,w2
  pl,aar,tit='aar_clean_itk result',yr=yl,ps=1
  pl,out,/opl,ps=1,/scan
;  pl,aar,ps=1,tit='aar_clean_itk result',yr=yl
;  pl,getscan(out,/down),ps=1,/opl
;  pl,getscan(out,/up),ps=1,/opl
  wset,w1
  
  print,'Accept, reject (l/r)?'
  !err = 0
  Cursor,x,y,2,/down
  IF (!err EQ 1) THEN BEGIN
    aar = out
    wegitks[itki].l1 = long(x1)+zero
    wegitks[itki].l2 = long(x2)+zero
    print,format='("Removed data between :",I12," and ",I12)', $
      wegitks[itki].l1,wegitks[itki].l2
    itki = itki+1
  ENDIF
  
  print,'Again, Again with same limits, stop (l/m/r)?'
  !err = 0
  Cursor,x,y,2,/down
  IF (!err EQ 2) THEN BEGIN
    GOTO,jump1
  ENDIF ELSE BEGIN 
    IF (!err EQ 1) THEN BEGIN
      xl = xrange
      GOTO,jump1
    ENDIF
  ENDELSE
  
;  if (itki gt 0) then begin
;    print,'removed parts:'
;    for i = 0,itki-1 do begin
;      print,format='(I12," to ",I12)',$
;        wegitks(i).l1,wegitks(i).l2
;    endfor
;  endif
  
  IF (itki GT 0) THEN BEGIN
    print,'removed parts:'
    FOR i = 0,itki-1 DO BEGIN
      print,name,'= sh_weg(',name,',itk=[', $
        wegitks[i].l1,',',wegitks[i].l2,'])'
    ENDFOR
  ENDIF
    
  return,aar
END
