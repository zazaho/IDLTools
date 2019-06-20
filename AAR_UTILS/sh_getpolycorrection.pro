; Thingy to get 1 order polynoom correction to align bands
; Usage indicate the wrong slope, and fit 1 order polynoom
; indicate where it should be
; calculate the correction params for offset and scaling
FUNCTION sh_getpolycorrection,aar,_extra=_extra
;init
  xc  =fltarr(100)
  yc =fltarr(100)
  i = 0
  !err = 0

; plot the data end let user indicate the continuum
  pl,aar,_extra=_extra,tit='SH_GETPOLYCORRECTION'
  print,'Please indicate the uncorrected points with the left button,', $
    'stop=right button'
  WHILE (!err ne 4) AND (i lt 99)  DO begin
    cursor,x,y,2,/down
    if (!err ne 4) then begin
      xc[i] = x
      yc[i] = y
      oplot,ps=4,[xc[i]],[yc[i]],col=!d.table_size*.85
      i = i + 1
    endif
  EndWhile
  uncor = poly_fit(xc[0:i-1],yc[0:i-1],1,outuncor)
  oplot,xc[0:i-1],outuncor,ps=0,col=!d.table_size*.85
; for the multiplication we need the endpoints of the uncor
  max_uncor = max(xc[0:i-1],min=min_uncor)
  
  i = 0
  !err = 0
  print,'Please indicate the corrected points with the left button,', $
    'stop=right button'
  WHILE (!err ne 4) AND (i lt 99)  DO begin
    cursor,x,y,2,/down
    if (!err ne 4) then begin
      xc[i] = x
      yc[i] = y
      oplot,ps=4,[xc[i]],[yc[i]],col=!d.table_size*.5
      i = i + 1
    endif
  EndWhile
  cor = poly_fit(xc[0:i-1],yc[0:i-1],1,outcor)
  oplot,xc[0:i-1],outcor,ps=0,col=!d.table_size*.5
  
  offs = [cor[0]-uncor[0],cor[1]-uncor[1]]
  multi = poly_fit([min_uncor,max_uncor], $
                   poly([min_uncor,max_uncor],cor)/ $
                   poly([min_uncor,max_uncor],uncor),1)
  print,'Polynome parameters for offset correction: ',offs[0],offs[1]
  print,'Polynome parameters for scaling correction: ',multi[0],multi[1]
  out = {offset:offs,scaling:multi}
;  return,[offs,multi]
  return,out
END
