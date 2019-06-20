PRO sh_plotdet_func,a,last=last,_extra=_extra
  
  object=object(a)

;  w = a.data.wave
;  f = a.data.flux
  det=a.data.det
  line=a.data(0).line
  sdir=a.data(0).sdir

  dets = sh_uniq(det)
  
  FOR i=0,(n_elements(dets)-1) DO BEGIN
;    idx=where(det EQ dets(i))
;    plot,w(idx),f(idx), title='DET='+STRING(dets(i)), $
;      _extra=_extra
    pl,a,adet=dets(i),xtit='',ytit='',title='DET='+STRING(dets(i)), $
      _extra=_extra  
  ENDFOR
  
  xyouts, 0.12, 0.94, '!5line'+STRING(sdir*line)+' of '+ object+'!X', $
    /normal, charsize=3

  IF (n_elements(last) GT 0) THEN BEGIN
    IF (strlowcase(!d.name) eq 'x') AND (last NE 1) THEN window,!d.window+1
  ENDIF
END

PRO sh_plotdet, aar_in,line_in=line_in,auto=auto,noprompt=noprompt, $
                xrange=xrange,yrange=yrange,updown=updown,_extra=_extra

  xold = !x
  yold = !y
  pold = !p
  
  !x.style=1
  !y.style=1
  !x.margin=2
  !x.omargin=10
  !y.margin=2.3
  !y.omargin=5
  !p.charsize=2
  !x.ticks=3
  
  w = !d.window > 0
  
  a=aar_in

  wave=a.data.wave
  flux=a.data.flux
  line=a.data.line
  sdir=a.data.sdir
  
  IF (NOT keyword_set(line_in)) THEN BEGIN
    line_in = sh_uniq(line)
  ENDIF
  
  prompt = (NOT keyword_set(noprompt)) AND (strlowcase(!d.name) eq 'x')
  l = line_in
  
  FOR i = 0,n_elements(l)-1 DO BEGIN
    idx = where(line EQ l(i))
    ; Do we want the scandir seperate or together updown
    IF NOT keyword_set(updown) THEN BEGIN
      s = sh_uniq(sdir(idx))
    ENDIF ELSE BEGIN
      s = 0
    ENDELSE
    
    wset,w
    
    IF (keyword_set(auto)) THEN BEGIN
      !x.range = [min(a.data(idx).wave),max(a.data(idx).wave)]
      !y.range = [min(a.data(idx).flux),max(a.data(idx).flux)]
    ENDIF
    
    IF (keyword_set(xrange)) THEN !x.range = xrange
    IF (keyword_set(yrange)) THEN !y.range = yrange
    
    FOR j = 0, n_elements(s)-1 DO BEGIN
      !p.multi=[0,4,3]
      IF NOT keyword_set(updown) THEN BEGIN
        sh_plotdet_func,sh_select(a,(line EQ l(i)) AND (sdir EQ s(j))), $
          last=(j EQ (n_elements(s)-1)),_extra=_extra
      ENDIF ELSE BEGIN
        sh_plotdet_func,sh_select(a,(line EQ l(i))),last=1,_extra=_extra,/scan
      ENDELSE
    ENDFOR
    
    IF  (prompt) AND (i LT (n_elements(l)-1)) THEN BEGIN
      print,'Type enter for next line (q to quit)'
      ans =''
      read,ans
      IF (StrLowCase(ans) EQ 'q') THEN GOTO,eind
    ENDIF
    
  ENDFOR
  
  eind:
  !x = xold
  !y = yold
  !p = pold
  wset,w
  
END
