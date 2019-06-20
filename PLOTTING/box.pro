pro box,p1,p2,p3,p4,p5,label=label,_extra=_extra
  
  npar = n_params()
  CASE npar OF
    0: BEGIN
    GOTO,usage  
    END
    1: BEGIN
      p = p1
    END
    5: BEGIN
      label = p5
      p =[p1,p2,p3,p4]
    END
    ELSE: BEGIN
      ;;;get the last param
      f = execute('plast = p'+n2s(npar))
      ;;;test to see if it is a string
      s_plast = size(plast)
      IF s_plast(s_plast(0)+1) EQ 7 THEN BEGIN
        label = plast
        npar = npar -1
      ENDIF
      p = p1
      FOR i=2,npar DO foo = execute('p = [p,p'+n2s(i))
    END
  ENDCASE
  
  IF n_elements(p) NE 4 THEN BEGIN
usage:
    print,'BOX: Usage:'
    print,'box,x1,y1,x2,y2[,label,label=label or'
    print,'box,p1,p2[,label,label=label (p1=[x1,y1]) or'
    print,'box,c[,label,label=label (w=[x1,y1,x2,y2]) '
    return
  ENDIF
    
  oplot,[p[0],p[0]],[p[1],p[3]],_extra=_extra
  oplot,[p[0],p[2]],[p[3],p[3]],_extra=_extra
  oplot,[p[2],p[2]],[p[3],p[1]],_extra=_extra
  oplot,[p[2],p[0]],[p[1],p[1]],_extra=_extra
  IF (!p.charsize EQ 0) THEN charsize = 1 ELSE charsize = !p.charsize 
  if n_elements(label) NE 0 THEN BEGIN 
    ;; We need to find the Y_offset to put the label center in the
    ;; y-direction so:
    normchar   = (convert_coord(1,!d.y_ch_size*charsize,/DEVICE,/TO_NORMAL))[1]
    normcenter = convert_coord((p[0]+p[2])/2d0,(p[1]+p[3])/2d0,/DATA  ,/TO_NORMAL)
    xyouts,normcenter[0],normcenter[1]-normchar/2.5,strtrim(string(label),2),align=0.5,_extra=_extra,/NORMAL
  endif
end

