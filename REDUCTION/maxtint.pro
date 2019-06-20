FUNCTION maxtint,a
  l = a.data.line
  l = l(sort(l))
  l = l(uniq(l))
  CASE n_elements(l) OF
    0: print,'The input has no lines !'
    1: BEGIN 
      return,sh_weg(a,tint=max(a.data.tint))
    END
    ELSE: BEGIN
      atmp = sh_select(a,a.data.line EQ l(0))
      a0 = sh_weg(atmp,tint=max(atmp.data.tint))
      FOR i = 1,n_elements(l)-1 DO BEGIN
        atmp = sh_select(a,a.data.line EQ l(i))
        a0 = sh_combine(a0,sh_weg(atmp,tint=max(atmp.data.tint)))
      ENDFOR
      return,a0
    END
  END
END
