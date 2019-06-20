FUNCTION sh_combine,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15, $
                    a16,quiet=quiet
  
  np = n_params()
  
; Fisrt check for valid data
  valid = make_array(np,value=0)
  FOR i = 0,np-1 DO BEGIN
    f = execute('exist=(n_tags(a'+n2s(i)+') gt 0)')
    IF (exist) THEN valid[i] = 1 ELSE $
      if not keyword_set(quiet) then $
      print,'parameter '+n2s(i)+' does not contain valid data'
  ENDFOR
  
  ivalid = where(valid)
  nvalid = total(valid)
  ; get the first valid one in variable a
  f = execute('a = a'+n2s(ivalid[0]))
  
  CASE nvalid OF
    0: BEGIN
      print,'SH_COMBINE: usage: all = sh_combine(aar,aar2[,aar3,..,aar16])'
      return,0
    END
    ; Return just the only valid one
    1: BEGIN
      return,a
    END
    ELSE: BEGIN
      f = execute("txt = '[a.data'")
      FOR i = 1,nvalid-1 DO BEGIN
        txt = txt+',a'+n2s(ivalid[i])+'.data'
      ENDFOR
      txt = txt+']'
      f = execute("out= {type:'SAAR',history:a.history,header:a.header,data:"+txt+"}")
      idx = sort(out.data.wave)
      FOR i = 0,n_tags(out.data)-1 DO BEGIN
        out.data.(i) = out.data[idx].(i)
      ENDFOR
      return,out
    END
  ENDCASE
END
