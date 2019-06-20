FUNCTION get_aotband,a,bnd,band=band,test=test
  b   = ['1a','1b','1d','1e','2a','2b','2c','3a','3c','3d','3e','4a','4c','4d', '4']
  l   = [   1,   2,   3,   4,   5,   6,   7,   9,  10,  11,  12,  20,  21,  22,  13]   
  dlo = [   1,   1,   1,   1,  13,  13,  13,  25,  25,  25,  25,  37,  37,  37,  37]*1L
  dhi = [  12,  12,  12,  12,  24,  24,  24,  36,  36,  36,  36,  48,  48,  48,  48]*1L
  flg = [ 128,  96,  96,  64,  64,  32,  32,  64,  64,  32,  32,  64,  64,  32,  32]*1L
  sts = [   1,   1,   2,   2,   2,   2,   3,   1,   2,   2,   3,   1,   2,   2,   3]*1L
  
  CASE n_params() OF
    0: BEGIN
      print,"SH_BAND: Usage t = get_aotband(aar,'2a'[,band='2a'])"
      return,0
    END
    1: BEGIN
      IF n_elements(band) NE 0 THEN BEGIN
        print,"SH_BAND: Usage t = get_aotband(aar,'2a'/,band='2a'[,test=test])"
        return,0
      ENDIF ELSE BEGIN
        bnd=band
      ENDELSE
    END
    ELSE: BEGIN
    END
  ENDCASE
  
  band=bnd
  IF is_int(band) THEN BEGIN
    idx = (where(l EQ band(0)))(0)
    IF idx NE -1 THEN band = b[idx] ELSE band=string(bnd)
  ENDIF
  
  idx = (where(band EQ b))(0)
  IF idx EQ -1 THEN BEGIN
    print,'SH_BAND: invalid band name: '+band
    return,0
  ENDIF
  
  d = a.data.det*1L
  f = a.data.flag*1L
  s = a.data.status*1L
  
  idx = (d GE dlo[idx]) AND (d LE dhi[idx]) AND ((f AND 224L) EQ flg[idx]) AND ((s AND 3L) EQ sts[idx])
  IF keyword_set(test) THEN BEGIN
    return,total(idx) NE 0
  ENDIF ELSE BEGIN
    return,sh_select(a,idx)
  ENDELSE
END
