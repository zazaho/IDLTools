FUNCTION sh_whichband,a,num=num,linedata=linedata
  b   = ['unknown','1a','1b','1d','1e','2a','2b','2c','3a','3c','3d','3e','4a','4c','4d', '4']
  lin = [        0,   1,   2,   3,   4,   5,   6,   7,   9,  10,  11,  12,  20,  21,  22,  13]   
  dlo = [        0,   1,   1,   1,   1,  13,  13,  13,  25,  25,  25,  25,  37,  37,  37,  37]*1L
  dhi = [     2000,  12,  12,  12,  12,  24,  24,  24,  36,  36,  36,  36,  48,  48,  48,  48]*1L
  flg = [        0, 128,  96,  96,  64,  64,  32,  32,  64,  64,  32,  32,  64,  64,  32,  32]*1L
  sts = [        0,   1,   1,   2,   2,   2,   2,   3,   1,   2,   2,   3,   1,   2,   2,   3]*1L
  
  IF n_params() EQ 0 THEN BEGIN
      print,'SH_WHICHBAND: Usage t = whichband(aar)'
      return,0
    END
  
  alin = a.data.line
  adet = a.data.det
  aflg = a.data.flag
  asts = a.data.status
  
  ;; For storing the standard line numbers
  linedata = alin*0

  lu = sh_uniq(alin)
  nlu = n_elements(lu)
  out = make_array(nlu,value='')
  num = make_array(nlu,value=0)
  FOR i=0,nlu-1 DO BEGIN
    idx =(where(alin EQ lu[i]))[0]
    idx =(where((dlo LE adet[idx]) AND (dhi GE adet[idx]) AND $
                ((aflg[idx] AND 224L) EQ flg) AND ((asts[idx] AND 3L) EQ sts)))[0]
    IF idx EQ -1 THEN idx = 0
    out[i] = b[idx]
    num[i] = lin[idx]
    linedata[where(alin EQ lu[i])] = lin[idx] 
  ENDFOR
  return,out
END
