function sh_res,lambda,bnd,BAND=band,ngc7027=ngc7027,recalc=recalc,_extra=_extra
  
  ngc = replicate({wave:0d0,rp:0d0,fullrp:0.0,band:''},12)
  ngc.wave = [2.495215,2.758352,3.207089,3.740726,4.654228,6.706848,10.511208,14.323432,18.715612,25.894011,27.000000,36.016670]
  ngc.rp=[1878.944702,1246.011108,1583.251587,1169.564209,1436.841309,1080.392700,1670.837280,1315.117676,1689.032104,1078.194458,735.500,1453.015015]
; Old values  
;  ngc.fullrp=[2858.99,2252.79,2719.92,1992.47,2239.08,1499.20,2533.64,1873.55,2538.04,1421.81,1469.92,1620.62]
  ngc.fullrp=[3616.25,2627.00,3144.20,2078.18,2543.29,1581.80,2819.54,2003.28,2768.58,1514.27,1508.38,1915.78]
  ngc.band=['1a','1b','1d','1e','2a','2b','2c','3a','3c','3d','3e','4']
 
  If (n_params() eq 2) then begin
    band = bnd
  endif
  
  nlambda = n_elements(lambda)
  IF nlambda EQ 0 THEN BEGIN
    print,'SH_RES: no wavelengths given'
    return,0
  ENDIF
  
  if not keyword_set(band) then begin
    band=make_array(nlambda,value='  ')
    for i=0,nlambda-1 do begin
      band[i] = lambda_to_aot(lambda[i])
    endfor
  endif
  
  nband = n_elements(band)
  
  if nlambda gt nband then begin
    band = [band,make_array(nlambda-nband,value=band[nband-1])]
  endif
  
  band = strtrim(strlowcase(band),2)
  
  foo = sh_comparray(band,ngc.band,where=where_band)
  IF total(foo) NE nlambda THEN BEGIN
    print,'SH_RES:invalid band(s) specified'
    print,band[where(foo EQ 0)]
    IF where_band(0) EQ -1 THEN return,0 
    band   = band[where_band]
    lambda = lambda[where_band]
    nlambda = n_elements(lambda)
  ENDIF
  
  res = lambda*0
 
  for i = 0,nlambda-1 do begin
    res[i] = resolution(lambda[i],band[i],_extra=_extra)
  endfor
      
; if we want to use the measured resolutions of ngc7027 aot1 speed4
; then we scale everything to these measurements
;; The ngc struct hold the measured ngc7027 measurd lines from rev024  
  if keyword_set(ngc7027) then begin
      if keyword_set(recalc) then begin
        ngc.fullrp = sh_res(ngc.wave,ngc.band)
        print,ngc.fullrp
      ENDIF
      ;; Get the positions of the bands in the ngc struct
      foo = sh_comparray(ngc.band,band,where=where_band)
      res = ngc[where_band].rp/ngc[where_band].fullrp*res
    endif
  return,res
end
