function sh_snr,aar,XRANGE=xrange,NDEGREE=ndegree,_EXTRA=_extra
  
  default,xrange,[0,0]
  default,ndegree,1
  
  x2range = fltarr(2)

jump2: 
  pl,aar,ps=0,xr=xrange,_extra=_extra
  
jump1:
  print, 'Now select the part you want to use to determine SNR'
  print, 'High to low to zoom'
  wait,0.5
  cursor,x,y
  print,x
  x2range[0] = x
  wait,0.5
  cursor,x,y
  print,x
  x2range[1] = x
  if x2range[0] ge x2range[1] then begin
    print,'you chose first the longs- and next the short wavelength limit;'
    print,'Zooming: '
    pl,aar,xr=[x2range[1],x2range[0]]
    goto, jump1
  endif

  aarline=select(aar,aar.data.wave ge x2range[0] and aar.data.wave le x2range[1])
  
  wv  = aarline.data.wave
  flx = aarline.data.flux
  
  param = poly_fit(wv,flx,ndegree,fitflx,YBand,Sigma)
  
  ft = aarline
  ft.data.flux = fitflx
  pl,aarline
  pl,ft,/o
  pl,offset(ft,sigma,/quiet),line=1,col=100,/o
  pl,offset(ft,-1*sigma,/quiet),line=1,col=100,/o
  
  mn = mean(fitflx)
  print,'SNR: ',mn/sigma
  print,'Mean Continuum: ',mn
  print,'Sigma: ',sigma
  
; now calculate the properties  
  return,mn/sigma

end
