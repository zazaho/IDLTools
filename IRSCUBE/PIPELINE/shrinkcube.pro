function shrinkcube,file,writefile=writefile,clean=clean

  cubestruct= readfitscube(file)

  c = cubestruct.cube
  h = cubestruct.header
  m = cubestruct.mask
  w = cubestruct.wave
  hw = cubestruct.hwave

  nx=n_elements(c[*,0,0])
  ny=n_elements(c[0,*,0])
  nz=n_elements(c[0,0,*])

  ordermask = m eq !fulldatamaskvalue
  idx = where(ordermask,count,complement=not_idx,ncomplement=not_count)

  if count eq 0 then begin
     message,/info,'no valid (mask='+!fulldatamaskvalue+') pixels found'
     return,-1
  endif

  if keyword_set(clean) and (not_count ne 0) then begin
     ;; do this with an index of the points to be masked since 0*NaN = NaN
     ordermaskcube = rebin(ordermask,nx,ny,nz)
     c[where(ordermaskcube eq 0)] = 0d0
     m[where(ordermask eq 0)] = 0d0
  endif

  x = idx mod nx
  y = idx  /  nx

  xmin = min(x,max=xmax)
  ymin = min(y,max=ymax)
  
  i = reform(c[*,*,0])
  
  sxdelpar,h,'naxis3'
  sxaddpar,h,'naxis',2
  
  hextract,i,h,xmin,xmax,ymin,ymax
  
  c = c[xmin:xmax,ymin:ymax,*]
  
  sxaddpar,h,'naxis',3
  sxaddpar,h,'naxis3',n_elements(c[0,0,*]),after='naxis2'
  
  m = m[xmin:xmax,ymin:ymax]

  cubestruct={cube:c,header:h,wave:w,hwave:hw,mask:m}

  if keyword_set(writefile) then begin
     sf = (stregex(file,'(.*).fits',/subex,/extra))[1]
     writefitscube,cubestruct,sf+'_shrunk.fits'
  endif

  return,cubestruct
end
