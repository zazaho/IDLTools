pro find_bad_bcds,files,path=path
  
  if n_elements(path) eq 1 then begin
     pushd,path
  endif
  
  default,files,'*_bcd.fits'
  f=file_search(files,count=cnt)
  
  if cnt eq 0 then goto,returntomain
  
  nan_frac = make_array(cnt,value=0d0)

  for idx=0,cnt-1 do begin
     img=readfits(f[idx])
     ntot=n_elements(img)
     nan_frac[idx]=total(finite(img) eq 0)/ntot
  endfor
  
  mdn = median(nan_frac)
  mdn_dev = median(abs(nan_frac-mdn))
  susp = where((nan_frac-mdn) gt 20*mdn_dev,cnt)

  if cnt eq 0 then goto,returntomain

  ans=''
  for idx=0,cnt-1 do begin
     filename=f[susp[idx]]
     img=readfits(filename)
     window,xpos=0,ypos=0,xsize=n_elements(img[0,*])*3,ysize=n_elements(img[*,0])*3
     tvimage,img
     print,'Would you like to reject this frame? (Y/n)'
     read,ans
     if ans eq 'y' or ans eq 'Y' then begin
        root=strmid(filename,0,strlen(filename)-9)
        print,'Gzipping '+root+'*'
        spawn,'gzip -9 '+root+'*'
     endif
  endfor

returntomain:
  if n_elements(path) eq 1 then begin
     popd
  endif

  return

end
