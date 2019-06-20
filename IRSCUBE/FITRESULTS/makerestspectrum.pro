pro makerestspectrum,highsnrdir,regriddir,status=status,restspec=restspec,restunc=restunc
  ;; find the lowest res step because that has good pixels already
  ;; taken out

  status = 0
  allfitsfiles = file_search(highsnrdir+'*.fits',count=count)
  if count eq 0 then begin
     status=1
     return
  endif
  highsnrfile = allfitsfiles[n_elements(allfitsfiles)-1]

  suffix = (stregex(highsnrfile,'.*(_[^.]*)\.fits',/extrac,/subex))[1]
  regriddedfiles = file_search(regriddir+'/*'+suffix+'.fits',count=count)
  if count ne 2 then begin
     status=1
     return
  endif
  
  highsnr = readfitscube(highsnrfile)
  regriddata = readfitscube(regriddedfiles[0])
  regridunc  = readfitscube(regriddedfiles[1])

  ;; ok by no we should have the data read

  ;; the last pixels to be masked and the non complete data
  masked_idx = where((highsnr.mask eq !fulldatamaskvalue) or (regriddata.mask ne !fulldatamaskvalue),cnt)
  
  if cnt eq 0 then begin
     status=1
     return
  endif
  
  data = regriddata.cube
  unc = regridunc.cube
  nx = n_elements(data[*,0,0])
  nz = n_elements(data[0,0,*])
  
  for i=0,n_elements(masked_idx)-1 do begin
     data[masked_idx[i] mod nx, masked_idx[i] / nx,*] = 0d0
     unc [masked_idx[i] mod nx, masked_idx[i] / nx,*] = 0d0
  endfor
  
  ;; fudge !!
  data = finitise(data)
  unc = finitise(unc)
  
  totaldata = total(total(data,1),1)
  totalunc = total(total(unc,1),1)
 
  header = regriddata.header
  sxaddpar,header,'NAXIS1',1
  sxaddpar,header,'NAXIS2',1

  restspec = {cube:reform(totaldata,1,1,nz),header:header,wave:regriddata.wave,hwave:regriddata.hwave,mask:reform(!fulldatamaskvalue,1,1)}
  restunc =  {cube:reform(totalunc,1,1,nz) ,header:header,wave:regridunc.wave ,hwave:regridunc.hwave ,mask:reform(!fulldatamaskvalue,1,1)}
  
end

  
