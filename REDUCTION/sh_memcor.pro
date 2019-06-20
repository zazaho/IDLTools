; Function to bring to scans of the same band together by subtracting
; half of the difference in low res form each scan
FUNCTION sh_memcor,ain,sigma=sigma,resol=resol,order=order
  
  default,sigma,2.5
  default,resol,50d0
  default,order,0d0
  
  a = ain
  d = getscan(a,/down)
  u = getscan(a,/up)
  
  rd = sws_rebin(sigclip(sws_flatfield(d,order,/noplot),sig=sigma), $
                 res=resol,met='median',/noplot)
  ru = sws_rebin(sigclip(sws_flatfield(u,order,/noplot),sig=sigma), $
                 res=resol,met='median',/noplot)
  
  dif = subtract(rd,ru)
  
  dcorr = subtract(d,fact(dif, 5d-1))
  ucorr = subtract(u,fact(dif,-5d-1))
  
  corr = sh_combine(dcorr,ucorr)
  
  return,corr
END
