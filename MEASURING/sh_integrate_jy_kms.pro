FUNCTION sh_integrate_jy_kms,fnu,vel,lambda0=lambda0

  c = 2.99792458d8                        ;; Speed of light [SI]
  if not keyword_set(lambda0) then lambda0=157.74 * 1d-6 ;; [SI]
  
  fnufnu = double(fnu) * 1d-26 ;; [W/m^2/Hz]
  velvel = double(vel) * 1d3   ;; m/s

  lambda0lambda0 = double(lambda0) ;[m]
  nu0nu0=c/lambda0lambda0 ;[Hz]
  
  ;; vel = -1*c*(nu-nu0)/nu0
  ;; nu =  nu0-1*vel/c*nu0

  nunu = nu0nu0-1d0*velvel/c*nu0nu0 ;; [Hz]
  
  idx = sort(nunu)
  nunu=nunu[idx]
  fnufnu=fnufnu[idx]
  
  npoints = n_elements(nunu)
  
  if (npoints lt 3) then begin
     message,/info,'not enough points to do an integration'
     return,!values.d_nan
  endif

  ;; size of each bin
  binsize = (shift(nunu,-1)-nunu)[0:npoints-2]
  
  ;; average height in each bin
  halfheight = ((shift(fnufnu,-1)+fnufnu)[0:npoints-2])/2d0

  return, total(binsize*halfheight) ;; W/m2
END
