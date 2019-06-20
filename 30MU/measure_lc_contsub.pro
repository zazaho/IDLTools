function measure_lc_contsub,in
  
      f = in

  ;; Now derive the central wavelength which divides the feature
  ;; in 2 equal halves.
  wave = f.data.wave
  flux = f.data.flux*3d14/wave^2d0/1d26
  np = n_elements(wave)
  
  ;; the flux until point n
  intval = make_array(np,value=0d0)
  intval[1:np-2]=0.5*(wave[2:np-1]-wave[0:np-3])
  intval[0]=0.25*(wave[1]-wave[0])
  intval[np-1]=0.25*(wave[np-1]-wave[np-2])
  fluxbin = flux*intval
  cumm_fluxbin = 0d0*fluxbin+fluxbin[0]
  FOR i=1,np-1 DO cumm_fluxbin[i]=cumm_fluxbin[i-1]+fluxbin[i]
  tot_fluxbin = cumm_fluxbin[np-1]
  foo = min(abs(cumm_fluxbin - tot_fluxbin/2d0),index)
  central_wave = wave[index]

  return,central_wave
end
