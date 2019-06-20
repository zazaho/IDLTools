function stdev_noise_map,map
  ;; make an histogram of the values
  hmin = min(map,max=hmax)
  hst = histogram(map,min=hmin,max=hmax,location=hloc,nbin=101)
  ;; remove sharp peaks (the peak around 0 from the borders
  mhst = median(hst,5)
  ;; fit a gaussian to this median profile
  fit = gaussfit(hloc,mhst,fitpars,nterm=3)
  ;; return sigma from the fit
  return,fitpars[2]
end
