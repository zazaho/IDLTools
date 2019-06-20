FUNCTION sh_scale,image

  simage= image[sort(image)]
  median = median(simage)
  ximage = dindgen(n_elements(simage))

  
  ;; now give weights which are inversely propotional to the
  ;; difference between the datavalue and the median
  
  errs = sqrt(abs(simage-median))

  foo = poly_fit(ximage,simage,1,measure_errors=errs,/double,yband=stdev,yfit=yfit)
  ;; cut of the lowest and hightest parts
  zmin = min(yfit,max=zmax)
  ikeep = where( (simage GT zmin) AND (simage LT zmax))

  fit = poly_fit(ximage[ikeep],simage[ikeep],1,measure_errors=errs[ikeep],/double,yfit=yfit)
      
  zmin = fit[0]
  zmax = fit[0]+n_elements(simage)*fit[1]
  tvscale,zmin>(image<zmax)
  return,[zmin,zmax]
END 

