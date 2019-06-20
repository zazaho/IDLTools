;; simple routine to remove the effects of a residual ltt or a
;; broadscale bbackground by removing the median of all good data as a
;; function of time
PRO corr_ltt_median,raster,quiet=quiet

  ;; first split in 3 groups dependent on the sensitivity
  ;; This allows for a better ltt determinations for the low flux
  ;; fixels that respond much more slowly

  xx = dindgen(2000)*0.001
  yy = histogram(raster.flat,min=0,max=2,binsize=0.001)
  cyy = yy
  FOR i=1,1999 DO cyy[i]=yy[i]+cyy[i-1]
  cut1 = max(xx[where(cyy LE 250)])
  cut2 = min(xx[where(cyy GE 774)])

  low = where(raster.flat LE cut1)
  mdl = where((raster.flat GT cut1) AND (raster.flat LT cut2))
  hgh = where(raster.flat GE cut2)
  
  vals = ['low','mdl','hgh']

  npoints = n_elements(raster.cube[0,0,*])

  flat = rebin(raster.flat,32,32,npoints,/sample)

  flattened_cube = raster.cube/flat

  FOR kk=0,2 DO BEGIN
      median = make_array(npoints,value=0d0)
      has_data = median

      foo = execute('val_idx='+vals[kk])
      
      x_idx = val_idx MOD 32
      y_idx = val_idx  /  32
      
      FOR i = 0,npoints-1 DO BEGIN
          idx = where(((raster.mask[*,*,i])[x_idx,y_idx] AND 1) EQ 0,count)
          IF count NE 0 THEN begin
              median[i] = median(((flattened_cube[*,*,i])[x_idx,y_idx])[idx])
              has_data[i] = 1
          ENDIF 
      ENDFOR 

      ;; interpolate for the non data points
      have = where(has_data EQ 1)
      miss = where(has_data EQ 0)
      median[miss] = interpol(median[have],have,miss,/qua)
      
      ;; fix the extrapolations
      before = where(miss LT have[0],count)
      IF count NE 0 THEN BEGIN
          median[before] = median[have[0]]
      ENDIF
  
      after = where(miss GT have[n_elements(have)-1],count)
      IF count NE 0 THEN BEGIN
          median[after] = median[ have[n_elements(have)-1]]
      ENDIF
  
      ;; median the median with a window of 101
      width=51
      long_median = [make_array(width,value=median[0]),median,make_array(width,value=median[n_elements(median)-1])]
      smooth_long_median = median(long_median,width)
      smooth_median = smooth_long_median[width:width+n_elements(median)-1]
      
      smooth_median = rebin(reform(smooth_median,1,1,npoints),32,32,npoints,/sample)
      nval = n_elements(val_idx)
      all_idx = collapse(rebin(val_idx,nval,npoints,/sample)+rebin(reform(32*32*lindgen(npoints),1,npoints),nval,npoints))
      raster.cube[all_idx] = (raster.cube-flat*smooth_median)[all_idx]
;;      IF kk EQ 0 THEN pl,smooth_median[0,0,*],ymin=0 ELSE pl,smooth_median[0,0,*],/opl
  ENDFOR 
END
