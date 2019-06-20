function sh_scale_image,in_image, $
                        percentile=percentile, $
                        zscale=zscale, $
                        log=log, $
                        power=power, $
                        sqrt=sqrt, $
                        square=square, $
                        equalise=equalise, $
                        positive=positive, $
                        min=min, $
                        max=max, $
                        normalise=normalise
                        
  image=double(in_image)
  
  if n_elements(percentile) ge 1 then begin
     if n_elements(percentile) eq 1 then begin
        lower_fractile = percentile/1d2
        upper_fractile = 1d0-lower_fractile
     endif else begin
        lower_fractile = percentile[0]/1d2
        upper_fractile = percentile[1]/1d2
     endelse
     nimage=n_elements(image)
     simage=image(sort(image))
     image=(image>simage[round(nimage*lower_fractile)])<simage[round(nimage*upper_fractile)]
  endif
  
  if n_elements(min) eq 1 then begin
     image=image>min
  endif
  
  if n_elements(max) eq 1 then begin
     image=image<min
  endif
  
  if keyword_set(positive) then begin
     image=image>0d0
  endif

  maxi=max(image,min=mini)
  norm_image = (image-mini)/(maxi-mini)
  
  if n_elements(log) eq 1 then begin
     if (log-1d0) lt 1d-10 then a=1d3 else a=log
     image = alog(a*norm_image+1d0)/alog(a)
  endif

  if n_elements(power) eq 1 then begin
     if (power-1d0) lt 1d-10 then a=1d3 else a=power
     image = (a^norm_image-1d0)/a
  endif

  if keyword_set(sqrt) eq 1 then begin
     image = sqrt(norm_image)
  endif
  
  if keyword_set(square) eq 1 then begin
     image = norm_image^2
  endif
  
  if keyword_set(equalise) eq 1 then begin
     image = hist_equal(norm_image)
  endif

  if n_elements(zscale) eq 1 then begin
     if (zscale-1d0) lt 1d-10 then contrast=zscale else contrast=0.25
     print,'not implemented'
  endif
  
  if keyword_set(normalise) eq 1 then begin
     maxi=max(image,min=mini)
     image = (image-mini)/(maxi-mini)
  endif

  return,image

end
