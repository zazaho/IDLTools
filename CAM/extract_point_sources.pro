FUNCTION enlarge_image,img_in,border

  img = img_in
  nx = n_elements(img[*,0])
  ny = n_elements(img[0,*])

  ;; Now extend the image to be able to median filter the whole image
  img_large =  make_array(nx+2*border,ny+2*border,value=0d0)

  ;; now fill the large image
  ;; the center
  img_large[border:nx+border-1,border:ny+border-1] = img

  ;; we want to reverse the edges and then extend the image with the
  ;; reversed pieces so we say
  
  idx = indgen(border)
  rev_idx = border-indgen(border)
  
  ;; The left piece
  img_large[idx,*] = $
    img_large[rev_idx+border,*]

  ;; The right piece
  img_large[nx+border+idx+1,*] = $
    img_large[nx+rev_idx+1,*]

  ;; The botton piece
  img_large[*,idx] = $
    img_large[*,rev_idx+border]

  ;; The top piece
  img_large[*,ny+border+idx+1] = $
    img_large[*,ny+rev_idx+1]
  
  return,img_large
END

FUNCTION make_median, img_in,mask=mask,med_width=med_width

  default,med_width,11
  img = img_in
  nx = n_elements(img[*,0])
  ny = n_elements(img[0,*])
  
  img_large = enlarge_image(img,med_width)

  IF n_elements(mask) NE 0 THEN BEGIN
      mask_large = enlarge_image(mask,med_width)
      mdn = median(img_large[where(mask_large NE 1)])
      ;; now make a composite image 
      img_large = img_large*(mask_large NE 1)+(mask_large EQ 1)*mdn
  ENDIF 
  
  return,(median(img_large,med_width))[med_width:nx+med_width-1,med_width:ny+med_width-1]
END


FUNCTION bkg_noise,img_in
  img = img_in

  mdn = median(img)
  hist = histogram(img,bins=0.001,min=mdn-1d0,max=mdn+1d0)
  x_hist = dindgen(2000)*1d-3-1d1+mdn
  
  foo = gaussfit(x_hist,median(hist,51),fit_params,nterms=4)
  
  return,fit_params[2]
END 
  
FUNCTION extract_point_sources,img_in,filt=filt,mask=mask
  
  default,med_width,11
  default,sigma_tol,1d-8
  default,source_radius,1.42 ;; in pixels

  img = img_in
  nx = n_elements(img[*,0])
  ny = n_elements(img[0,*])
  
  ;; Now we want to make a median filter for several scales
  global_median = make_median(img,med_width=31)
  filt = img - global_median
  medium_median = make_median(filt,med_width=21)
  filt = filt - medium_median
  local_median = make_median(filt,med_width=11)
  filt=filt-local_median

  bkg_sigma = bkg_noise(filt)

  print,'The background noise sigma equals: ',bkg_sigma

  ;; mask the missing data at the edges
  edge_mask = (filt LE -5d0*bkg_sigma)
  filt[where(edge_mask EQ 1)] = 0d0

  ;; the pixels brighter than 3 sigma
  source_idx = where((filt GE 3d0*bkg_sigma) AND (NOT edge_mask))
  
  ;; Now sort them in order of decreasing brightness
  source_val = filt[source_idx]
  sort_idx = reverse(sort(source_val))
  source_idx = source_idx[sort_idx]

  ;; remove the pixels that are too close to a brighter pixels
  uniq_source = 1+0*source_idx
  x_src = source_idx MOD nx
  y_src = source_idx  /  nx
  n_sources = n_elements(source_idx)

  FOR i=0,n_sources-1 DO BEGIN
      IF (uniq_source[i] EQ 1) THEN BEGIN
          distance = sqrt((x_src-x_src[i])^2d0+(y_src-y_src[i])^2d0)
          too_close = where((distance GT 0.5) AND (distance LE source_radius),count)
          IF count NE 0 THEN BEGIN
              uniq_source[too_close] = 0
          ENDIF 
      ENDIF
  ENDFOR 
  source_idx = source_idx[where(uniq_source EQ 1)]

  ;; Now select only those pixels within the radius of a source
  source_mask = lindgen(nx,ny)
  x_source_mask = source_mask MOD nx
  y_source_mask = source_mask  / nx
  source_mask = source_mask*0d0
  
  n_sources = n_elements(source_idx)
  source_list = replicate({source,x:0,y:0,peak:0d0},n_sources)
  
  FOR i=0,n_elements(source_idx)-1 DO BEGIN
      x = source_idx[i] MOD nx
      y = source_idx[i]  /  nx
      source_list[i].x = x
      source_list[i].y = y
      source_list[i].peak = filt[source_idx[i]]
      src = where(((x_source_mask-x)^2d0+(y_source_mask-y)^2d0) LE source_radius^2d0)
      source_mask[src] = 1d0
  ENDFOR 
  
  full_mask = (source_mask OR edge_mask)
  
  ;; Now we want to make a much better median without our sources
  global_median = make_median(img,mask=full_mask,med_width=31)
  filt = img - global_median
  medium_median = make_median(filt,mask=full_mask,med_width=21)
  filt = filt - medium_median
  local_median = make_median(filt,mask=full_mask,med_width=11)
  filt=filt-local_median
  mask=source_mask

  return,source_list
end
