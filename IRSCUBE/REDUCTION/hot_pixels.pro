;; Category CUBISM enhanced

;; function to find isolated rogue pixels in a frame above a certain threshold
;; based on median filtering but we impose that the pixel is diviant
;; both in the horizontal and vertical dimension. So not part of a
;; stripe in the map wich may be a spectral line or a dispersed bright source

function hot_pixels,image,threshold=threshold,width=width
  
  default,threshold,1d2 ;; percentage of the values around
  default,width,3        ;; only find single pixels so keep the filter small
  
  nx = n_elements(image[0,*])
  ny = n_elements(image[*,0])

  image_filtered_col = image*0d0
  image_filtered_row = image*0d0

  ;; first make an image with each column replaced by the median
  ;; filtered column

  image_filtered = median(image,width)
  for i=0,nx-1 do image_filtered_col[i,*] = median(reform(image[i,*]),width)
  for i=0,ny-1 do image_filtered_row[i,*] = median(reform(image[*,i]),width)
  
  idx_spikes = where(abs(image-image_filtered_col) gt image_filtered*threshold/1d2 and abs(image-image_filtered_row) gt image_filtered*threshold/1d2)

  hot_pixel_image = image*0d0
  hot_pixel_image[idx_spikes] = image[idx_spikes] -image_filtered[idx_spikes]

  return,hot_pixel_image

end

