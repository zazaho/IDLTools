function median_in_circle,i,h,ra=ra,dec=dec,size=size,deviation=deviation
  
  ;; determine the coordinates of the center of each pixel
  nx = long(n_elements(i[*,0]))
  ny = long(n_elements(i[0,*]))

  x  = lindgen(nx,ny) mod nx
  y  = lindgen(nx,ny)  /  nx
  
  ;; this return the CENTER of each pixel with index x,y in the image
  ;; verified with ds9
  xyad,h,x,y,a,d

  ;; determine the distance to the ra,dec asked for
  dist = sqrt(((a-ra)/cos(dec/180d0*!dpi))^2 + (d-dec)^2)*3600d0

  ;; determine which pixels are in the circle
  idx_inside = where(dist le size,cnt)
  
  ;; determine the median value of those pixels
  if cnt ne 0 then begin
     inside = i[idx_inside]
     idx_not_nan = where(finite(inside) ne 0,cnt2)
     if cnt2 ge 3 then begin
        deviation=median_absolute_deviation(inside[idx_not_nan])
        mdn=median(inside[idx_not_nan])
     endif else begin
        message,/info,'not enough non-nan pixels to compute median'
        return,0d0
     endelse
  endif else begin
        message,/info,'not enough pixels in circle to compute median'
     return,0d0
  endelse

  return,mdn

end
