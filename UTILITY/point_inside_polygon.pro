FUNCTION point_inside_polygon, x, y, px,py

  nx = n_elements(x)
  ny = n_elements(y)

  IF nx NE ny THEN $
    message,'the number of elements in x and y should be equal', $
            /error
  
  npx = n_elements(px)
  npy = n_elements(py)

  IF (npx NE npy) OR (npx LT 3) THEN $
    message,'the number of elements in x and y for the boundary should be equal and larger than 2', $
    /error

  ;; Make some reforming just to make sure we have simple arrays
  tx = reform(x,nx)
  ty = reform(y,nx)
  tpx = reform(px,npx)
  tpy = reform(py,npx)
  
  ;; stretch the points to the dimension of the polygon
  sx = tx # make_array(npx,val=1)
  sy = ty # make_array(npx,val=1)

  ;; stretch the polygon points to the dimension of x
  spx = tpx ## make_array(nx,val=1)
  spy = tpy ## make_array(nx,val=1)

  
  ;; now calculate the differences in X and Y for each line segment
  dx1 = spx - sx
  dy1 = spy - sy

  dx2 = shift(spx,0,-1) - sx
  dy2 = shift(spy,0,-1) - sy

  ;; The dot and cross product
  dotp   = dx1*dx2 + dy1*dy2
  crossp = dx1*dy2 - dy1*dx2
  ;; Now the angles are:
  theta = atan(crossp,dotp)
  
  ;; if the sum of the angles over the segments is 0 then it is
  ;; outside else it is inside. Inside it would be 2PI. Which gives
  ;; that 1e-2 is small enough
  idx_inside = where(abs(total(theta,2)) GT 1e-2)

  return, idx_inside
END
