function pah_spatialaverage,map

  ;; we want to calculate the distance average intensity for each
  ;; pixel in the map

  nx =  n_elements(map[*,0])
  ny =  n_elements(map[0,*])

  foo = lindgen(nx,ny)

  x = foo mod nx
  y = foo  /  nx

  valid = where(finite(map) eq 1)

  result = make_array(nx,ny,value=!values.d_nan)
  
  for i = 0,n_elements(valid)-1 do begin

     this_x = x[valid[i]]
     this_y = y[valid[i]]

     ;; set the minumum distance to 0.5 pixel (the pixel itself
     this_distance = sqrt( (x[valid]-this_x)^2 + (y[valid]-this_y)^2) > 0.5
     
     result[valid[i]] = total(map[valid]*this_distance^(-2))/total(this_distance^(-2))
  endfor

  return,result
end
