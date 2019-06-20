function islands,x,y,max_allowed_distance

  positions = transpose([[x],[x]])
  distance_matrix = distance_measure(positions,/matrix)
  
  near_matrix = distance_matrix le 2*max_allowed_distance
  
  groups = LIST()
  
  while min(distance_matrix,/nan) le max_allowed_distance do begin
     ;; find the point which has most friends within a search radius
     ;; of 2*max_allowed_distance
     ;; 2* because the final reference position could be half way
     ;; between the points

     foo = max(total(near_matrix,2),idx_best_connected)
     
     idx_friends = where(near_matrix[idx_best_connected,*] eq 1)

     x_mean = mean(x[idx_friends])
     y_mean = mean(y[idx_friends])

     idx = where(sqrt( (x[idx_friends]-x_mean)^2  + (y[idx_friends]-y_mean)^2))
     idx_grouped = idx_friends[idx]

     groups.ADD,idx_grouped
     
     ;; remove the points that have been assigned to this group
     distance_matrix[idx_grouped,*] = !values.f_nan
     distance_matrix[*,idx_grouped] = !values.f_nan

     near_matrix[idx_grouped,*] = 0
     near_matrix[*,idx_grouped] = 0
     
  endwhile
  
  pl,x,y,ps=1
  for i=0,n_elements(groups)-1 do begin
     pl,x,y,idx=groups[i],ps=4,/opl,thick=3
  endfor
  
  return,groups
end

x = randomu(seed,10)*100
y = randomu(seed,10)*100

xx = [x,x,x,x,x]+randomn(seed,50)
yy = [y,y,y,y,y]+randomn(seed,50)

isles = islands(xx,yy,4)

end
