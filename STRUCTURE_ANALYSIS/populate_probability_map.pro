;; function to put stars in a field according to a probabilty map
;;
;; usage: starmap = populate_probabilty_map(pmap,nstars=3000)
;;
;;
;; the procedure is to assign to each pixel an interval between 0..1
;; with a size according to the probability. Then we distribute nstars
;; uniformly from 0..1 and assign stars to the pixels in which the
;; stars happens to fall.
;;
;; The limits are calculated using total(pmap,/cumulative) after
;; normalising the probabilty map to get the upper bound of each
;; interval corresponding to each pixel
;; Next we interpolate the random star probabilties on the limits,pixel-index
;; curve to get the pixel indices of the random stars.
;; Those selected will have a star added to them.

function populate_probability_map,pmap_in,nstars=nstars,catalog=catalog

  default,nstars,5150L
  
  ;; make sure not to overwrite the input map
  pmap=pmap_in
  
  ;; remove nans
  idx_nan = where(finite(pmap) ne 1,cnt)
  if cnt ne 0 then pmap[idx_nan]=0

  ;; normalise to unity
  pmap = pmap/total(pmap)

  ;; dimension
  nx = n_elements(pmap[*,0])
  ny = n_elements(pmap[0,*])
  pixel_index =lindgen(nx*ny)
  
  ;; the star map (empty to start with)
  smap = make_array(nx,ny,value=0)

  ;; calculate the probability upper limit for each pixel
  prob_interval_limits = total(pmap,/cumulative)

  ;; distribute the stars uniformly over the 0..1 interval
  prob_stars = randomu(seed,nstars)
  
  ;; find the star pixel indices corresponding to their random value
  index_stars = ceil(interpol(pixel_index,prob_interval_limits,prob_stars))

  x = index_stars mod nx
  y = index_stars / nx

  catalog=transpose([[x],[y]])

  ;; add stars to the selected pixels
  smap[index_stars]++
  
  return,smap

end

;; test to populate a gaussian field with stars
img = lindgen(1001L,1001L)

xx = double((img mod 1001L) - 500L)
yy = double((img  /  1001L) - 500L)

rr = sqrt(xx^2 +yy^2)

img = exp( -1d0 * (rr / 100d0)^2)

star_image = populate_probability_map(img)

end
