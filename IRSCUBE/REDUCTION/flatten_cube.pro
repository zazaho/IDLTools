;; routine to remove some residual banding in the reduced cubism
;; cubes.

;; After all the bad data removal some residual banding remains along
;; the y axis (ie in the scanning direction) This is due to some
;; slight offsets that remain. We cannot flag those since that would
;; remove a lot of otherwise good data.
;;
;; This fudge removes the stripes by calculating the median of each
;; column and calculating the difference between the median and a
;; smooth function (polynomian fit) to the median values  along the
;; X-axis

;; These offsets are removed from the entire column. The results is an
;; image which has its median value along the y-axis as smooth as the
;; polynomial

pro flatten_cube,cubein,cubeout, $
                 width=width, $
                 degree=degree

  default,degree,3
  default,width,5

  if n_params() eq 1 then begin
     dirin = file_dirname(cubein)
     basenamein = file_basename(cubein)
     cubeout=dirin+'/flattened_'+basenamein
  endif
  
  ;; read the data
  i=readfitscube(cubein)
  nx = n_elements(i.cube[*,0,0])
  ny = n_elements(i.cube[0,*,0])
  nz = n_elements(i.cube[0,0,*])

  lower_quartile_along_y = reform((i.cube[sort_nd(i.cube,2)])[*,ny/4.,*])

  smoothed_lower_quartile_along_y = smooth(lower_quartile_along_y,[width,1],/edge_truncate,/nan)

  offsets_along_y = lower_quartile_along_y - smoothed_lower_quartile_along_y

;;; fit a poly to x,lower_quartile_y for each wavelength
;  polyfit_along_y = lower_quartile_along_y*0d0
;  xx = dindgen(nx)
;  for zz=0,nz-1 do begin
;     mm = lower_quartile_along_y[*,zz]
;     ii = where(finite(mm))
;     pars = poly_fit(xx[ii],mm[ii],degree,yfit=fit)
;     polyfit_along_y[ii,zz] = fit
;  end
;  offsets_along_y = lower_quartile_along_y - polyfit_along_y


  ;; cast the offsets back to the cube shape
  offsets_along_y = transpose(rebin(offsets_along_y,nx,nz,ny),[0,2,1])
  
  ;; replace the values with corrected values
  o = i
  o.cube=i.cube-offsets_along_y

  writefitscube,o,cubeout
  
end
