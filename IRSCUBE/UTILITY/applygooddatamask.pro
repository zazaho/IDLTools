;; put all not masked points to zero
;; stupidly good points are at 1 and the not good points at 0
function applygooddatamask,in,mask
  data=in

  idx = where(mask eq 0,count)
  if count ne 0 then begin
     ;; do this with an index of the points to be masked since 0*NaN = NaN
     maskcube = rebin(mask,n_elements(mask[*,0]),n_elements(mask[0,*]),n_elements(data.cube[0,0,*]))
     data.cube[where(maskcube eq 0)] = 0d0
     data.mask[where(mask eq 0)] = 0d0
  endif
  return,data
end
