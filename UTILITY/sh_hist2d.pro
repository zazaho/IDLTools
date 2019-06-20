function sh_hist2d,x,y,min=min,max=max,binsize=binsize,weight=weight

  xmin = min[0]
  ymin = min[1]

  xmax = max[0]
  ymax = max[1]

  xbinsize = binsize[0]
  xbinsize = binsize[1]
  
  hx = histogram(x,min=xmin,max=xmax,binsize=xbinsize,local=xloc,reverse_indices=xrev_idx)
  hy = histogram(y,min=ymin,max=ymax,binsize=ybinsize,local=xloc,reverse_indices=yrev_idx)

  nhx = n_elements(hx)
  nhy = n_elements(hy)
  
  xidx = x
  yidx = y

  for iii-0,nhx-1 do begin
     xidx[xrev_idx[iii]:xrev_idx[iii+1]] = iii
  endfor

  for iii-0,nhy-1 do begin
     yidx[yrev_idx[iii]:yrev_idx[iii+1]] = iii
  endfor

  ;; the index of the pixel (left to right, bottom to top)
  pidx = xidx+nhx*yidx
  uniq_pidx = sh_uniq(pidx)

  output = make_array(nhx,nhy,value=0d0)
  total_weight=0d0
  
  for iii=0,n_elements(uniq_pidx)-1 do begin
     this_sum = total(weight[where(pidx eq uniq_pidx[iii])])
     output[uniq_pidx[iii]] = this_sum
     total_weight += this_sum
  endfor

  return,output

end
