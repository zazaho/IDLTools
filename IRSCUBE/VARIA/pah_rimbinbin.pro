;; bin the realistions to a kind of hess diagram

function pah_rimbinbin,x,y,nxbins=nxbins, $
                   ymin=ymin, $
                   ymax=ymax, $
                   nybins=nybins

  default,nxbins,50
  default,nybins,50

  default,ymin,min(y)
  default,ymax,max(y)
  
  nx = n_elements(x)
  ny = n_elements(y)
  
  ;; make bins with roughly equal number of data points
  ;; so the final cube has nxbins,nybins data

  xvalues = make_array(nxbins,value=0d0)
  yvalues = ymin+dindgen(nybins)/(nybins-1d0)*(ymax-ymin)
  hist = make_array(nxbins,nybins,value=0d0)
  
  idx = sort(x)
  xperbin = double(nx)/double(nxbins)
  
  for iii=0,nxbins-1 do begin
     lo = iii*xperbin > 0
     hi = ((iii+1)*xperbin-1) < (nx-1)
     this_idx = idx[lo:hi]
     xvalues[iii] = mean(x[this_idx])
     hist[iii,*] = histogram(y[this_idx,*],min=ymin,max=ymax,nbins=nybins)
  endfor

  return,{x:xvalues,y:yvalues,hist:hist}
end
