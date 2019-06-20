;; this function calculates error statistics on observed ratios based
;; on the observed a and b and the stddevs on a and b

function pah_add_ratio_error,struct,a=a,b=b

  ;; extract data from the structure
  foo = execute('a_obs = struct.'+a)
  foo = execute('b_obs = struct.'+b)
  adata = a_obs.data
  astdv = a_obs.stdev
  bdata = b_obs.data
  bstdv = b_obs.stdev
  ndata = n_elements(adata)
  
  ;; we generate many realisations of the ratio
  nmc = 10000L
  ;; keep track of the realistations
  realisations = make_array(ndata,nmc,value=!values.d_nan)

  for mmm=0,nmc-1 do begin
     ;; cap the values to 0 like in the observations
     this_a = adata+astdv*randomn(seed,ndata) > 0d0
     this_b = bdata+bstdv*randomn(seed,ndata) > 0d0
     realisations[*,mmm] = this_a/this_b
  endfor
  
  ;; here we extract the stdev
  moms = moment(realisations,dim=2)

  out = sh_add_tag(struct,"R_"+a+"_"+a+"_stdev_mc",sqrt(reform(moms[*,1])))
  
  return,out
end
