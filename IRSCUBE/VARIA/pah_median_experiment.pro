;; This is a function that examines the intrinsic median ratio and intrinsic
;; dispersion on a given ratio (a/b) that is required to understand the
;; observed ratio given the uncertanties on a and b
;;
;; The idea is to isolate a number of pixels that share a common
;; property (slicer) for example the I_pah and determine the histogram
;; of the ratio for these pixels.
;; the question becomes what central value with which dispersion
;; around it is required to reproduce this histogram.
;;
;; The ultimate goal is to have the ability to put a trace on a
;; scatter plot showing the evolution of these values as a function of
;; the slicer variable.

;; Implemenation details. Binning along the slicer could be done in a fixed count
;; binning or along 

;; AAA
;; First determine the median ratio to test (target_median)

;; determine the instrinsic scatter to test (scatter)
;; at these randomly to the pixels target_ratio_perturbed = target_median+scatter*randomn(seed,npixels)

;; replace the actual values by some that are similar but respect the
;; target ratio target_a/target_b = target_ratio_perturbed
;; consider the target_X a correction on the observed value:
;; target_a = (1+x) * a
;; target_b = (1-x) * b
;; i.e. same percentage correction but in the opposite diretion
;; then if C = target_median / observed_median
;; x = (C-1)/(C+1)

;; this yields the assumed "intrinsic" distribution of a and b
;; add noise from the observations.
;; simulated_a = target_a+randomu(seed,npixels)*a_e
;; careful: stdev is what randomu generates?
;; This yields the observable ratios

;; determine the simulated histogram

;; Compare with observed.
;; if median is not good, adjust and goto AAA
;; if scatter is not matched well enough goto BBB

;; fill structure with center of bin in slicer
;; best median guess and scatter

function pah_median_experiment,struct,a=a,b=b, $
                      slicer=slicer, $
                      nslicer=nslicer, $
                      min_slicer=min_slicer, $
                      max_slicer=max_slicer, $
                      fixed_count_slicer=fixed_count_slicer,$
                      log_slicer=log_slicer,$
                      idx=idx


  default,nslicer,10

  ;; some arrays to hold the output
  intrinsic_median  = make_array(nslicer,value=!values.d_nan)
  intrinsic_scatter = make_array(nslicer,value=!values.d_nan)
  
  ;; extract data from the structure
  foo = execute('a_obs = struct.'+a)
  foo = execute('b_obs = struct.'+b)
  foo = execute('s_obs = struct.'+slicer)
  adata = a_obs.data
  astdv = a_obs.stdev
  bdata = b_obs.data
  bstdv = b_obs.stdev
  sdata = s_obs.data

;; select data if requested
  if n_elements(idx) ne 0 then begin
     adata = adata[idx]
     astdv = astdv[idx]
     bdata = bdata[idx]
     bstdv = bstdv[idx]
     sdata = sdata[idx]
  endif

  ;; unNANify (check that this is what we want)
  foo = where(finite(adata) and finite(astdv) and finite(bdata) and finite(bstdv) and finite(sdata),nfoo)
  if nfoo eq 0 then begin
     message,/info,'There are no pixels left after throwing out the NANs'
     return,{}
  endif
  adata = adata[foo]
  astdv = astdv[foo]
  bdata = bdata[foo]
  bstdv = bstdv[foo]
  sdata = sdata[foo]
  
  ;; now make the slicing bins
  ;;determine the min max range
  default,min_slicer,min(sdata)
  default,max_slicer,max(sdata)

  ;;If we want the same number of points in each bin
  if keyword_set(fixed_count_slicer) then begin
     n_sdata = n_elements(sdata)
     n_per_bin = round( n_sdata / double(nslicer))

     ;; the borders of the bins we want are at the values of pixel
     ;; (0 .. n_per_bin*1 .. n_per_bin*2 .. n_per_bin*(nslicer-1)*0.5 .. n_sdata-0.5

     xxx = dindgen(n_sdata)
     yyy = sdata[sort(sdata)]
     foo = [n_per_bin*dindgen(nslicer)-0.5,n_sdata-0.5]
     borders = interpolate(foo,xxx,yyy)

  endif else begin
     ;; bins that are equally space (possibly in log)
     ;; enough values to hold the borders of the bins
     foo = dindgen(nslicer+1)/(nslicer)
     if keyword_set(log_slicer) then begin
        borders= min_slicer * 10 ^ foo * alog10(max_slicer/min_slicer)
     endif else begin
        borders = min_slicer + foo * (max_slicer-min_slicer)
     endelse

  endelse
  slimit_min=borders[0:nslicer-1]
  slimit_max=borders[1:nslicer]
  slimit_mean=(slimit_min+slimit_max)/2d0

  for ss=0,nslicer-1 do begin
     ;; select to pixels that fall in this bin
     iii = where(sdata ge slimit_min[ss] and sdata le slimit_max[ss],niii)
     ;; only do something if there are pixels in the bin
     if niii gt 0 then begin
        this_adata = adata[iii]
        this_astdv = astdv[iii]
        this_bdata = bdata[iii]
        this_bstdv = bstdv[iii]
        this_sdata = sdata[iii]

        this_observed_ratio = this_adata/this_bdata
        this_target_ratio = median(this_observed_ratio)

        ;; expand the target_ratio array such that we can perturb the
        ;; values
        this_target_ratio_array = make_array(niii,value=this_target_ratio)

        ;; perturb
        ;; should define an amplitude algorithm
        amplitude = 0.068
        this_target_ratio_array = (this_target_ratio + amplitude*randomn(seed,niii)) > 0d0
        
        ;; replace the actual values by some that are similar but respect the
        ;; target ratio target_a/target_b = target_ratio_perturbed
        ;; consider the target_X a correction on the observed value:
        ;; target_a = (1+x) * a
        ;; target_b = (1-x) * b
        ;; i.e. same percentage correction but in the opposite diretion
        ;; then if C = target_median / observed_median
        ;; x = (C-1)/(C+1)
        C = this_target_ratio_array/this_observed_ratio
        x = (C-1d0)/(C+1d0)
        this_target_adata = (1d0+x)*this_adata
        this_target_bdata = (1d0-x)*this_bdata
        
        ;; after talking to Rahul we decided it is more
        ;; straightforward to make many realisations and simply keep
        ;; track of the difference between the observed ratio and the
        ;; model.
        ;; The offset and the width are the information we want about
        ;; the residuals compared to the model (fixed ratio)

        ;; we generate many realisations of the ratio
        nmc = 10000L
        ;; keep track of the realistations
        realisations = make_array(niii,nmc,value=!values.d_nan)
        for mmm=0,nmc-1 do begin
           ;; cap the values to 0 like in the observations
           this_a = this_target_adata+this_astdv*randomn(seed,niii) > 0d0
           this_b = this_target_bdata+this_bstdv*randomn(seed,niii) > 0d0
           realisations[*,mmm] = this_a/this_b
        endfor
        
        ;; here we extract the important summary statistics
        ;; we want to get a distribution of synthetic values
        ;; one for each realistion (each line of realisations; dim=1)
        ;; 
        ;; there are several measures we could consider
        ;; median of the absolute deviation from the model ratio
        ;; mean   of the absolute deviation from the model ratio
        ;; mean   of the squared  deviation from the model ratio

        deviations_synth = realisations        - this_target_ratio
        median_abs_dev_synth     = median(abs(deviations_synth),dimension=1)
        mean_abs_dev_synth       = mean  (abs(deviations_synth),dimension=1)
        mean_squared_dev_synth   = sqrt(mean  ((deviations_synth)^2,dimension=1))

        ;; do the same but for the one realisation that is the data
        deviations_obsed = this_observed_ratio - this_target_ratio
        median_abs_dev_obsed     = median(abs(deviations_obsed))
        mean_abs_dev_obsed       = mean  (abs(deviations_obsed))
        mean_squared_dev_obsed   = sqrt(mean  ((deviations_obsed)^2))

        ;; make some histograms to compared observed with synthetic
        ;; deviation measures
        hmedian_abs_dev_synth   = histogram(median_abs_dev_synth  ,nbins=nmc/11.0,min=0,max=mean_squared_dev_obsed*2,location=xhist)
        hmean_abs_dev_synth     = histogram(mean_abs_dev_synth    ,nbins=nmc/11.0,min=0,max=mean_squared_dev_obsed*2)
        hmean_squared_dev_synth = histogram(mean_squared_dev_synth,nbins=nmc/11.0,min=0,max=mean_squared_dev_obsed*2)


        
        stop
        
     endif
  endfor
  
  return,0
end
