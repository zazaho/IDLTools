FUNCTION fit_broken_powerlaw_model, par
  COMMON data,logx,logy,stdev_logy

;; parameters:
;; p[0] + p[1]*logx : logx < p[3] (Line A)
;; C    + p[2]*logx : logx > p[3] (Line B)
;;
;; Line A at p[3] equals p[0] + p[1]*p[3]
;; Line B at p[3] equals C    + p[2]*p[3]
;;
;; a contineous line means these two have to be equal thus:
;; C + p[2]*p[3] = p[0] + p[1]*p[3]
;; C = p[0] + (p[1]-p[2])*p[3] 
;; 
;; The full equation is thus

;; logy = p[0]                    + p[1]*logx : logx < p[3]
;; logy = p[0] + (p[1]-p[2])*p[3] + p[2]*logx : logx > p[3]

  idx = where(logx gt par[3])
  
  model_logy      = par[0] + par[1]*logx
  model_logy[idx] = par[0] + (par[1]-par[2])*par[3] + par[2]*logx[idx]
  
  return, model_logy

END


FUNCTION fit_broken_powerlaw_deviation, par
  COMMON data,logx,logy,stdev_logy

   model_logy=fit_broken_powerlaw_model(par)

   return, (logy-model_logy)/stdev_logy

END

function fit_broken_powerlaw,x,y,dy, $
                             initial_par=initial_par, $
                             initial_scale=initial_scale, $
                             ymodel=ymodel, $
                             xmax=xmax, $
                             xmin=xmin
  
  COMMON data,logx,logy,stdev_logy

  default,xmin,0. ;; arcsec
  default,xmax,100. ;; arcsec

  ;;; define range to be fitted
  idx = where(x le xmax,cnt)
  
  if cnt eq 0 then begin
     message,/info,'No radii smaller than ',xmax
     return,0
  endif
  
  logx = alog10(x[idx])
  logy = alog10(y[idx])
  stdev_logy = (dy[idx]/y[idx]) > 0.01
  
  max_logx = max(logx,min=min_logx)
  max_logy = max(logy,min=min_logy)
  
  ;; reasonable values that avoid falling in a local minimum
  IF n_elements(initial_par) ne 4 then initial_par=[ $
     max_logy, $
     (max_logy-min_logy)/(max_logx-min_logx), $
     (max_logy-min_logy)/(max_logx-min_logx), $
     (max_logx+min_logx)/2d0 $
                                                   ]
  
   IF n_elements(initial_scale) ne 4 then initial_scale=initial_par/2d0

   parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0]}, 4)

   parinfo[*].value = initial_par

;   parinfo[0].limited = [1,1]
;   parinfo[0].limits = [XXX,XXX]

   best_fit_par = mpfit('fit_broken_powerlaw_deviation',initial_par,parinfo=parinfo)

;; calculate the model for the whole range

   logx = alog10(x)
   logy = alog10(y)

   model_logy = fit_broken_powerlaw_model(best_fit_par)
   ymodel = 10d0^model_logy

   print,'***********************'
   print,'First powerlaw: ' +string(10^ best_fit_par[0])+' *x^'+string(best_fit_par[1])
   print,'Second powerlaw: '+string(10^(best_fit_par[0] + (best_fit_par[1]-best_fit_par[2])*best_fit_par[3]))+' *x^'+string(best_fit_par[2])
   print,'Breaking point: '+string(10^best_fit_par[3])+' arcsec'
   print,'***********************'

   return,best_fit_par

END
