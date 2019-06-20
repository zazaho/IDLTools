FUNCTION slope_funct,x=x,par
   return, double(par[0])+x*double(par[1])
END

FUNCTION chi2_slope,x=x,y=y,err=err,par
   return, (y-slope_funct(par,x=x))/err
END

FUNCTION fit_slope, x_in,y_in,err_in, $
                    initial_par=initial_par,$
                    initial_scale=initial_scale
  
  x=x_in
  y=y_in
  if n_elements(err_in) eq n_elements(x) then begin
     err=err_in
  endif else begin
     err=x*0+median(y)
  endelse
  
  IF n_elements(initial_par) ne 2 then initial_par=[0,total(y)/total(x)]
  
  IF n_elements(initial_scale) ne 2 then initial_scale=initial_par*10
  
  parinfo = replicate({value:0D0, fixed:0, limited:[0,0],limits:[0D0,0d0]}, 2)
  
;; Set some reasonable constraints
  
  parinfo(*).value = initial_par
  
  ;; function arguments
  fa = {X:x, Y:y, ERR:err}
  
;;;1=fixed, 0=notfixed
  parinfo(*).fixed = [0,0]
  parinfo(*).limited = [[0,0],[1,0]]
  parinfo(*).limits = [[!values.d_nan,!values.d_nan],[0.,!values.d_nan]]
  
  par = mpfit('chi2_slope', initial_par, functargs=fa,parinfo=parinfo,ftol=1d-12)

  return,par


END
