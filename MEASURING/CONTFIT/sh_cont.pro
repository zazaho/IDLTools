;simple wrapper around sh_cont_fit
 ;usage:
 ;out = sh_cont(in[,sel],/option)
 ;in is the input aar
 ;sel holds the points the continuum is fitted to
 ;where option means:
 ;flamdba: do the fitting in flambda
 ;log    : do the fitting on a logarithmic scale
 ;nu     : do the fitting in Hz
;(SH Jan 29 1999)
; Added xlog ylog iras and min options

function sh_cont,in,selin,log=log,flambda=flambda,nu=nu, $
                 xlog=xlog,ylog=ylog,iras=iras,help=help, $
                 min=min,_extra=_extra

  if ((n_params() eq 0 ) or keyword_set(help)) then begin
    print,'simple wrapper around sh_cont_fit'
    print,'usage:'
    print,'out = sh_cont(in[,sel],/option,min=value)'
    print,'in is the input aar'
    print,'sel holds the points the continuum is fitted to'
    print,'where option means:'
    print,'flamdba: do the fitting in flambda'
    print,'log    : do the fitting on a logarithmic scale'
    print,'nu     : do the fitting in Hz'
    print,'xlog   : do the fitting in xlog'
    print,'ylog   : do the fitting in ylog'
    print,'iras   : do the fitting in lfl'
    print,'min=min: set values below min to min'
    return,0
  endif

;Check for valid input
  if not is_aar(in) then error,'F','No valid AAR structure specified!'

  if ((n_params() eq 1 ) or (n_elements(selin) eq 0)) then selin = in
  
     out  =    in
     wout =    out.data.wave
     fout =    out.data.flux
  
  selout  = selin
  selwout = selin.data.wave
  selfout = selin.data.flux
  
;(SH Jan 29 1999) set values below min to min
  if keyword_set(min) then begin
    idx = where(fout lt min,count)
    if (count gt 0) then begin
      fout(idx) = min
      print,'trown out everything below: ',min
    endif
  endif
  
  if keyword_set(flambda) then begin
    fout = fout*3e-12/wout^2*1e20
    selfout = selfout*3e-12/selwout^2*1e20
    print,'converted the fluxes to f_lambda'
  endif
  
  if keyword_set(iras) then begin
    fout = fout*3e-12/wout*1e20
    selfout = selfout*3e-12/selwout*1e20
    print,'Converted the fluxes to Lambda*F_Lambda'
  endif
  
  if keyword_set(nu) then begin
    wout = 3e14/wout
    selwout = 3e14/selwout
    print,'converted the waves to Hz or vice-versa'
  endif
  
  if keyword_set(log) then begin
      fout = alog10(fout)
      wout = alog10(wout)
      selfout = alog10(selfout)
      selwout = alog10(selwout)
      print,'Taken the logarithm of the wave,flux'
  endif
  
  if keyword_set(ylog) then begin
    fout = alog10(fout)
    selfout = alog10(selfout)
    print,'Taken the log10 of the flux'
  endif
   
  if keyword_set(xlog) then begin
      wout = alog10(wout)
      selwout = alog10(selwout)
      print,'Taken the log10 of the wave'
  endif
  
  out.data.wave=   wout
  out.data.flux=   fout
  selout.data.wave=selwout
  selout.data.flux=selfout
  
  out = sh_cont_fit(out,selout,_extra=_extra)
  
  selwout=selout.data.wave
  selfout=selout.data.flux
     wout=   out.data.wave
     fout=   out.data.flux
   
  if keyword_set(xlog) then begin
      wout = 10^(wout)
      selwout = 10^(selwout)
      print,'Taken the exp10 of the wave'
  endif
  
  if keyword_set(ylog) then begin
      fout = 10^(fout)
      selfout = 10^(selfout)
      print,'Taken the exp10 of the flux'
  endif
  
  if keyword_set(log) then begin
      fout = 10^(fout)
      wout = 10^(wout)
      selfout = 10^(selfout)
      selwout = 10^(selwout)
      print,'Taken the exp10 of the wave,flux'
  endif
  
  if keyword_set(iras) then begin
      fout = fout/3e-12*wout/1e20
      selfout = selfout/3e-12*selwout/1e20
      print,'Converted the fluxes back to f_nu'
  endif
  
  if keyword_set(flambda) then begin
      fout = fout/3e-12*wout^2/1e20
      selfout = selfout/3e-12*selwout^2/1e20
      print,'converted the fluxes back to f_nu'
  endif
  
  if keyword_set(nu) then begin
    wout = 3e14/wout
    selwout = 3e14/selwout
    print,'converted the waves to Hz or vice-versa'
  endif
  
     out.data.wave=   wout
     out.data.flux=   fout
  selout.data.wave=selwout
  selout.data.flux=selfout
  
  if (n_params() eq 2) then selin = selout
  
  return,out
  
end

