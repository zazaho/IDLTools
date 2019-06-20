pro plr,ratios,var1,var2, $
        nooplot=nooplot, $
        smartscaling=smartscaling, $
        _extra=_extra
  
  default,smartscaling,0
  
  if n_params() eq 2 then begin
     foo = execute('y=ratios.R_'+var1)
     foo = execute('syd=ratios.SRD_'+var1)
     foo = execute('syu=ratios.SRU_'+var1)
     x = lindgen(n_elements(y))
     if smartscaling ne 0 then begin
        idx = where(finite(y))
        medy = median(y[idx])
        medsigy = median(sqrt((y[idx]-medy)^2))
        if medsigy lt 0.01 then medsigy = 1.5
        yrange=[(medy-3*medsigy)>1d-10,medy+3*medsigy]
     endif else begin
        yrange=[0,0]
     endelse
     if keyword_set(nooplot) then pl,x,y,/nodata,xtitle='#',ytitle=var1,yrange=yrange,_extra=_extra
     ple,x,y,y_errup=syu,y_errdown=syd,_extra=_extra
  endif else begin
     foo = execute('x=ratios.R_'+var1)
     foo = execute('sxd=ratios.SRD_'+var1)
     foo = execute('sxu=ratios.SRU_'+var1)
     foo = execute('y=ratios.R_'+var2)
     foo = execute('syd=ratios.SRD_'+var2)
     foo = execute('syu=ratios.SRU_'+var2)
     if smartscaling ne 0 then begin
        idx = where(finite(x))
        medx = median(x[idx])
        medsigx = median(sqrt((x[idx]-medx)^2))
        if medsigx lt 0.01 then medsigx = 1.5
        xrange=[(medx-3*medsigx)>1d-10,medx+3*medsigx]

        idx = where(finite(y))
        medy = median(y[idx])
        medsigy = median(sqrt((y[idx]-medy)^2))
        if medsigy lt 0.01 then medsigy = 1.5
        yrange=[(medy-3*medsigy)>1d-10,medy+3*medsigy]
     endif else begin
        xrange=[0,0]
        yrange=[0,0]
     endelse
     if keyword_set(nooplot) then pl,x,y,/nodata,xtitle=var1,ytitle=var2,xrange=xrange,yrange=yrange,_extra=_extra
     ple,x,y,x_errdown=syd,x_errup=sxu,y_errdown=sxd,y_errup=syu,_extra=_extra
  endelse

end
