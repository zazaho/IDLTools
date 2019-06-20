function sh_cow,in,xrange=xr,plot=plot,_extra=_extra
  if (n_elements(xr) eq 2) then begin
    a = sh_select_range(in,xr=xr,/q)
  endif else begin
    a = in
  endelse
  fa= a.data.flux
  wa= a.data.wave
  r = total(fa*wa)/total(fa)
  if keyword_set(plot) then begin
    pl,a,_extra=_extra
    oplot,[r,r],!y.crange
  endif
  return,r
end
