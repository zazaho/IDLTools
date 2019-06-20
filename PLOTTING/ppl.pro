pro ppl,in,power,title=title,_extra=_extra
  if (n_params() eq 2) then begin
    if (keyword_set(title)) then begin
      title = title + ' P_lambda = '+STRING(power )
    endif else begin 
      title = 'P_lambda = '+STRING(power)
    endelse
    pl,plambda(in,power),title = title,ytit='',_extra=_extra
  endif
  if (not keyword_set(title)) then title = ' ' 
  if (n_params() eq 1) then pl,in,title=title,_extra=_extra
end
