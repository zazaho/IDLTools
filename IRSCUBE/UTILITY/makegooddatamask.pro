function makegooddatamask,data,unc,xrange=xrange,snr=snr

  mask = make_array(n_elements(data.cube[*,0,0]),n_elements(data.cube[0,*,0]),value=0)

  waves = data.wave

  idx =  where( waves gt xrange[0] and waves lt xrange[1],count)
  
  if count eq 0 then begin
     message,/info,"wavelengths are not found"
     return,mask
  endif

  total_signal = total(data.cube[*,*,idx],3)
  total_error  = sqrt(total(unc.cube[*,*,idx],3)^2)

  sufficient_idx = where(total_signal/total_error gt snr,count)

;  plot,total_signal,total_signal/total_error,ps=1
;  wait,5

  if count ne 0 then begin
     mask[sufficient_idx] = 1
  endif

  message,/info,'gooddatamask has '+string(total(mask))+' out of '+string(n_elements(mask))+' pixels'
 
  return,mask

end

