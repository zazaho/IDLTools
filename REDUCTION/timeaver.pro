pro timeaver,in,_extra=_extra
  itk = in.data.itk - sh_itk0(in)
  flux = in.data.flux
  idx = sort(itk)
  itk = itk[idx]
  flux = flux[idx]
  itk_uniq = itk[uniq(itk)]
  tot_flux = itk_uniq
  for i = 0,n_elements(itk_uniq) - 1 do begin
    tot_flux(i) = total(flux(where(itk eq itk_uniq[i])))/ $
      total(itk eq itk_uniq[i])
  endfor
  plot,itk_uniq,tot_flux,ps=0,_extra=_extra
end
