function syntspec,ref,l
  out = ref
  out.data.flux = 0d0
  flx = out.data.flux
  wv = out.data.wave
  for i = 0,n_elements(l)-1 do begin
    flx = l(i).gmaxy* $
      exp(-1d0*(((l(i).gmaxx-wv)/l(i).gwidth)^2d0)/2d0)+flx
  endfor
  out.data.flux = flx
  return,out
end
