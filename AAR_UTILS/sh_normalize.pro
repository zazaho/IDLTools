FUNCTION sh_normalize,aar,normxy
  out = aar
  idx = sort(out.data.wave)
  out.data.flux = out.data.flux * normxy[1] / $
    interpol(out.data.flux[idx],out.data.wave[idx],normxy[0]) 
  return,out
END 
