FUNCTION sh_iras2aar,in
  n_in = n_elements(in)
  out = makeaar(len=n_in)
  out.data.wave = ([12,25,60,100])[0:n_in-1]
  out.data.flux = in
  return,out
END
