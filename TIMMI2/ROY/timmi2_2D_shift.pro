FUNCTION timmi2_2D_shift,in,dx,dy
  x=in[*,0]
  y=in[0,*]
  
  for i=0,(n_elements(x)-1) do begin
    x[i]=i-dx
  endfor
  for i=0,(n_elements(y)-1) do begin
    y[i]=i-dy
  endfor

  s=interpolate(in,x,y,cubic=-0.5,/grid)
  return,s
END
