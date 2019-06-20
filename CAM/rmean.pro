FUNCTION rmean,x,y,width,width=w
  
  default,w,100
  default,width,w
  nx = n_elements(x)
  out = y
  
  FOR i = 0,nx-1 DO BEGIN
      wleft = x[i]-min(x[where(x GE (x[i]-width))])
      wrght = max(x[where(x LE (x[i]+width))])-x[i]
      wtot = (wleft<wrght)>10
      idx = where( (x GE (x[i]-wtot)) AND (x LE (x[i]+wtot)))
      out[i] = mean(y[idx])
  ENDFOR 
  return,out
END 
