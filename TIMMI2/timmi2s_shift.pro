;;(SH Mar 21 2001)
;; Function to take and image and shift it. 

FUNCTION timmi2s_shift,in,dx,dy
  ;; What we do is just a simple linear interpol which mean for dx=2.4
  ;; that we want ((1-.4)*dx2+.4*dx3) where dx2 is the image shifted
  ;; by 2 pixels and dx3 by 3 pixels.
  
  default,dx,0d0
  default,dy,0d0
  
  floordx = floor(dx)
  floordy = floor(dy)
  fracdx = dx-floordx
  fracdy = dy-floordy
  sx = ((1d0-fracdx)*shift(in,floordx,0)+fracdx*shift(in,floordx+1,0))
  s  = ((1d0-fracdy)*shift(sx,0,floordy)+fracdy*shift(sx,0,floordy+1))
  return,s
END

