;;(SH Mar 21 2001)
;; Function to take and image and shift it. 
;; We try this to see if we can improve on the resample-gaussfit-shift method
;; The idea is to take one image and shift the second such that after
;; subtraction the residue is minimal (see also timmi_shiftadd2.pro)

FUNCTION timmi_shift,in,dx,dy
  ;; What we do is just a simple linear interpol which mean for dx=2.4
  ;; that we want ((1-.4)*dx2+.4*dx3) where dx2 is the image shifted
  ;; by 2 pixels and dx3 by 3 pixels.
  
  default,dx,0d0
  default,dy,0d0
  
  fracx = dx-floor(dx)
  fracy = dy-floor(dy)
  sx = ((1d0-fracx)*shift(in,floor(dx),0)+fracx*shift(in,ceil(dx),0))
  s  = ((1d0-fracy)*shift(sx,0,floor(dy))+fracy*shift(sx,0,ceil(dy)))
  return,s
END

