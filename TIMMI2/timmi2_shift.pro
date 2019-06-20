;; better interpolation routine

FUNCTION timmi2_shift,in,dx,dy
  ;; What we do is just a simple linear interpol which mean for dx=2.4
  ;; that we want ((1-.4)*dx2+.4*dx3) where dx2 is the image shifted
  ;; by 2 pixels and dx3 by 3 pixels.
  
  default,dx,0d0
  default,dy,0d0
  
  nx=n_elements(in[*,0])
  ny=n_elements(in[0,*])

  new_x=dindgen(nx)-dx
  new_y=dindgen(ny)-dy
  s  = interpolate(in,new_x,new_y,cubic=-0.5,/grid)
  return,s

END

