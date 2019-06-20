PRO plcirc, center, radi,npoints=npoints,_extra=_extra

  default,center,[0,0]
  default,radi,1d0
  default,npoints,36

  rads = dindgen(npoints+1)/npoints*2d0*!dpi
  
  x_unit_circle = cos(rads)
  y_unit_circle = sin(rads)

  FOR i=0,n_elements(radi)-1 DO BEGIN
     plots,x_unit_circle*radi[i]+center[0], $
           y_unit_circle*radi[i]+center[1], $
           _extra=_extra
  ENDFOR
END
