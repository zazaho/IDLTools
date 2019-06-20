PRO oplot_aper,in,band,radec,size,_extra=_extra
  ;; from the size we get the scale
  xscale = !d.x_size/double(size)
  yscale = !d.y_size/double(size)
  radecs = sh_aper(in,band)
  p0 = (coord2pix(radec,1d0,radecs[*,0]))*[xscale,yscale]+[!d.x_size/2d0,!d.y_size/2d0]
  p1 = (coord2pix(radec,1d0,radecs[*,1]))*[xscale,yscale]+[!d.x_size/2d0,!d.y_size/2d0]
  p2 = (coord2pix(radec,1d0,radecs[*,2]))*[xscale,yscale]+[!d.x_size/2d0,!d.y_size/2d0]
  p3 = (coord2pix(radec,1d0,radecs[*,3]))*[xscale,yscale]+[!d.x_size/2d0,!d.y_size/2d0]

  plots,[p0[0],p1[0],p2[0],p3[0],p0[0]],[p0[1],p1[1],p2[1],p3[1],p0[1]],_extra=_extra,/device
END
