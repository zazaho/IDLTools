                                ;(SH Jun 23 2002)
;; routine to draw an ellips on a plot
;; Usage:
;; sh_ellips,x0,y0,xsize,ysize[,options]
;; Where:
;; x0 the x coordinate of the center of the ellipse
;; y0 the y coordinate of the center of the ellipse
;; xsize the size of the ellipse in the x direction BEFORE rotation
;; ysize the size of the ellipse in the y direction BEFORE rotation
;; Options:
;; angle=a (the rotation angle counter clockwise that is applied to
;;       the ellipse 
;; npoints=np the number of points in the ellipse, more is smoother)
;; /fill fill the ellipse (extra options)
;;      fcolor = col use this color to fill the ellipse
;;      /line_fill fill with lines
;;           orientation=o angle of the lines
;;           spacing=s distance in cn between the lines

PRO sh_ellipse,x0,y0,xsize,ysize,angle=pa,npoints=npoints,fill=fill, $
               fcolor=fcolor,_extra=_extra
  
  IF NOT keyword_set(pa) THEN pa = 0
  IF NOT keyword_set(npoints) THEN npoints = 360
  
  ;; the angles
  phi = dindgen(npoints)/(npoints-1)*2*!dpi

  ;; Convert to radian
  pa = 2*!dpi/360d0*pa

  ;; convert the xsize and ysize to device coord
  foo = convert_coord([0,xsize],[0,ysize],[0,0],/data,/to_normal)
  xsize = foo[0,1,0]-foo[0,0,0]
  ysize = foo[1,1,0]-foo[1,0,0]

  ;; convert the x0 and y0 to device coord
  foo = convert_coord(x0,y0,0,/data,/to_normal)
  x0 = foo[0,0,0]
  y0 = foo[1,0,0]

  ;; make the ellipse
  x = xsize*cos(phi)
  y = ysize*sin(phi)

  ;; now apply the rotation
  xr = x*cos(pa)-y*sin(pa)
  yr = x*sin(pa)+y*cos(pa)
  
  ;; shift to the x0,y0
  x = xr + x0
  y = yr + y0

  ;; convert the elipse back to data points
  xy = convert_coord(x,y,/normal,/to_data)
  
  x = reform(xy[0,*],npoints)
  y = reform(xy[1,*],npoints)

  IF keyword_set(fill) THEN BEGIN
      IF NOT keyword_set(fcolor) THEN fcolor = !p.color
      x = [x,x[0]]
      y = [y,y[0]]
      polyfill,x,y,_extra=_extra,color=fcolor
  ENDIF 

  oplot,x,y,ps=0,_extra=_extra
END

;Polyfill:
;Fill Methods
;
;Line-fill method: Filling using parallel lines is device-independent
;and works on all devices that can draw lines. Crosshatching can be
;simulated by performing multiple fills with different
;orientations. The spacing, linestyle, orientation, and thickness of
;the filling lines can be specified using the corresponding keyword
;parameters. The LINE_FILL keyword selects this filling style, but is
;not required if either the ORIENTATION or SPACING parameters are
;present.
;
;Solid fill method: Most, but not all, devices can fill with a solid
;color. Solid fill is performed using the line-fill method for devices
;that don't have this hardware capability. Method specifying keyword
;parameters are not required for solid filling.
;
;
;LINE_FILL
;
;Set this keyword to indicate that polygons are to be filled with
;parallel lines, rather than using solid or patterned filling
;methods.When using the line-drawing method of filling, the thickness,
;linestyle, orientation, and spacing of the lines may be specified with
;keywords.
;
;SPACING
;
;The spacing, in centimeters, between the parallel lines used to fill
;polygons.
;
;
