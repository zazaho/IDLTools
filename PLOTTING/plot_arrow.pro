FUNCTION pla_head_points,ax,ay,hscale,hsize=hsize
  dx = ax[1]-ax[0]
  dy = ay[1]-ay[0]
  length = sqrt(dx^2d0+dy^2d0)
  IF dx NE 0d0 THEN BEGIN
      angle = atan(dy/dx) 
  ENDIF ELSE BEGIN
      angle = dy/abs(dy)*!PI/2d0
  endeLSE

  IF dx LT 0d0 THEN angle=angle+!PI
  ;; in the frame of the arrow
  normx = length-length*hscale*0.0866
  normy =        length*hscale*0.05
  IF keyword_set(hsize) THEN BEGIN 
      normx = length-hsize*0.0866
      normy =        hsize*0.05
  ENDIF
  pt1x = ax[0]+cos(angle)*normx-sin(angle)*normy
  pt1y = ay[0]+sin(angle)*normx+cos(angle)*normy
  pt2x = ax[0]+cos(angle)*normx+sin(angle)*normy
  pt2y = ay[0]+sin(angle)*normx-cos(angle)*normy
  pts = [[pt1x,ax[1],pt2x],[pt1y,ay[1],pt2y]]
  return,pts
END

FUNCTION pla_base_points,ax,ay,hscale,hsize=hsize
  dx = ax[1]-ax[0]
  dy = ay[1]-ay[0]
  
  length = sqrt(dx^2d0+dy^2d0)
  IF dx NE 0d0 THEN BEGIN
     angle = atan(dy/dx) 
  ENDIF ELSE BEGIN
     angle = dy/abs(dy)*!PI/2d0
  endeLSE
  IF dx LT 0d0 THEN angle=angle+!PI
  
  ;; in the frame of the arrow
  normy =        length*hscale*0.05
  IF keyword_set(hsize) THEN BEGIN 
      normy =        hsize*0.05
  ENDIF
  pt1x = ax[0]-sin(angle)*normy
  pt1y = ay[0]+cos(angle)*normy
  pt2x = ax[0]+sin(angle)*normy
  pt2y = ay[0]-cos(angle)*normy
  pts = [[pt1x,pt2x],[pt1y,pt2y]]
  return,pts
END


;; Simple routine to plot a line and make a proportional arrow on it
;; Mode bit wise operator to
;; Mode can be 1: arrow on the start
;;             2: arrow on the end
;;            (3: arrow on both size)
;;             4: fill the head
;;             8: only draw head (don't draw lines) 
PRO plot_arrow,x_in,y_in,mode=mode,hscale=hscale,help=help,fill=fill, $
               spline=splinep,todevice=todevice,hsize=hsize, $
               drawbase=drawbase,_extra=_extra

  default,hscale,1.0
  default,mode,3
  default,help,0
  default,hsize,0

  ;; To plot in device coords
  dvc=0
  x=double(x_in)
  y=double(y_in)

  dofill = ((mode AND 4) EQ 4) OR keyword_set(fill)
  
  nx = n_elements(x)
  ny = n_elements(y)
  
  IF (nx LT 2) OR (ny NE nx) OR help THEN BEGIN
      print,'---------------PLOT_ARROW-----------------------------'
      print,'PLOT_ARROW: usage: plot_arrow,x,y,mode=mode,hscale=hs,'
      print,'                              spline=spl,/todevide,/help'
      print,'Where: x, y arrays of points for the line'
      print,' mode is bitwise (default 3):'
      print,' 1: arrow on the start'
      print,' 2: arrow on the end'
      print,'(3: arrow on both size)'
      print,' 4: fill the head'
      print," 8: only draw head (don't draw lines)"
      print,'hscale=hs: scale head by factor hs from default'
      print,'spline=spl: smooth the lines by splining to a finer grid'
      print,'/todevice: convert to device coordinates to make a nice head'
      print,'/help: this help'
      print,'---------------PLOT_ARROW-----------------------------'
      return
  ENDIF
  
  IF keyword_set(splinep) THEN BEGIN
      x = spline(dindgen(n_elements(x)),x,dindgen((n_elements(x)-1)*splinep+1)/splinep)
      y = spline(dindgen(n_elements(y)),y,dindgen((n_elements(y)-1)*splinep+1)/splinep)
      
;      minx = min(x,max=maxx)
;      xx = dindgen(nx*spline)/(nx*spline-1)*(maxx-minx)+minx
;      y = shc_spline(x,y,xx)
;      x = xx
      nx = n_elements(x)
      ny = n_elements(y)
      hscale=hscale*splinep
  ENDIF

  IF (mode AND 8) NE 8 THEN BEGIN
      ;; We want to draw the line too
      plots,x,y,_extra=_extra
  ENDIF
  
  ;; This is to convert to device coordinates before plotting to make
  ;; the heads on the right angles and on the right scale
  IF keyword_set(todevice) THEN BEGIN
      transf = convert_coord(x,y,/to_device)
      x = transf[0,*]
      y = transf[1,*]
      dvc=1
  ENDIF

  IF (mode AND 1) EQ 1 THEN BEGIN
      ax = x[[1,0]]
      ay = y[[1,0]]
      pts = pla_head_points(ax,ay,hscale,hsize=hsize)
      IF dofill THEN BEGIN
          polyfill,[pts[*,0],pts[0,0]],[pts[*,1],pts[0,1]],dev=dvc, $
                   _extra=_extra
      ENDIF ELSE BEGIN
          plots,pts[*,0],pts[*,1],dev=dvc,_extra=_extra
      ENDELSE
  ENDIF

  IF (mode AND 2) EQ 2 THEN BEGIN
      ax = x[nx-2:nx-1]
      ay = y[nx-2:nx-1]
      pts = pla_head_points(ax,ay,hscale,hsize=hsize)
      IF dofill THEN BEGIN
          polyfill,[pts[*,0],pts[0,0]],[pts[*,1],pts[0,1]],dev=dvc, $
                   _extra=_extra
      ENDIF ELSE BEGIN
          plots,pts[*,0],pts[*,1],dev=dvc,_extra=_extra
      ENDELSE
  ENDIF

  IF keyword_set(drawbase) THEN BEGIN
      ax = x[[0,1]]
      ay = y[[0,1]]
      pts = pla_base_points(ax,ay,hscale,hsize=hsize)
      plots,pts[*,0],pts[*,1],dev=dvc,_extra=_extra
  ENDIF

END
