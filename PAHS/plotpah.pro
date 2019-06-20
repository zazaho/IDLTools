PRO plotpah,points,origin=orig,rotation=rota,size=size,ring=ring, $
            hydro=hydr,label=label,nooplot=nooplot,fullhydro=fullhydr, $
            aspect=aspect,_extra=_extra
  ;; Procedure to plot a pah ring on the current plot
  ;;    _   _   _   _
  ;; 1 /1\_/3\_/5\_/7\_ 
  ;; 2 \_/2\_/4\_/6\_/8\_
  ;; 3 /1\_/3\_/5\_/7\_/9\       6   1
  ;; 4 \_/2\_/4\_/6\_/ \_/        \_/
  ;; 5 /1\_/3\_/5\_/7\          5_/ \_2
  ;;   \_/ \_/ \_/ \_/            \_/
  ;;                              / \ 
  ;;                             4   3
  ;;
  ;; For this we need the following input:
  ;; 1) position of 1,1 (=origin)
  ;; 2) size of the ring (=size)
  ;; 3) which ring(s) to plot (x,y) (=points)
  ;; 4) a general rotation (=rotation)
  ;; options:
  ;; plot ring inside (ring=1)
  ;; which H bond to draw (hydro=[1,2,4])
  
  ;; The center of the (1,1) ring
  default,orig,[.2,.2]
  default,rota,0d0
  ;; Half the distance between two neighbouring centers (eg. 1,1-2,2)
  default,size,.02
  default,ring,0
  default,hydr,[0]
  default,label,''
  default,points,[1,1]
  IF keyword_set(fullhydr) THEN hydr = [1,2,3,4,5,6]
  ;; Determines the axis ratio of the y:x
  ;; give -1 for no aspecting
  ;; 0 for automatic
  ;; other take that
  default,aspect,0
  CASE aspect OF
    -1: y_scaling = 1d0
    0: y_scaling = double(!d.x_size)/double(!d.y_size)
    ELSE: 
  ENDCASE
  
  ;; Make an empty plot if nooplot wanted
  IF keyword_set(nooplot) THEN plot,[0,1],[0,1],xstyle=4,ystyl=4,/nod
  
  size_points = size(points)
  IF size_points[0] GT 1 THEN BEGIN
    npoints = size_points[2]
    shydr = size(hydr)
    nhydr = shydr[0]
    nlabel = n_elements(label)

    IF nhydr LT npoints THEN BEGIN
      lhydr = shydr[1]
      rest_hydr = make_array(lhydr,npoints)
      FOR j = 0, nhydr-1 DO BEGIN
        rest_hydr[j*lhydr:(j+1)*lhydr-1] = hydr[j*lhydr:(j+1)*lhydr-1]
      ENDFOR
      FOR j = nhydr,npoints-1 DO BEGIN
        rest_hydr[j*lhydr:(j+1)*lhydr-1] = hydr[(nhydr-1)*lhydr:nhydr*lhydr-1]
      ENDFOR
      hydr = rest_hydr
    ENDIF
    IF nlabel LT npoints THEN BEGIN
      rest_label = make_array(npoints-nlabel,value=label[nlabel-1])
      label = [label,rest_label]
    ENDIF
    FOR i = 0,npoints-1 DO BEGIN
      plotpah,points[*,i],origin=orig,rotation=rota,size=size,ring=ring, $
            hydro=hydr[*,i],label=label[i],_extra=_extra
    ENDFOR
    return
  ENDIF
  
  tan30 = 0.57735029d0
  sin30 = 0.5d0
  cos30 = 0.86602540d0
  
  bond_length = 2d0*size*tan30
  ;; The center is just a simple translation
  center = [orig[0]+size*points[0]*cos30*2,orig[1]+size*points[1]]
  norm_corners_x = center[0]+bond_length/2d0*[1d0,2d0,1d0,-1d0,-2d0,-1,1]
  norm_corners_y = center[1]+size*[1d0,0,-1d0,-1d0,0,1d0,1d0]
  corners = convert_coord(norm_corners_x,norm_corners_y,/normal,/to_data)
  
  oplot,corners[0,*],corners[1,*]*y_scaling,_extra=_extra
  
  ;; Put the label in
  IF (!p.charsize EQ 0) THEN charsize = 1 ELSE charsize = !p.charsize 
  normchar   = (convert_coord(1,!d.y_ch_size*charsize,/DEVICE,/TO_NORMAL))[1]
  lpos = convert_coord(center[0],center[1]-normchar/2.5,/norm,/to_dat)
  xyouts,lpos[0],lpos[1]*y_scaling,align=0.5,label,_extra=_extra
  
  ;; Draw the ring inside
  IF ring THEN BEGIN
    rxy = circle(0,0,size*.8)
    circ = convert_coord(center[0]+rxy[0,*],center[1]+rxy[1,*],/norm,/to_data)
    oplot,circ[0,*],circ[1,*]*y_scaling,linestyle=2
  ENDIF
  
  ;; Draw the hydrogens
  IF hydr[0] NE 0 THEN BEGIN
    FOR i = 0,n_elements(hydr)-1 DO BEGIN
      h_length = 1.5
      h_points_x = center[0]+[1,h_length]*bond_length* $
        cos(2d0*!pi*((2d0-hydr[i])/6d0))
      h_points_y = center[1]+[1,h_length]*bond_length* $
        sin(2d0*!pi*((2d0-hydr[i])/6d0))
      h_points = convert_coord(h_points_x,h_points_y,/normal,/to_data)
      oplot,h_points[0,*],h_points[1,*]*y_scaling,_extra=_extra
    ENDFOR
  ENDIF
END

