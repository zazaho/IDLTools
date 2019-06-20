PRO plploint,filt_in,y_in,e_y_in,flambda=flambda,iras=iras,_extra=_extra
  
  flambda = keyword_set(flambda)
  iras = keyword_set(iras)

  filt = filt_in
  y = y_in
  e_y = e_y_in
  mean_filt = mean(filt)
  
;;Change the flux scales    
  IF flambda THEN BEGIN
     y = 3e-12*y/mean_filt^2d0*1d13
     e_y = 3e-12*e_y/mean_filt^2d0*1d13
  ENDIF
  
  IF iras THEN BEGIN
     y = 3e-12*y/mean_filt*1d12
     e_y = 3e-12*e_y/mean_filt*1d12
  ENDIF

  ;; the x bar
  oplot,filt,y*[1d0,1d0],ps=0,_extra=_extra
  ;; the y bar
  oplot,mean_filt*[1d0,1d0],[y-e_y,y+e_y],ps=0,_extra=_extra
END
