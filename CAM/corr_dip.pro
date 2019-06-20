;; this routine corrects a broad median of the detected signal:
;; However it can break the signal of a single pixel in time is pieces
;; if a dip is detected. This allows for a slightly better
;; determinations of the dip tail

PRO corr_dip,raster,domean=domean,level=level,plot=plot, $
             width=width
  
  default,domean,0
  default,level,0.03
  default,plot,0
  default,width,61
  
  cube = raster.cube
  mask = raster.mask
  use  = (mask AND 1) EQ 0
  all_x = indgen(n_elements(raster.cube[0,0,*]))
  
  FOR i=0,31 DO BEGIN
      print,'starting column:',i
      FOR j=0,31 DO BEGIN
          ;; only use the not masked points
          idx = where(use[i,j,*] EQ 1,count)
          IF count NE 0 THEN begin
              x = idx
              y = cube[i,j,idx]
              ;; shoud do this if the discrete readout noise is important
              IF domean THEN y = rmean(x,y,10)
              tot_median = median(y)
              ;; call the routine to find the step down in flux with
              ;; tail (dipper)
              dips = detect_dip(y,min=level,width=width)
              IF dips[0] NE -1 THEN BEGIN 
                  start = [0,dips]
                  eind  = [dips-1,n_elements(y)-1]
              ENDIF  ELSE BEGIN
                  start = [0]
                  eind = [n_elements(y)-1]
              ENDELSE 
              ;; now determine a running median for each segment
              mdn_y = y*0d0
              FOR k=0,n_elements(start)-1 DO BEGIN
                  mdn_y[start[k]:eind[k]] = rmedian(x[start[k]:eind[k]], $
                                                    y[start[k]:eind[k]])
              ENDFOR
              ;; interpolate for the masked points
              mdn_y = interpol(mdn_y,x,all_x,/qua)
              ;; here we apply the correction:
              ;; we divided by the running median and multiply by
              ;; the total median
              ;; This assumes a dip in sensitivity
              ;; we remove the total median
              cube[i,j,*] = $
                cube[i,j,*]*tot_median/mdn_y-tot_median
              IF plot THEN BEGIN
                  pl,x,y,ymin=0,xtit='time',ytit='readout',/autoy,ps=3
                  pl,x,rmean(x,cube[i,j,x],10)+tot_median,/opl,ps=3
              ENDIF 
          ENDIF 
      ENDFOR 
  ENDFOR  
  ;; correct col 23 
  cube[23,*,*] = (cube[22,*,*]+cube[24,*,*])/2d0
  raster.cube = cube
END
