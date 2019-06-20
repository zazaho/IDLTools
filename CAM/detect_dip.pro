FUNCTION detect_dip,y_in,width=width,min_dip=min_dip,verbose=verbose

  default,width,61
  default,width2,21
  default,min_dip,0.03
  default,verbose,0

  y = collapse(y_in)

  median = median(y)
  stdev = (moment(y))[1]
  idx = where(abs(y-median) LE 3*stdev)
  stdev = (moment(y[idx]))[1]

  min_dip = (stdev/median>min_dip)
  ny = n_elements(y)

  ye = [make_array(width,val=median(y[0:width-1])),y,make_array(width,val=median(y[ny-1-width:*]))]

  ;; first we simply select on low points
  dip = where(y LT median*(1-min_dip),count)

  IF count eq 0 THEN return,-1
  dipe = dip+width

;; now we select those points that remain low FOR some time after the
;; dip
  aver_after = dipe*0d0
  FOR i=0,n_elements(dipe)-1 DO BEGIN
      aver_after[i] = mean(ye[dipe[i]:dipe[i]+width-1])
  ENDFOR 
  idx = where(aver_after LT (1-min_dip*0.7)*median,count)
  IF count EQ 0 THEN return,-1
  
  dipe = dipe[idx]    
  
  ;; now select those points that were high before
  ;; this rejects the tails themselves
  aver_step = dipe*0d0
  FOR i=0,n_elements(dipe)-1 DO BEGIN
      aver_step[i] = mean(ye[dipe[i]-width2:dipe[i]-1]) - $
        mean(ye[dipe[i]:dipe[i]+width2-1])
  ENDFOR 
  idx = where(aver_step GT 0.7*min_dip*median,count)
  IF count eq 0 THEN return,-1
  
  real_dipe = dipe[idx]
  real_dip = real_dipe - width
  
  ;; now we take only those that are far enough from each other and
  ;; that are not too close to the edges
  good = 0*real_dip+1
  good = good AND (real_dip GT 100)
  good = good AND (real_dip LT (ny-100))

  dist_dip = real_dip - shift(real_dip,1)
  dist_dip[0] = real_dip[0]
  
  good = good AND (dist_dip GT 100)

  idx = where(good EQ 1,count)

  IF count eq 0 THEN return,-1
  dip = real_dip[idx]

  IF verbose THEN BEGIN
      pl,y,ymin=0,/autoy,ps=3,xtit='time',ytit='readout'
      pl,(smooth(ye,21))[width:width+n_elements(y)-1],/opl
      pl,!x.crange,median*[1,1],thi=2,/opl
      pl,!x.crange,median*[1,1]*(1-min_dip),thi=2,/opl
      FOR i=0,n_elements(dip)-1 DO BEGIN
          pl,dip[i]*[1,1],!y.crange,/opl,col=100,lines=1
      ENDFOR
      wait,1
  ENDIF 
  return,dip
END
