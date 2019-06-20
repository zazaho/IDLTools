PRO histfill,x,y,ymin=ymin, $
             _extra=_extra

  default,ymin,0d0
  
  nx = n_elements(x)

;; sort the values in creasing x  
  idx = sort(x)
  xx = x[idx]
  yy = y[idx]
  
  ;; find the middle values between the points
  xmean = (shift(xx,1)+xx)/2d0
  ;; these are the positions of each side to be drawn
  xxx = [xx[0],xmean[1:*],xx[nx-1]]
  ;; create an index that looks like [0,0,1,1,...,N,N] to duplicate
  ;; the values in order
  idx2 = reform([1,1]#indgen(nx+1),2*(nx+1))
  xxxx = xxx[idx2]

  ;; add zero at the end of the Y values
  yyy = [yy,ymin]
  ;;duplicate the values in order 
  yyyy = yyy[idx2]

  ;; move the last value to the beginning:
  yyyy = shift(yyyy,1)

  polyfill,xxxx,yyyy,_extra=_extra
END
