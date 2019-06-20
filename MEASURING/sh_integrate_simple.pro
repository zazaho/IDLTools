FUNCTION sh_integrate_simple,x,y,xrange=xrange

  xx = double(x)
  yy = double(y)

  idx = sort(xx)
  xx=xx[idx]
  yy=yy[idx]

  ;;; Take only the part of interest
  IF keyword_set(xrange) THEN BEGIN
     idx = where((xx ge min(xrange)) and (xx ge max(xrange)),cnt)
     if cnt ne 0 then begin
        xx = xx[idx]
        yy = yy[idx]
     endif else begin
        message,/info,'no points in the required xrange'
        return,!values.d_nan
     endelse
  ENDIF
  
  npoints = n_elements(xx)
  
  if (npoints lt 3) then begin
     message,/info,'not enough points to do an integration'
     return,!values.d_nan
  endif

  ;; size of each bin
  binsize = (shift(xx,-1)-xx)[0:npoints-2]
  
  ;; average height in each bin
  halfheight = ((shift(yy,-1)+yy)[0:npoints-2])/2d0

  return, total(binsize*halfheight)
END
