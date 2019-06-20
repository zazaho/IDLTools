;; take the 
FUNCTION percentile, indata, percent,top=top,bottom=bottom, $
                     normalise=normalise,replace=replace

  default,percent,99d0
  IF NOT keyword_set(bottom) AND NOT keyword_set(top) THEN BEGIN
     top = 1
     bottom = 1
  ENDIF

  ;; make sure the input array is not changed
  data = indata
  i = sort(data)
  d = data[i]
  nd = n_elements(d)
  
  IF keyword_set(top) THEN BEGIN
     toohigh = (data GT d[round(percent/1d2*nd)])
     IF total(toohigh) NE 0 THEN BEGIN
        IF keyword_set(replace) THEN BEGIN
           data[where(toohigh)] = d[round(percent/1d2*nd)]
        ENDIF ELSE BEGIN
           data = data[where(NOT toohigh)]
        ENDELSE
     ENDIF
  ENDIF

  IF keyword_set(bottom) THEN BEGIN
     toolow = (data LT d[round((1d2-percent)/1d2*nd)])
     IF total(toolow) NE 0 THEN BEGIN
        IF keyword_set(replace) THEN BEGIN
           data[where(toolow)] = d[round((1d2-percent)/1d2*nd)]
        ENDIF ELSE BEGIN
           data = data[where(NOT toolow)]
        ENDELSE
     ENDIF
  ENDIF

  IF keyword_set(normalise) THEN BEGIN
     mindata = min(data,max=maxdata)
     data = (data-mindata)/(maxdata-mindata)*normalise
  ENDIF

  return,data
END
