;; Determine the minima in an aar around a center
;; Use only on smooth data
FUNCTION nearest_around,a,val
  b = a-val
  return,[max(b[where(b LT 0)])+val,min(b[where(b GE 0)])+val]
END

FUNCTION local_extremes,p1,p2,around=around,min=min,max=max,getmin=getmin,getmax=getmax
  
  usage = 'LOCAL_EXTREMES: usage: extremes = local_extremes(aar|x,y[,min=min,max=max,around=around])'
  CASE n_params() OF
    0: BEGIN
      print,usage
      return,0
    END
    1: BEGIN
      IF n_tags(p1) GT 3 THEN BEGIN
        x = p1.data.wave
        y = p1.data.flux
      ENDIF
    END
    ELSE: BEGIN
      x = p1
      y = p2
    END
    
  ENDCASE
  
  ;;Find the local minima by shifting the arrays and subtracting. 
  ;;these points where the signs change or are at most 0 are the local
  ;;extremes
  diffl = y-shift(y,-1)
  diffr = shift(y, 1)-y
  
  ext = x[where(  diffl*diffr LE 0                   )]
  min = x[where( (diffl*diffr LE 0)  AND (diffl LE 0))]
  max = x[where( (diffl*diffr LE 0)  AND (diffr LE 0))]
  
  IF keyword_set(around) THEN BEGIN
    min = nearest_around(min,around)
    max = nearest_around(max,around)
    ext = nearest_around(ext,around)
  ENDIF
  
  CASE 1 OF
    keyword_set(getmin): return,min
    keyword_set(getmax): return,max
    ELSE: return,ext
  ENDCASE
  
END
