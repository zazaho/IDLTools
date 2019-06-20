PRO plotedges,xl=xl,yr=yr,nu=nu,edges=edges,_extra=_extra

  IF NOT keyword_set(edges) THEN BEGIN
      edges = [2.38,2.6,3.02,3.52,4.08, $
               5.3,7.,12.5,              $
               16.5,19.5,27.5,29.,      $
               45.2]
  ENDIF 
  
  IF keyword_set(nu) THEN edges = 3e14/edges
  
  IF keyword_set(xl) THEN BEGIN
      edges = alog10(edges)
  ENDIF 
  
  IF NOT keyword_set(yr) THEN BEGIN
      yr = !y.crange
      IF !y.type EQ 1 THEN yr = 1d1^yr
  ENDIF

  FOR i = 0,n_elements(edges)-1 DO BEGIN
      oplot,edges[i]*[1,1],yr,line=1,_extra=_extra
  ENDFOR 

END 
