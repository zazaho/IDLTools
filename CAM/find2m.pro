PRO as_findmatch,max_dist=max_dist
  
  IF NOT keyword_set(max_dist) THEN BEGIN
      ans = ''
      print,'Please enter the maximum distance'
      read,ans
      max_dist = float(ans)
  ENDIF
  
  restore,'twomass_data.sav'
  T = twomass_data
  
  restore,'USNO_data.sav'
  U = usno_data
  
  FOR i = 0,n_elements(sources[0,*])-1 DO BEGIN
      x = sources[0,i]
      y = sources[1,i]
      
      ;; find the closest match
      dist = sqrt((T.ra-x)^2d0+(T.dec-y)^2d0)
      match = where(dist LE max_dist/3600.,cnt)
      
      IF cnt EQ 0 THEN BEGIN
          print,'no match found'
      ENDIF ELSE BEGIN
          imatch = match[sort(dist[match])]
          FOR j=0,cnt-1 DO BEGIN
              k = imatch[j]
              print, T.ra[k],T.dec[k],T.J[k],T.H[k],T.K[k],T.B[k],T.R[k],dist[k]*3600.
          ENDFOR
      ENDELSE
      print,'--------'
  ENDFOR 
END 

