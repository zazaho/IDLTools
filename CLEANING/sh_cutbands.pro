;Remove some bad parts from aar data
FUNCTION sh_cutbands,in
  IF (n_elements(edges) lt 1) THEN BEGIN
    edges = [ [0,1d3], $                                              ;unknown
              [0,2.62],[0,1d3],[0,3.6 ],[3.4,1d3], $                  ;1a,1b,1d,1e
              [0,1d3],[0,1d3],[0,1d3], $                              ;2a,2b,2c
              [0,1d3], $                                              ;non exist
              [12.25,1d3],[0,1d3],[0,1d3],[27.5,29.0], $              ;3a,3c,3d,3e
              [28.7,1d3], $                                           ;4
              [0,0],[0,0],[0,0],[0,0],[0,0],[0,0], $                  ;nonexist
              [12.25,1d3],[0,1d3],[0,1d3],[27.5,29.0]]                ;4a,4c,3d
  ENDIF
  
  w = in.data.wave
  l = in.data.line
  
  ;; Get the corresponding line nmbers from sh_whichband
  foo = sh_whichband(in,line=l)
  
  ;; compare each wavelength with the given limits for that line(band)
  ;; (stored in edges)
  return,sh_select(in,(w GT edges[0,l]) AND (w LT edges[1,l]))
END
