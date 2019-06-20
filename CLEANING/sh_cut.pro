; Cut edges of the bands of aar data
; (SH Mar  2 2000) Much improved speed by smart testing and removal of
; line2index function
; (SH Apr  6 2000) Now uses sh_whichband in an attempt to centralize code
FUNCTION sh_cut,ain,edges=edges
  
  IF (n_elements(edges) lt 1) THEN BEGIN
    edges = [ [0,2000], $                                              ;unknown
              [2.38,2.62],[2.6,3.02],[3.0,3.52],[3.5,4.1], $           ;1a,1b,1d,1e
              [4.08,5.32],[5.3,7.05],[7.0,12.5], $                     ;2a,2b,2c
              [0,0], $                                                 ;non exist
              [12.45,16.55],[16.35,19.55],[19.5,27.55],[27.5,29.0], $  ;3a,3c,3d,3e
              [28.9,45.2], $                                           ;4
              [0,0],[0,0],[0,0],[0,0],[0,0],[0,0], $                   ;nonexist
              [12.45,16.55],[16.35,19.55],[19.5,27.55] ]               ;4a,4c,3d
  ENDIF
  
  w = ain.data.wave
  l = ain.data.line
  
  ;; Get the corresponding line nmbers from sh_whichband
  foo = sh_whichband(ain,line=l)
  
  ;; compare each wavelength with the given limits for that line(band)
  ;; (stored in edges)
  return,sh_select(ain,(w GT edges[0,l]) AND (w LT edges[1,l]))
  
END

