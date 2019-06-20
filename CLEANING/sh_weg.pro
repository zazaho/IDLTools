FUNCTION sh_weg,in,itk=itk,det=det,box=box,bands=bands,tint=tint,diff=diff, $
                updet=updet,downdet=downdet,ditk=ditk
  out = in
  
  IF keyword_set(bands) THEN BEGIN
      edges = [ [0,1d3], $                                              ;;;unknown
                [0,2.62],[0,1d3],[0,3.6 ],[3.4,1d3], $                  ;;;1a,1b,1d,1e
                [0,1d3],[0,1d3],[0,1d3], $                              ;;;2a,2b,2c
                [0,1d3], $                                              ;;;non exist
                [12.25,1d3],[0,1d3],[0,1d3],[27.5,29.0], $              ;;;3a,3c,3d,3e
                [28.7,1d3], $                                           ;;;4
                [0,0],[0,0],[0,0],[0,0],[0,0],[0,0], $                  ;;;nonexist
                [12.25,1d3],[0,1d3],[0,1d3],[27.5,29.0]]                ;;;4a,4c,3d
      
      w = out.data.wave
      l = out.data.line
      
      ;; Get the corresponding line nmbers from sh_whichband
      foo = sh_whichband(out,line=l)
      
      ;; compare each wavelength with the given limits for that line(band)
      ;; (stored in edges)
      out = sh_select(out,(w GT edges[0,l]) AND (w LT edges[1,l]))
  ENDIF
  
  IF keyword_set(tint) THEN BEGIN
    CASE (tint) OF
      1: BEGIN
        mtint = max(out.data.tint)
        mpoint = n_elements(out.data.tint)
        FOR i=mtint,round(.5*mtint),-1 DO BEGIN
          foo = where(out.data.tint LT i,count)
          print, $
            format='("Alles weg met tint < ",I2," gooit: ",F6.2,"% weg")', $
            i,double(count)/double(mpoint)*1d2
        ENDFOR
        print,'Kies de grens:'
        read,i
        out = sh_select(out,out.data.tint GE i)
      END
      ELSE: out = sh_select(out,out.data.tint GE tint)
    ENDCASE
  ENDIF
  
  IF keyword_set(det) THEN BEGIN
    out = sh_select(out,sh_comparray(out.data.det,det) EQ 0)
  ENDIF
  
  IF keyword_set(updet) THEN BEGIN
    out = sh_select(out,(sh_comparray(out.data.det,updet) AND (out.data.sdir eq -1)) EQ 0)
  ENDIF
  
  IF keyword_set(downdet) THEN BEGIN
    out = sh_select(out,(sh_comparray(out.data.det,downdet) AND (out.data.sdir eq 1)) EQ 0)
  ENDIF
  
  IF keyword_set(itk) THEN BEGIN
    out = sh_select(out,(out.data.itk lt itk[0]) or $
                    (out.data.itk gt itk[1]))
  ENDIF

  IF keyword_set(ditk) THEN BEGIN
      out = sh_select(out, $
                      (out.data.det ne ditk[0]) or $
                      (out.data.itk lt ditk[1]) or $
                      (out.data.itk gt ditk[2]))
  ENDIF
  
;;; box:
;;; 0 bandnaam eg '4'
;;; 1 scandir eq up
;;; 2 detector eq 37
;;; 3,4 x1,x2
;;; 5,6 y1,y2
  
  IF keyword_set(box) THEN BEGIN
    out = sh_select(out, $
                 (out.data.line NE box[0]) OR $
                 (out.data.sdir NE box[1]) OR $
                 (out.data.det  NE box[2]) OR $
                 (out.data.wave LT box[3]) OR $
                 (out.data.wave GT box[4]) OR $
                 (out.data.flux LT box[5]) OR $
                 (out.data.flux GT box[6]))
  ENDIF
  
  IF keyword_set(diff) THEN BEGIN
    ;; First elements is the detector
    det = diff[0]
    ; Second element is the offset (itk(0))
    bads = diff[2:*]+diff[1]
    weg = ((out.data.det EQ det) AND sh_comparray(out.data.itk,bads))
    out = sh_select(out,(weg EQ 0))
  ENDIF
  
  return,out
END
