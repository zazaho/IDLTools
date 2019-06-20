; Function to read an lws fits file into a sws/aar
; copies all data structures that are available

                                ;(SH Nov  5 2003)
; Changed to use mrdfits rather than sap_rfits because it gave errors
; in update_history when no journal file was open

FUNCTION read_flws,fname
  IF file_test(fname) EQ 1 THEN BEGIN
      tmp = mrdfits(fname,0,hdr1)
      tmp = mrdfits(fname,1,hdr2)
      len = n_elements(tmp.wave)
      out = sh_define_aar(len=len)
      hdr = ''
      FOR  i=0,n_elements(hdr1)-1 DO hdr = hdr+hdr1[i]
      FOR  i=0,n_elements(hdr2)-1 DO hdr = hdr+hdr2[i]
      out.header  = hdr
      keys = STRUPCASE(tag_names(tmp)) 
      aar_keys = STRUPCASE(tag_names(out. data))
     
      FOR i = 0,n_elements(keys)-1 DO BEGIN
          IF ((where(aar_keys EQ keys[i]))(0) NE -1) THEN BEGIN
              f = execute('out.data.'+keys[i]+'=tmp.'+keys[i])
          ENDIF
      ENDFOR
      
;    stat = sap_rfits(fname,tmp)
;    len = n_elements(tmp.data)
;    out = sh_define_aar(len=len)
;    out.header  = tmp.header
;    out.history = tmp.history
;    keys = STRUPCASE(tag_names(tmp.data))
;    aar_keys = STRUPCASE(tag_names(out.data))
;    
;    FOR i = 0,n_elements(keys)-1 DO BEGIN
;        IF ((where(aar_keys EQ keys[i]))(0) NE -1) THEN BEGIN
;            f = execute('out.data.'+keys[i]+'=tmp.data.'+keys[i])
;        ENDIF
;    ENDFOR
      
;; Make line number 99 if all lines are equal (= rebinned spectrum)
      IF (max(out.data.line) EQ min(out.data.line)) THEN out.data.line = 99
      return,sh_calcaar(out,fl=-1,fact=1e4)
  ENDIF ELSE BEGIN
      print,'READ_FLWS: Fatal  (F): Could not open '+fname
      return,0
  ENDELSE
END
