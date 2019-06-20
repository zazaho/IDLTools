pro sh_aardiff,old,new,name=name,filename=filename
  
  ;; figure out which band is given (fudge)
  IF NOT keyword_set(name) THEN BEGIN
    name = 'a'+(sh_whichband(old))(0)
  ENDIF
  
  ;; Use the detectors given in the old = dets 
  dets= sh_uniq(old.data.det)
  ndets = n_elements(dets)
  
  IF n_elements(filename) NE 0 THEN openw,lun,filename,/append,/get_lun ELSE lun = -1
  
  ;; itk0  print 
  itk0 = min(old.data.itk)
  ;; loop over detectors
  FOR i = 0,ndets-1 DO BEGIN
    ;; select the data points with given detector
    idxo = where(old.data.det EQ dets(i))
    idxn = where(new.data.det EQ dets(i),countold)
    IF countold EQ 0 THEN BEGIN
      diff = old.data(idxo).itk
    ENDIF ELSE BEGIN
      o = old.data(idxo).itk
      n = new.data(idxn).itk
    ;; diff holds values of itks in o but not in n
      diff = sh_setdifference(o,n)
    ENDELSE
    IF (diff(0) NE -1) THEN BEGIN
      ;; now we want these to print out in the following format:
      ;; a4 = sh_weg(a4,diff=[det,itk0,diff(0),diff(1),..,diff(19)])
      ;; a4 = sh_weg(a4,diff=[det,itk0,diff(20),diff(21),..,diff(39)])
      ;; etc
      ndiff = n_elements(diff)
      ;;Which means ndiff/20+1 lines. In which the lastline is maybe
      ;;not complete
      ;;First the complete lines
      FOR j=0,ndiff/20-1 DO BEGIN
        printf,lun,format='(A," = sh_weg(",A,",diff=[",I2,",",I11,$)', $
          name,name,dets(i),itk0
        printf,lun,format='(20(",",I6),$)',diff(j*20:(j+1)*20-1)-itk0
        printf,lun,'])'
      ENDFOR
      ;; Now the incomplete line
      IF (ndiff MOD 20) NE 0 THEN BEGIN
        printf,lun,format='(A," = sh_weg(",A,",diff=[",I2,",",I11,$)', $
          name,name,dets(i),itk0
        printf,lun,format='('+n2s(ndiff MOD 20)+'(",",I6),$)',diff(ndiff/20*20:*)-itk0
        printf,lun,'])'
      ENDIF
    ENDIF
  ENDFOR
  
  IF n_elements(filename) NE 0 THEN BEGIN
    close,lun
    free_lun,lun
  ENDIF
END
