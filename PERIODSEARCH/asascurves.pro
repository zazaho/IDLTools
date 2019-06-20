PRO asascurves,table
;; Quantify the light curves. In the following steps:

  css = col_mr(table,'css')
  ncss = n_elements(css)
  
;; 1) For each source in the cataloge. Find the flies that hold the
;; data from the ASAS and/or NSVS. from mr table
  asasname = col_mr(table,'asasname')
  
  asaspath = '../ASAS/tables/'
  
  ;; keep track of the result of the fitting
  ;; status:
  ;; 0  = not yet/done not succesfull
  ;; -1 = file not found
  ;; 1  = successfully fitted
  ;; -666 = failed
  
  IF file_test('asasstatus.xdr',/read) THEN BEGIN
     restore,'asasstatus.xdr'
  ENDIF ELSE BEGIN
     asasstatus = make_array(ncss,value=0)
  ENDELSE
  
  FOR i=0,n_elements(css)-1 DO BEGIN

     IF asasstatus[i] EQ 0 THEN BEGIN
        hasasas = 0
        
;; 2) Read the data files
        IF (asasname[i] NE '') THEN BEGIN
           file = asaspath+asasname[i]+'.txt'
           IF file_test(file+'.gz',/read) THEN BEGIN
              spawn,'gunzip '+file+'.gz'
              asas = read_ascii(file)
              spawn,'gzip -9 '+file
              asas = {times:reform((asas.(0))[0,*]), $
                      values:reform((asas.(0))[5,*]), $
                      errors:reform((asas.(0))[10,*]), $
                      units:['days','mag','mag'], $
                      name:asasname[i]}
              hasasas = 1
           ENDIF ELSE BEGIN
              message,'missing asas file: '+file,/info
              asasstatus[i] = -1
           ENDELSE
        ENDIF
        
        IF hasasas THEN BEGIN
           print,'Determine a period for: '+asasname[i]
           sh_period,asas,status=status,logfile='asas_periods.txt'
           CASE status OF
              'SUCCESS': asasstatus[i] = 1
              'FAILED':  asasstatus[i] = -666
              'STOP':  return
              ELSE: 
           ENDCASE
        ENDIF
        save,asasstatus,filename='asasstatus.xdr',/xdr
     ENDIF
  ENDFOR 
END
