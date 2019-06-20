PRO nsvscurves,table
;; Quantify the light curves. In the following steps:

  css = col_mr(table,'css')
  ncss = n_elements(css)
  
;; 1) For each source in the cataloge. Find the flies that hold the
;; data from the NSVS and/or NSVS. from mr table
  nsvsname = col_mr(table,'nsvsname')
  
  nsvspath = '../NSVS/curve/'
  nsvsprefix = 'curve'
  
  ;; keep track of the result of the fitting
  ;; status:
  ;; 0  = not yet/done not succesfull
  ;; -1 = file not found
  ;; 1  = successfully fitted
  ;; -666 = failed
  
  IF file_test('nsvsstatus.xdr',/read) THEN BEGIN
     restore,'nsvsstatus.xdr'
  ENDIF ELSE BEGIN
     nsvsstatus = make_array(ncss,value=0)
  ENDELSE
  
  FOR i=0,n_elements(css)-1 DO BEGIN

     IF nsvsstatus[i] EQ 0 THEN BEGIN
        hasnsvs = 0
        
;; 2) Read the data files
        IF (nsvsname[i] NE '') THEN BEGIN
           file = nsvspath+nsvsprefix+nsvsname[i]+'.txt'
           IF file_test(file+'.gz',/read) THEN BEGIN
              spawn,'gunzip '+file+'.gz'
              nsvs = read_ascii(file)
              spawn,'gzip -9 '+file
              nsvs = {times:reform((nsvs.(0))[0,*]), $
                      values:reform((nsvs.(0))[1,*]), $
                      errors:reform((nsvs.(0))[2,*]), $
                      units:['days','mag','mag'], $
                      name:nsvsname[i]}
              hasnsvs = 1
              IF n_elements(nsvs.times) LT 10 THEN BEGIN
                 message,'File has too few points for doing period search: '+file,/info
                 hasnsvs=0
              ENDIF
           ENDIF ELSE BEGIN
              message,'missing nsvs file: '+file,/info
              nsvsstatus[i] = -1
           ENDELSE
        ENDIF
        
        IF hasnsvs THEN BEGIN
           print,'Determine a period for: '+nsvsname[i]
           sh_period,nsvs,status=status,logfile='nsvs_periods.txt'
           CASE status OF
              'SUCCESS': nsvsstatus[i] = 1
              'FAILED':  nsvsstatus[i] = -666
              'STOP':  return
              ELSE: 
           ENDCASE
        ENDIF
        save,nsvsstatus,filename='nsvsstatus.xdr',/xdr
     ENDIF
  ENDFOR 
END
