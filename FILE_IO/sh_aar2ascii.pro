;S Hony
; Read/Write aar fluxes to/from ascii-file 
;(SH May  4 1999) added error to table

PRO sh_aar2ascii,p1,p2,back=back,wave=wave,flux=flux,help=help,head=head, $
                 format=format,maxi=maxi,error=errs

;initialise
  object = ''
;(SH Feb 22 1999)
; Maximum number of points to read  
  default,maxi,20000L
  
;no parameters or help requested
  IF ((n_params() EQ 0) OR keyword_set(help)) THEN BEGIN
    print,'procedure sh_aar2ascii:'
    print,'usage:'
    print,'sh_aar2ascii,[aar,]filename[,wave=wave,flux=flux,/help,/back', $
      ',/head,format=format]'
    print,'aar: optional structure that holds wave,flux values'
    print,'filename: name of ascii file to read/write from/to'
    print,'wave,flux arrays that hold wave,flux values to be written'
    print,'/help provides this help'
    print,'/back reads from file into aar (aar must be given)'
    print,'/head also write sm header (object,date,time)'
    print,'/error also write sm header (object,date,time)'
    print,'format=format specifies format for writing'
    print,'maxi=maxi specifies then maximum number of points to read'
    return
  ENDIF
  
  IF keyword_set(errs) THEN BEGIN
    doerrs = 1
  ENDIF ELSE BEGIN
    doerrs = 0
  ENDELSE

  IF NOT keyword_set(format) THEN BEGIN
    IF doerrs THEN format = '(3f10.4)' ELSE format = '(2f10.4)' 
  ENDIF
  
;read instead of write
  IF keyword_set(back) THEN BEGIN
    
;make sure we have both needed parameters
    IF (n_params() EQ 2) THEN BEGIN 
;;      foo = findfile(p2,count=count)
      foo = file_search(p2,count=count)

;does the file to read from exist  ?
      IF (count EQ 0) THEN BEGIN
        print,'the file ',p2,' does not exist'
        return
      ENDIF ELSE BEGIN

;initialise
        teller = 0L
        a= dindgen(maxi)
        b= dindgen(maxi)
        c= dindgen(maxi) 
        w = 0d0
        f = 0d0
        e = 0d0

;now read
        openr,lun,p2,/get_lun
        IF (n_elements(head) GT 0) THEN BEGIN
          IF (head EQ 1) THEN head = 4
          foo = ''
          FOR i =1,head DO BEGIN
            readf,lun,foo
          ENDFOR
        ENDIF
        
        WHILE NOT (EOF(lun) OR (teller GE maxi))  DO BEGIN
          IF (doerrs) THEN BEGIN
            readf,lun,w,f,e
          ENDIF ELSE BEGIN
            readf,lun,w,f
            e = 0d0
          ENDELSE
          a(teller) = w
          b(teller) = f
          c(teller) = e
          teller = teller + 1L
        ENDWHILE
        close,lun
        free_lun,lun

;store in an aar
        p1 = define_aar(length=teller)
        p1.data.wave = a(0L:teller-1L)
        p1.data.flux = b(0L:teller-1L)
        p1.data.stdev = c(0L:teller-1L)
      ENDELSE
    ENDIF ELSE BEGIN
      print,'please supply both aar and filename'
      return
    ENDELSE  

;write aar
  ENDIF ELSE BEGIN

;only one parameter?
    IF (n_params() EQ 1) THEN BEGIN

;but supplied wave and flux
      IF keyword_set(wave) AND keyword_set(flux) THEN BEGIN 
        p2 = p1
      ENDIF ELSE BEGIN
        print,'please supply aar,filename or filename,wave=wave,flux=flux'
        return
      ENDELSE
    ENDIF ELSE BEGIN

;check for valid aar
      IF is_aar(p1) THEN BEGIN
        wave = p1.data.wave
        flux = p1.data.flux
        stdv = p1.data.stdev
        status = read_fits_key( p1.header, 'OBJECT', object )
      ENDIF ELSE BEGIN
        print,'Please supply valid aar'
        return
      ENDELSE
    ENDELSE

;now write ascii table
    openw,lun,p2,/get_lun
    n = n_elements(wave)
    IF keyword_set(head) THEN BEGIN
      printf,lun,'# object: ',object
      printf,lun,'# time: ',systime()
      printf,lun,'# number of values: ',n
      IF doerrs THEN $
        printf,lun,'#     wave      flux     error' $
      ELSE   printf,lun,'#     wave      flux'
      
    ENDIF
    FOR i = 0,n-1 DO BEGIN
      IF doerrs THEN BEGIN
        printf,lun,wave[i],flux[i],stdv[i],format=format
      ENDIF ELSE BEGIN
        printf,lun,wave[i],flux[i],format=format
      ENDELSE
    ENDFOR
    close,lun
    free_lun,lun
  ENDELSE

END
