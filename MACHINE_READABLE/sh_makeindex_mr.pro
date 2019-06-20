PRO sh_makeindex_mr,lws=lws,lun=lun
  
  default,lws,0
  default,lun,-1

  files = get_files(/num,lws=lws)

  IF lws THEN BEGIN
    pad = shell_expand('~/d1/ISO/STARTYPE_LWS/')
    prefix = "lsp"
  ENDIF ELSE BEGIN
    pad = shell_expand('~/d1/ISO/STARTYPE/')
    prefix = "ssp"
  ENDELSE
  
  month_in = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  month_out = ['01','02','03','04','05','06','07','08','09','10','11','12']

  rnd = n2s(floor(1e6*(randomu(xxx))))
  
;; the machine-readable header:
  printf,lun,'Title: Log of ISO observations'
  printf,lun,'Author: Sacha Hony '
  printf,lun,'Table: ISO observation bookkeeping data'
  printf,lun,'================================================================================'
  printf,lun,'Byte-by-byte Description of table'
  printf,lun,'--------------------------------------------------------------------------------'
  printf,lun,'   Bytes Format Units  Label   Explanations'
  printf,lun,'--------------------------------------------------------------------------------'
  printf,lun,'   1- 19 A19    ---     Name    Name of target given by observer'
  printf,lun,'  21- 28 I8     ---     TDT     The TDT of the observation'
  printf,lun,'  31- 33 A3     ---     Inst    Name of the instrument'
  printf,lun,'  36- 37 I2     ---     AOT     AOT number'
  printf,lun,'  39     I1     ---     Speed   AOT speed'
  printf,lun,'  41- 49 F9.5   Deg     Ra      Right ascension in degrees'
  printf,lun,'  51- 59 F9.5   Deg     Dec     Declination in degrees'
  printf,lun,'  62- 63 I2     h       Ra_h    The hour of RA'
  printf,lun,'  65- 66 I2     m       Ra_m    The minutes of RA'
  printf,lun,'  68- 72 F5.2   s       Ra_s    The seconds of RA'
  printf,lun,'  75- 77 I3     d       Dec_d   The hour of DEC  '
  printf,lun,'  79- 80 I2     m       Dec_m   The minutes of DEC'
  printf,lun,'  82- 86 F5.2   s       Dec_s   The seconds of DEC'
  printf,lun,'  89- 92 I4     ---     Year    The year of observation'
  printf,lun,'  94- 95 I2     ---     Month   The month of observation'
  printf,lun,'  97- 98 I2     ---     Day     The day of observation'
  printf,lun,' 100-101 I2     h--     Hour    The hour of observation'
  printf,lun,' 103-104 I2     m--     Min     The minutes of observation'
  printf,lun,' 106-107 I2     s--     Sec     The second of observation'
  printf,lun,' 109-120 A12    ---     Observ  The name of the observer'
  printf,lun,' 122-136 A15    ---     OName   Alternative name (from Simbad)'
  printf,lun,'--------------------------------------------------------------------------------'
  printf,lun,'Name                TDT       Inst AO S RA(deg)    Dec(Deg)  Ra(sex)      Dec(sex)      Date       Time     Observer      OName         '
  printf,lun,'----------------------------------------------------------------------------------------------------------------------------------'
    
  FOR i=0,n_elements(files)-1 DO BEGIN
      
      fname = prefix+files[i]+'.fits.gz'
      
      spawn,'gunzip -vcq '+pad+fname+' >/tmp/temp'+rnd+'.fits'	
      
;; now read the shit in
      openr,ui,'/tmp/temp'+rnd+'.fits',/get_lun
      
;;two headers with each 36 lines of 80 characters
;;But we only need the first 36 lines
      head = bytarr(36*80)
      readu,ui,head
      
      close,ui
      free_lun,ui
      spawn,'rm /tmp/temp'+rnd+'.fits'	
      
      head = string(head)
      
      padding = '                                          '
      inst= sh_getkey(head,'INSTRUME')+padding
      aot = strmid(sh_getkey(head,'EOHAAOTN'),1,2,/reverse)
      IF NOT lws THEN BEGIN
          speed=strmid(strtrim(sh_getkey(head,'SPEED'),2),0,1,/reverse)
      ENDIF ELSE BEGIN
          speed = '0'
      ENDELSE
      obj = sh_getkey(head,'OBJECT')+padding
      obs = sh_getkey(head,'OBSERVER')+padding
      date_s= strtrim(sh_getkey(head,'DATE-OBS'),2)
      ;; we have two kinds of formats:
      ;; 1996-07-01T10:30:01 and
      ;; 24-FEB-96 11:50:19
      ;; 012345678901234567
      IF strmid(date_s,10,1) EQ 'T' THEN BEGIN
          date=strmid(date_s,0,10)
          time=strmid(date_s,11,8)
      ENDIF ELSE BEGIN
          date = strmid(date_s,0,9)
          date_parts = strsplit(date,'-',/extract)
          date = '19'+date_parts[2]+'-'+ $
                 month_out[where(month_in EQ date_parts[1])]+'-'+ $
                 date_parts[0]
          time=strmid(date_s,10,8)
      ENDELSE
      ra  = sh_getkey(head,'ATTRA')
      dec = sh_getkey(head,'ATTDEC')
      ra_f  =float(ra)
      dec_f =float(dec)
      ra_s = ra2s(ra_f,/full)
      dec_s = dec2s(dec_f,/full)
      ra=string(ra_f,format='(F10.5)')
      dec=string(dec_f,format='(F10.5)')
      
      printf,lun, $
             string(format='(A20,A10,A4,A3,A2,A10,A10,A14,A14,A11,A9," ",A12)', $
                    obj,files[i]+padding, $
                    inst,aot,speed, $
                    ra,dec,ra_s,dec_s, $
                    date,time,obs)
  ENDFOR
END
