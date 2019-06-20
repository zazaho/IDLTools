PRO sh_makeindex,lws=lws
  
  default,lws,0
  
  files = get_files(/num,lws=lws)
  
  IF lws THEN BEGIN
    pad = shell_expand('~/d1/ISO/STARTYPE_LWS/')
    prefix = "lsp"
  ENDIF ELSE BEGIN
    pad = shell_expand('~/d1/ISO/STARTYPE/')
    prefix = "ssp"
  ENDELSE
  
  rnd = n2s(floor(1e6*(randomu(xxx))))
  
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
    obj = sh_getkey(head,'OBJECT')+padding
    obs = sh_getkey(head,'OBSERVER')+padding
    ra  = sh_getkey(head,'RAATT')+padding
    dec = sh_getkey(head,'DECATT')+padding
    print,string(format='(A10,A20,A12,A14,A14)',files[i]+padding,obj,obs,ra,dec)
  ENDFOR
END
