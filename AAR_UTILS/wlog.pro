;+
; NAME: wlog.pro
;
; PURPOSE: Writes a line of info from observation, taken from header
;
; CALLING SEQUENCE: wlog,aar[,out=filename]
;
; INPUTS: structure with header
;
; KEYWORD PARAMETERS:format=format
;
; SIDE EFFECTS:writes a line of info to the screen
;
; RESTRICTIONS: expects certain header keywords to be defined
;
; EXAMPLE: wlog,aar
;
; MODIFICATION HISTORY:
;(SH Feb  8 2000) added obsprogram to output
;-

; make a nice looking string for RA
FUNCTION frmt_ra,ra
  ; In degrees
  r = double(ra)
  h = floor(r/15d0)
  ; convert the rest to arcmin
  r = (r*4d0-h*60d0)
  m = floor(r)
  ; convert the rest to arcsecs
  s = (r-m*1d0)*60d0
  IF (s LT 9.995d0) THEN BEGIN
    s = '0'+string(format='(F4.2)',s)
  ENDIF ELSE BEGIN
    s = string(format='(F5.2)',s)
  ENDELSE
  
  return,string(format='(I2.2,"h",I2.2,"m",A,"s")',h,m,s)
END

; make a nice looking string for DEC
FUNCTION frmt_dec,dec
  
  d = double(dec)
  
  ;;First take the abs but keep the sign
  positive = (d GE 0)
  d = abs(d)
  
  ;; In degrees
  deg = floor(d)
  ; convert the rest to arcmin
  d = (d-deg*1d0)*60d0
  m = floor(d)
  ; convert the rest to arcsecs
  s = (d-m*1d0)*60d0
  IF positive THEN BEGIN
    deg = '+'+string(format='(I2.2)',deg)
  ENDIF ELSE BEGIN
    deg = '-'+string(format='(I2.2)',deg)
  ENDELSE
  
  IF (s LT 9.995d0) THEN BEGIN
    s = '0'+string(format='(F4.2)',s)
  ENDIF ELSE BEGIN
    s = string(format='(F5.2)',s)
  ENDELSE
  
  return,string(format='(A,"d",I2.2,"m",A,"s")',deg,m,s)
END

; procedure to write line of log-file 
PRO wlog,aar,format=format,out=f_out
  
  ;Check for valid input
  IF (NOT is_aar(aar)) THEN BEGIN
    print,'please supply a valid aar structure'
    return
  ENDIF
  
  IF (n_elements(format) EQ 0) THEN BEGIN
    format='(A15,3A12,I4,2A15,F7.2)'
  ENDIF
  
  head = aar.header
  
;;; Now extract the info we want:
;;; Object,OBSERVER,POSITION,DATE,REVOLUTION,AOT TYPE(and SPEED)
  
  keys = ['OBJECT','OBSERVER','EOHAUTCS','EOHAAOTN','EOHAPLID','ATTRA','ATTDEC']
  
  FOR i=0,n_elements(keys)-1 DO BEGIN
    foo = execute('s=read_fits_key(head,keys(i),'+keys(i)+')')
  ENDFOR
  
  obj =  strtrim(strupcase (object),2)
  obs =  strtrim(strlowcase(observer),2)
  aot =  strtrim(strupcase (eohaaotn),2)
  
  CASE (strmid(aot,0,1)) OF
    'S': inst = 'SWS '
    'L': inst = 'LWS '
    ELSE: inst = strmid(aot,0,1)
  ENDCASE
  
  mde = strmid(aot,1,strlen(aot)-1)
  
  speed = '   '
  IF (aot EQ 'S01') THEN BEGIN
    speed = '('+strtrim(string(get_aot_speed(aar)),2)+')'
  ENDIF
  
  aot = inst+mde+speed
  
  dte  = utc2date(aar,juld=jd)
  rev= jd2rev(jd)
  
  ra = frmt_ra(ATTRA)
  
  dec = frmt_dec(ATTDEC)
  
  IF n_elements(f_out) NE 0 THEN BEGIN
    openw,olun,f_out,/get_lun,/append
    printf,olun,string(form=format,obj,obs,aot,dte,rev,ra,dec,jd-2450000)
    close,olun
    free_lun,olun
  ENDIF ELSE BEGIN 
    print,string(form=format,obj,obs,aot,dte,rev,ra,dec,jd-2450000)
  ENDELSE
END
