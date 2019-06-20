; to get all of them in
; t = get_files(/nums)
;for i=0,n_elements(t)-1 do foo = execute("r"+t(i)+"=read_fssp('"+t(i)+"')")
FUNCTION read_fpsp,fname,pad=pad,noprint=noprint
  
;; Where to find the startype data
  default,pad,'./'  
  
;Check to see if file can be found
;; First try just the name  
;;  foo = (findfile(pad+fname,count=count))[0]
  foo = (file_search(pad+fname,count=count))[0]
  IF count  EQ 0 THEN BEGIN
    fname = 'psp'+fname+'.fits.gz'
;;Now try with a ssp prefix    
;;    foo = (findfile(pad+fname,count=count))[0]
    foo = (file_search(pad+fname,count=count))[0]
    IF count  EQ 0 THEN BEGIN
      print,'Cannot find file'
      return,-1
    ENDIF
  ENDIF
  
  IF NOT keyword_set(noprint) THEN print,'reading: '+pad+fname
  
;; gunzip the file to a file with some random name
  gzipped = strmid(foo,strlen(foo)-3,3) EQ '.gz'
  IF gzipped THEN BEGIN
    rnd = n2s(floor(1e6*(randomu(xxx))))
    spawn,'gunzip -vcq '+pad+fname+' >/tmp/temp'+rnd+'.fits'
    ;; now read the shit in
    openr,ui,'/tmp/temp'+rnd+'.fits',/get_lun
  ENDIF ELSE BEGIN
    openr,ui,foo,/get_lun
  ENDELSE
  
;;two headers with each 36 lines of 80 characters
  head = bytarr(36*160*2)
  readu,ui,head
  
  hdr=strarr(36)
  xhdr=strarr(36)
  
  FOR j=0,35 DO BEGIN
    hdr[j]  = string(head[j*160:(j+1)*160-1])
    xhdr[j]  = string(head[j*160+36*160:(j+1)*160-1+36*160])
  ENDFOR
  nv=sxpar(xhdr,'naxis2')
  
  fs=fstat(ui)
        
  nv2=(fs.size-2l*2880)/160l
  IF nv2 LT nv THEN BEGIN
    print,fname+' not properly sized'
    print,'trying to recover what is left.'
    ; Integer part of lines left
    nv = floor(nv2)
  ENDIF

  datab=bytarr(160,nv)
  readu,ui,datab
  
  close,ui
  free_lun,ui
  IF gzipped THEN spawn,'rm /tmp/temp'+rnd+'.fits'	
  
  w1 = fltarr(nv)
  f1 = fltarr(nv)
  s1 = fltarr(nv)
  
  datab = strtrim(strcompress(datab),2)
  FOR j=0l,nv-1 DO BEGIN
;;    a1 = str_sep(datab[j],' ')
    a1 = strsplit(datab[j],' ',/extract)
    w1[j] = float(a1[0])
    f1[j] = float(a1[1])
    s1[j] = float(a1[2])
  ENDFOR
  
  head = string(head)

  aar = sh_define_aar(length=n_elements(w1))
  aar.header = head
  ;; Fix the lines from 8 until 12 as to adhere to the sws standard
  ;; numbers for the lines
  aar.data.line = 0
  aar.data.wave = w1
  aar.data.flux = f1*w1^2d0/3d14*1d26
  aar.data.stdev= s1*w1^2d0/3d14*1d26
  aar.data.sdir = 0
  
  return,aar
END
