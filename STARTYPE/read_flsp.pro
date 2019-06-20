; to get all of them in
; t = get_files(/nums)
;for i=0,n_elements(t)-1 do foo = execute("r"+t(i)+"=read_flsp('"+t(i)+"')")
FUNCTION read_flsp,fname,pad=pad,noprint=noprint,xlog=xlog
  
;; Where to find the startype data
  default,pad,shell_expand('$HOME/d1/ISO/STARTYPE_LWS/')
  
;Check to see if file can be found
;; First try just the name  
;;  foo = (findfile(pad+fname,count=count))[0]
  foo = (file_search(pad+fname,count=count))[0]
  IF count  EQ 0 THEN BEGIN
    fname = 'lsp'+fname+'.fits.gz'
;;Now try with a lsp prefix    
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
  
  fs=fstat(ui)
  
;;two headers with each 36 lines of 80 characters
  head = bytarr(36*80*2)
  readu,ui,head
  
;; Let's skip the rastered observations for now
;; So read everything in and see if the RASTRA keyword is present in
;; the header
  rastered = strpos(head,'RASTRA') NE -1
  IF rastered THEN BEGIN
    print,'WARNING: Rastered observation. Skipping file'
    close,ui
    free_lun,ui
    spawn,'rm '+ftarget
    dummy = sh_define_aar(length=33)
    dummy.data.wave = ([1,1,2,2, $
                       2.5,2.5,3.5,3.5,2.5, $
                       4.5,5.5,5.5,4.5,4.5, $
                       6,6.5,7,6.75,6.25, $
                       7.1,8.9,8,8, $
                       9.1,9.6,10.1,9.85,9.35, $
                      0,0,11.1,11.1,0])+1
    dummy.data.flux = [0,2,0,2, $
                       0,2,2,0,0, $
                       2,1.8,0.2,0,2, $
                       0,2,0,1,1, $
                       2,2,2,0, $
                       0,2,0,1,1, $
                      -1,3,3,-1,-1]+2
    dummy.data.det =  [1,1,1,1, $
                       2,2,2,2,2, $
                       3,3,3,3,3, $
                       4,4,4,4,4, $
                       5,5,5,5, $
                       6,6,6,6,6, $
                       7,7,7,7,7]
    IF keyword_set(xlog) THEN dummy.data.wave = 10^dummy.data.wave
    dummy.header = string(head)
    return,dummy
  ENDIF
  
  hdr=strarr(36)
  xhdr=strarr(36)
  
  FOR j=0,35 DO BEGIN
    hdr[j]  = string(head[j*80:(j+1)*80-1])
    xhdr[j]  = string(head[j*80+36*80:(j+1)*80-1+36*80])
  ENDFOR
  nv=sxpar(xhdr,'naxis2')
  
  nv2=(fs.size-2l*2880)/80.0
  IF nv2 LT nv THEN BEGIN
    print,fname+' not properly sized'
    print,'trying to recover what is left.'
    ; Integer part of lines left
    nv = floor(nv2)
  ENDIF
  
  datab=bytarr(80,nv)
  readu,ui,datab
  
  close,ui
  free_lun,ui
  IF gzipped THEN spawn,'rm /tmp/temp'+rnd+'.fits'	
  
  w1 = fltarr(nv)
  f1 = fltarr(nv)
  s1 = fltarr(nv)
  d1 = intarr(nv)
  l1 = intarr(nv)
  ;; Active flag is read in too
  act= intarr(nv)
  
;;  data=fltarr(4,nv)
  datab = strtrim(strcompress(datab),2)
  FOR j=0l,nv-1 DO BEGIN
;;    a1 = str_sep(datab[j],' ')
    a1 = strsplit(datab[j],' ',/extract)
    w1[j] = float(a1[0])
    f1[j] = float(a1[1])
    s1[j] = float(a1[2])
    d1[j] = fix(a1[3])
    l1[j] = fix(a1[4])
    act[j]= fix(a1[6])
  ENDFOR
  
;;  nu nog even op schonen als nodig is door alle niet actieve punten
;;  er uit te gooien. Die zijn act <> 1
  
  IF total(act eq 1) ne nv THEN BEGIN
    idx = where(act EQ 1)
    w1 = w1[idx]
    f1 = f1[idx]
    s1 = s1[idx]
    d1 = d1[idx]
    l1 = l1[idx]
  ENDIF
  
  head = string(head)

  aar = sh_define_aar(length=n_elements(l1))
  aar.header = head
  aar.data.wave = w1
  ;; De flux eenheden zijn in Watt/cm^2/um dus omzetten naar f_nu in
  ;; Jy levert *l^2/c*1e26*1d4
  aar.data.flux = f1*w1^2*1d26/3d14*1d4
  aar.data.stdev= s1*w1^2*1d26/3d14*1d4
  ;; what we call lines, lws call dets. Watch out
  aar.data.det  = d1
  aar.data.line = l1
  aar.data.sdir = 0
  
  return,aar
END
