FUNCTION read_flrs,fname_in,pad=pad,noprint=noprint,makehead=makehead, $
                   nocorrection=nocorrection
  fname=fname_in
;; Where to find the startype data
  default,pad,expand_string('$HOME/d1/ISO/LRS/')
  default,nocorrection,0
  
;Check to see if file can be found
;; First try just the name  
;;  foo = (findfile(pad+fname,count=count))[0]
  foo = (file_search(pad+fname,count=count))[0]
  IF count  EQ 0 THEN BEGIN
    fname = 'tdt'+fname+'_lrs.dat.gz'
;;Now try with a tdtNNNNNNN_lrs.dat.gz format
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
    spawn,'gunzip -vcq '+pad+fname+' >/tmp/temp'+rnd+'.dat'
    ;; now read the shit in
    lrs = read_fdat('/tmp/temp'+rnd+'.dat')
    ;; cleanup
    spawn,'rm /tmp/temp'+rnd+'.dat'	
  ENDIF ELSE BEGIN
    lrs = read_fdat(foo)
  ENDELSE
  
  lrs = sh_calcaar(lrs,flambda=-1)
  IF keyword_set(makehead) THEN BEGIN
      sws_database = read_fmr(shell_expand('$SWS_DATABASE'))
      tdt = strmid(foo,strpos(foo,'_lrs.dat')-8,8)
      entry = select_mr(sws_database,cons='tdt eq '+tdt)
      head = (col_mr(entry,'NAME')+','+col_mr(entry,'IRAS'))[0]
      lrs = object(lrs,set=head)
  ENDIF
  
  IF NOT nocorrection THEN BEGIN
      restore,shell_expand('$HOME/IA_FILES/lrs_correction_factors.xdr')
      lrs = div(lrs,lrs_correction_factors,/quiet)
  ENDIF

  return,lrs
END
