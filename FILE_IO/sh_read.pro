;; wrapper around many functions for reading in files into aars
;; and also expanding .gz files

;; Look for file if it exists or maybe gzipped also
;; allow optional path(s)
FUNCTION sh_read_check_file,files,paths,status=status
  
  default,paths,''
  status='nofound'
  
  FOR j=0,n_elements(files)-1 DO BEGIN
     file=files[j]
     FOR i=0,n_elements(paths)-1 DO BEGIN
        path=paths[i]
;;        tmp = (findfile(path+file,count=count))[0]
        tmp = (file_search(path+file,count=count))[0]
        IF count NE 0 THEN BEGIN
           ;; This is needed for wildcards swer* or so
           IF strmid(tmp,strlen(tmp)-3,3) EQ '.gz' THEN BEGIN
              status='gzipped'
              ;; remove .gz
              foundfile = strmid(tmp,0,strlen(tmp)-3)
           ENDIF ELSE BEGIN
              status='simple'
              foundfile =tmp
           ENDELSE
        ENDIF ELSE BEGIN ;; no match for the file try with .gz
;;           tmp = (findfile(path+'/'+file+'.gz',count=count))[0]
           tmp = (file_search(path+'/'+file+'.gz',count=count))[0]
           IF count NE 0 THEN BEGIN
              status='gzipped'
              foundfile =strmid(tmp,0,strlen(tmp)-3)
           ENDIF
        ENDELSE
        IF status NE 'nofound' THEN return,foundfile
     ENDFOR
  ENDFOR
  return,''
END

;;Wrapper around gzip and gunzip
PRO gzip,arg
  spawn,'gzip '+arg
END
PRO gunzip,arg
  spawn,'gunzip '+arg
END

FUNCTION sh_read,filein,aar=aar,spd=spd,erd=erd,ssp=ssp,dat=dat, $
                 lws=lws,lsp=lsp,fits=fits, $
                 type=type,found=found,_extra=_extra
  
  default,type,''
  found=1
  
  if n_elements(filein) eq 0 then begin
     filein = dialog_pickfile(FILTER='*.fits',/MUST_EXIST)
  endif
  
  ;; take care of shell expansion
  file = expand_string(filein)

  IF keyword_set(aar) THEN type='aar'
  IF keyword_set(spd) THEN type='spd'
  IF keyword_set(erd) THEN type='erd'
  IF keyword_set(ssp) THEN type='ssp'
  IF keyword_set(dat) THEN type='dat'
  IF keyword_set(lws) THEN type='lws'
  IF keyword_set(lsp) THEN type='lsp'
  IF keyword_set(fits) THEN type='fits'
  
  IF type EQ '' THEN BEGIN
     ;; Now we wish to guess the type from the name
     tmp=strlowcase(file)
     CASE 1 OF 
        strmid(tmp,0,4) EQ 'swaa': type='aar'
        strmid(tmp,0,4) EQ 'swsp': type='spd'
        strmid(tmp,0,4) EQ 'swer': type='erd'
        strmid(tmp,0,3) EQ 'ssp' : type='ssp'
        strmid(tmp,0,3) EQ 'lsp' : type='lsp'
        strmid(tmp,0,4) EQ 'lsan': type='lws'
        strpos(tmp,'.dat') NE -1 : type='dat'
        strpos(tmp,'spd' ) NE -1 : type='spd'
        strpos(tmp,'aar' ) NE -1 : type='aar'
        strpos(tmp,'.tab') NE -1 : type='dat'
        ELSE:type='aar'
     ENDCASE
  ENDIF
  
  CASE type OF
     'aar': BEGIN
        tmp = sh_read_check_file(file,status=sts)
        CASE sts OF 
           'simple': out = read_faar(tmp)
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = read_faar(tmp)
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
     
     'lws': BEGIN
        tmp = sh_read_check_file(file,status=sts)
        CASE sts OF 
           'simple': out = read_flws(tmp)
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = read_flws(tmp)
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
     
     'spd': BEGIN
        tmp = sh_read_check_file(file,status=sts)
        CASE sts OF 
           'simple': out = read_fspd(tmp)
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = read_fspd(tmp)
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
     
     'ssp': BEGIN
        files=['','ssp']+file+['','.fits']
        tmp = sh_read_check_file(files, status=sts, $
                                 ['./',shell_expand('~/d1/ISO/STARTYPE/')])
        CASE sts OF 
           'simple': out = read_fspd(tmp,pa='')
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = read_fssp(tmp,pa='')
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
     
     
     'lsp': BEGIN
        files=['','lsp']+file+['','.fits']
        tmp = sh_read_check_file(files, status=sts, $
                                 ['./',shell_expand('~/d1/ISO/STARTYPE_LWS/')])
        CASE sts OF 
           'simple': out = read_flsp(tmp,pa='')
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = read_flsp(tmp,pa='')
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
     
     'dat': BEGIN
        tmp = sh_read_check_file(file, status=sts)
        CASE sts OF 
           'simple': out = read_fdat(tmp)
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = read_fdat(tmp)
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
     
     'fits': BEGIN
        tmp = sh_read_check_file(file, status=sts)
        CASE sts OF 
           'simple': out = readfits(tmp)
           'gzipped': BEGIN
              gunzip,tmp+'.gz'
              out = readfits(tmp)
              gzip,tmp
           END
           ELSE: BEGIN
              print,'SH_READ: file not found: '+file
              return,0
           END
        ENDCASE
     END
    
    'erd': BEGIN
       tmp = sh_read_check_file(file,['./','../original/'],status=sts)
       print,tmp,sts
       CASE sts OF 
          'simple': BEGIN
             p1 = strpos(tmp,'swer')+4
             p2 = strpos(tmp,'.fits')
             obs = strmid(tmp,p1,p2-p1)
             path= strmid(tmp,0,p1-4)
             out = read_ferd(obs,path=path)
          END
          'gzipped': BEGIN
             p1 = strpos(tmp,'swer')+4
             p2 = strpos(tmp,'.fits')
             obs = strmid(tmp,p1,p2-p1)
             path= strmid(tmp,0,p1-4)
             print,path
             gunzip,path+'/*'
             out = read_ferd(obs,path=path)
             gzip,path+'/*'
          END
          ELSE: BEGIN
             print,'SH_READ: file not found: '+file
             return,0
          END
       ENDCASE
    END
    
    ELSE: found=0
 ENDCASE
  
  return,out
END
