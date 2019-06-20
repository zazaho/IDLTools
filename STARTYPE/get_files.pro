FUNCTION get_files,num=num,pad=pad,lws=lws
  
;; to get the numbers of the LWS data  
  default,lws,0
  
;; Where to find the startype data
  IF lws THEN BEGIN
    default,pad,shell_expand('~/d1/ISO/STARTYPE_LWS')
    prefix = "lsp"
  ENDIF ELSE BEGIN
    default,pad,shell_expand('~/d1/ISO/STARTYPE/')
    prefix = "ssp"
  ENDELSE
  
  pushd,pad
  
  file = ''
  ;; Silly loop to get the files in in smaller arrays, otherwise
  ;; findfile doesn't work
  FOR i = 0,8 DO BEGIN
;;    tmp = findfile(prefix+strtrim(string(i),2)+"*.fits.gz")
    tmp = file_search(prefix+strtrim(string(i),2)+"*.fits.gz")
    file = [file,tmp]
  ENDFOR
  
  popd
  
  ;take the empty string off
  file = file[where(file NE '')]
  
  IF (keyword_set(num)) THEN BEGIN
    file = strmid(file,3,8)
  ENDIF
  
  return,file
END
