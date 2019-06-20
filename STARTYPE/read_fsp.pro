FUNCTION read_fsp,fname,lws=lws,_extra=_extra
;; wrapper around reading different survey products
;; By default read the ssp (SWS survey product)
  IF keyword_set(lws) THEN BEGIN
    return,read_flsp(fname,_extra=_extra)
  ENDIF ELSE BEGIN
    return,read_fssp(fname,_extra=_extra)
  ENDELSE
END
