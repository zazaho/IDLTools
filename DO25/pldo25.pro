PRO pldo25,in,modust=modust,_extra=_extra
  restore,'/home/sacha/IA_FILES/do25.xdr'
  IF keyword_set(modust) THEN BEGIN
     pl,in,/ll,ps=0,xrange=[1,200],yrange=[1,1d4],_extra=_extra,/nodata
     pl,do25,/opl,ps=4
     pl,in,/opl,ps=0,thick=3
  ENDIF ELSE begin
     pl,in,/iras,/xlog,ps=0,_extra=_extra
     plphot,do25,/opl,_extra=_extra
  ENDELSE
END
