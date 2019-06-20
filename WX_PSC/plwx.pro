PRO plwx,in,modust=modust,_extra=_extra
  restore,'/home/sacha/IA_FILES/wx_psc.xdr'
  IF keyword_set(modust) THEN BEGIN
      pl,in,/ll,ps=0,xrange=10d0^[-1d0,4d0],yrange=10d0^[-7d0,4d0], $
         ymin=1d-7,_extra=_extra,/nodata
      pl,wx_psc,/opl,ps=4
      pl,in,/opl,ps=0,thick=3
  ENDIF ELSE begin
      pl,in,/iras,/xlog,ps=0,_extra=_extra
      plphot,wx_psc,/opl,_extra=_extra
  ENDELSE
END
