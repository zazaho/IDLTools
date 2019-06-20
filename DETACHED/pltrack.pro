PRO pltrack,tracks,idx=idx,mdot=mdot,deltat=deltat,mgs=mgs,_extra=_extra, $
  c12=c12,c25=c25,times=times,verbose=verbose,number=number

  default,mgs,4
  default,mdot,1
  default,deltat,500
  
  IF NOT keyword_set(idx) THEN BEGIN
      i = where( (tracks.mdot EQ mdot) AND $
                   (tracks.deltat EQ deltat)  AND $
                   (tracks.mgs EQ mgs), count)
      IF count NE 0 THEN i = i[0] ELSE i=0
  ENDIF ELSE BEGIN
      i = idx
  ENDELSE

  IF keyword_set(verbose) THEN BEGIN
      print,'Mass loss rate =',tracks[i].mdot
      print,'Delta T        =',tracks[i].deltat
      print,'Fract MgS      =',tracks[i].mgs
  ENDIF 

  c12 = 2.5*alog10(tracks[i].iras25/tracks[i].iras12)
  c25 = 2.5*alog10(tracks[i].iras60/tracks[i].iras25)
  times = tracks[i].times
  
  pl,c12,c25,_extra=_extra
  
  IF keyword_set(number) THEN BEGIN
      xyouts,c12,c25+0.05,f2s(times,dec=0),noclip=0,charsize=0.7,align=0.5
  ENDIF

END
