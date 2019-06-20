PRO pl30,aar,cont,lnk,temp,fact,_extra=_extra
  grey = kleur('40g')
  default,temp,0
  default,fact,0

  pl,aar,_extra=_extra,title='+object; T!Dmgs!N='+n2s(temp)
  IF n_elements(cont) NE 0 THEN BEGIN
      pl,cont,/opl,_extra=_extra,linestyle=2
      IF n_elements(lnk) NE 0 THEN BEGIN
          pl,add(cont,sim_spec(lnk,/cde,temp=temp,fact=fact,_extra=_extra)),/oplot,_extra=_extra
          pl,subtract(aar,sim_spec(lnk,/cde,temp=temp,fact=fact,_extra=_extra)),/oplot,_extra=_extra,color=grey
      ENDIF
  ENDIF
END 
