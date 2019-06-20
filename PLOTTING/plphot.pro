PRO plphot,aar,ppsym=ppsym,pthick=pthick,perror=perror, $
           psym=psym,oplot=oplot,pcolor=pcolor,bands=bands,_extra=_extra

  default,ppsym,4
  default,pthick,1
  default,perror,0
  default,pcolor,!p.color
  default,oplot,0
  default,psym,10
  default,bands,0

  IF oplot EQ 0 then pl,aar,/nodata,_extra=_extra
  pl,aar,ab='phot',thick=pthick,error=perror,/oplot,psym=ppsym,ecolor=pcolor,_extra=_extra
  pl,aar,/nphot,/oplot,psym=psym,bands=bands,_extra=_extra
END
