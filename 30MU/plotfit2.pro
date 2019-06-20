PRO plotfit2,a,c,lnk,temp,fact,xpos,ypos,obj=obj,opos=opos,nsize=nsize, $
            axis=axis,_extra=_extra
  

  IF n_elements(obj) EQ 0 THEN obj=object(a)
  default,opos,[38,1]
  
  !p.multi=[1,1,1]
  pl30,tit='',posi=[xpos,ypos,xpos+0.4,ypos+0.2],xtit='',ytit='',/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra,xsty=5,ysty=5
  pl,axis,[0,0],linestyle=0,ps=0,thi=2,/white,/opl
  xyouts,opos[0],opos[1],obj,align=1,charsize=nsize
END
