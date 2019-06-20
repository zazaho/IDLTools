PRO plotfit,a,c,lnk,temp,fact,num=num,obj=obj,opos=opos,nsize=nsize, $
            _extra=_extra
  

  IF n_elements(obj) EQ 0 THEN obj=object(a)
  default,opos,[.1,.02]

  CASE num OF
      1: BEGIN
          pl30,tit='',posi=[0.1,0.7,0.5,0.9],xtit='',ytit='',xtickformat='(A1)',/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          xyouts,opos[0]+0.1,opos[1]+0.7,obj,align=0,/normal,charsize=nsize
      END

      2: BEGIN
          pl30,tit='',posi=[0.5,0.7,0.9,0.9],xtit='',xtickformat='(A1)',ytit='',ytickformat='(A1)',yticks=1,/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          axis,yaxis=1,ystyle=1,_extra=_extra
          xyouts,opos[0]+0.5,opos[1]+0.7,obj,align=0,/normal,charsize=nsize
      END

      3: BEGIN
          pl30,tit='',posi=[0.1,0.5,0.5,0.7],xtit='',ytit='',xtickformat='(A1)',/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          xyouts,opos[0]+0.1,opos[1]+0.5,obj,align=0,/normal,charsize=nsize
      END

      4: BEGIN
          pl30,tit='',posi=[0.5,0.5,0.9,0.7],xtit='',xtickformat='(A1)',ytit='',ytickformat='(A1)',yticks=1,/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          axis,yaxis=1,ystyle=1,_extra=_extra
          xyouts,opos[0]+0.5,opos[1]+0.5,obj,align=0,/normal,charsize=nsize
      END

      5: BEGIN
          pl30,tit='',posi=[0.1,0.3,0.5,0.5],xtit='',ytit='',xtickformat='(A1)',/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          xyouts,opos[0]+0.1,opos[1]+0.3,obj,align=0,/normal,charsize=nsize
      END

      6: BEGIN
          pl30,tit='',posi=[0.5,0.3,0.9,0.5],xtit='',xtickformat='(A1)',ytit='',ytickformat='(A1)',yticks=1,/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          axis,yaxis=1,ystyle=1,_extra=_extra
          xyouts,opos[0]+0.5,opos[1]+0.3,obj,align=0,/normal,charsize=nsize
      END

      7: BEGIN
          pl30,tit='',posi=[0.1,0.1,0.5,0.3],xtit='',ytit='',/autoy,ymin=0,thick=3,/white,/cbands,a,c,lnk,temp,fact,_extra=_extra
          xyouts,opos[0]+0.1,opos[1]+0.1,obj,align=0,/normal,charsize=nsize
      END

      ELSE: BEGIN
          pl30,tit='',posi=[0.5,0.1,0.9,0.3],xtit='',ytit='',/autoy,ymin=0,thick=3,/white,/cbands,ytit='',ytickformat='(A1)',yticks=1,a,c,lnk,temp,fact,_extra=_extra
          axis,yaxis=1,ystyle=1,_extra=_extra
          xyouts,opos[0]+0.5,opos[1]+0.1,obj,align=0,/normal,charsize=nsize
      END
  END
END
