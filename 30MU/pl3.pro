PRO pl3,a1,a2,a3, $
        style2=style2,col2=col2,sym2=sym2,thi2=thi2, $
        style3=style3,col3=col3,sym3=sym3,thi3=thi3, $
        _extra=_extra
  default,style2,0
  default,col2,kleur('40grey')
  default,sym2,0
  default,thi2,3

  default,style3,0
  default,col3,kleur('black')
  default,sym3,4
  default,thi3,5
  pl,a1,_extra=_extra
  pl,a2,/opl,_extra=_extra,linestyle=style2,color=col2,psym=sym2,thick=thi2
  pl,a3,/opl,_extra=_extra,linestyle=style3,color=col3,psym=sym3,thick=thi3
END
