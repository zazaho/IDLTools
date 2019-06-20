PRO show_apers,ima,aar,center,size,_extra=_extra
  default,size,5*60d0
  tvscale,ima,_extra=_extra
  oplot_aper,aar,'1a',center,size,_extra=_extra
  oplot_aper,aar,'3a',center,size,_extra=_extra
  oplot_aper,aar,'3e',center,size,_extra=_extra
  oplot_aper,aar,'4' ,center,size,_extra=_extra
END
