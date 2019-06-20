PRO plot_1a_ellips,ra,dec,err1,err2,PA,_extra=_extra
  theta = dindgen(101)*1d-2*2d0*!dpi
  PA_radian = PA*2d0*!dpi/360d0
  
  dec_ell = err1*sin(theta)
  ra_ell = err2*cos(theta)
  
  ;; now rotate the thing with PA
  
  rot_ra_ell = ra_ell*cos(PA_radian) - dec_ell*sin(PA_radian)
  rot_dec_ell = ra_ell*sin(PA_radian) + dec_ell*cos(PA_radian)

  oplot,ra[0]+rot_ra_ell/cos(dec[0]*!dpi/180.),dec[0]+rot_dec_ell,ps=0,_extra=_extra
  oplot,ra[1]+rot_ra_ell/cos(dec[1]*!dpi/180.),dec[1]+rot_dec_ell,ps=0,_extra=_extra
END

FUNCTION s2deg,str
  IF (strpos(str,'"') NE -1) THEN unit = 1.0/3600. ELSE unit = 1./60.
  return,double(str)*unit
END

PRO plot_1ast,p,line=line,ellips=ellips,name=name,_extra=_extra
  
  default,line,1
  default,ellips,1
  default,name,0

  nm   = p[0]
  ra   = double(p[[1,3]])
  dec  = double(p[[2,4]])
  err1 = s2deg(p[5])
  err2 = s2deg(p[6])
  PA   = double(p[7])
  F_min = double(p[8])
  F_max = double(p[9])
  
  IF line   THEN oplot,ra,dec,ps=0,_extra=_extra
  IF ellips THEN plot_1a_ellips,ra,dec,err1,err2,PA,_extra=_extra

END 

PRO plot_asteroid,all=all,f1_lw2=f1_lw2,f1_lw3=f1_lw3, $
          f2_lw2=f2_lw2,f2_lw3=f2_lw3,_extra=_extra


  default,all,0
  default,f1_lw2,0
  default,f1_lw3,0
  default,f2_lw2,0
  default,f2_lw3,0


  IF keyword_set(all) THEN BEGIN
      f1_lw2 = 1
      f1_lw3 = 1
      f2_lw2 = 1
      f2_lw3 = 1
  ENDIF 

  IF f1_lw2 NE 0 THEN BEGIN
      plot_1ast,_extra=_extra,['138334 2001 QU216','68.24170','24.58597','68.25296','24.59053','  5   ','  5   ','0    ','0.36',' 2.87'] 
      plot_1ast,_extra=_extra,['    003810 Aoraki','68.20609','24.58074','68.22311','24.58209','0.090"','0.068"',' 89.6','6.86','54.85'] 
      plot_1ast,_extra=_extra,[' 017092 1999 JP21','68.27850','24.56808','68.29654','24.57626','0.109"','0.091"',' 70.5','1.75','14.02'] 
      plot_1ast,_extra=_extra,[' 129850 2002 VU18','68.31845','24.59274','68.32890','24.59862','2.248 ','0.052 ',' 71.5','0.17',' 1.34'] 
      plot_1ast,_extra=_extra,['144594 2001 SP128','68.33994','24.60770','68.35521','24.61079','2.807 ','0.009 ',' 84.6','0.36',' 2.86'] 
      plot_1ast,_extra=_extra,['145756 2002 AN154','68.32511','24.99427','68.34675','25.00166','  5   ','  5   ','0    ','0.57',' 4.59'] 
      plot_1ast,_extra=_extra,[' 015930 1997 WT37','68.21215','24.40924','68.22838','24.41429','0.084"','0.074"',' 80.2','1.94','15.52'] 
  ENDIF

  IF f1_lw3 NE 0 THEN BEGIN
      plot_1ast,_extra=_extra,['143048 2000 SO272','68.28923','24.64593','68.29787','24.65081','18.127"','0.237"','80.8',' 2.35',' 18.77']
      plot_1ast,_extra=_extra,[' 168006 2001 SA10','68.15327','24.45957','68.15693','24.46624','11.999"','0.168"','72.4',' 5.53',' 44.25']
      plot_1ast,_extra=_extra,[' 121844 1997 WX35','68.31028','24.47115','68.32683','24.47727',' 0.196"','0.191"','72.4',' 5.00',' 39.97']
      plot_1ast,_extra=_extra,['   176632 2001 VK','68.19273','24.44219','68.20017','24.45393','    5  ',' 5    ',' 0  ','11.87',' 94.92']
      plot_1ast,_extra=_extra,[' 116039 1999 GD16','68.36581','24.45049','68.37222','24.45582','14.778"','0.149"','77.4',' 2.10',' 16.80']
      plot_1ast,_extra=_extra,[' 136139 2001 TD94','68.13050','25.03912','68.14085','25.04331','57.246"','0.233"','82.5',' 2.38',' 19.02']
  ENDIF

  IF f2_lw2 NE 0 THEN BEGIN
      plot_1ast,_extra=_extra,[' 108755 2001 SO36','68.19743','24.13439','68.20239','24.13914','10.393"','0.242"','78.0',' 0.28','  2.27']
      plot_1ast,_extra=_extra,[' 102783 2001 TN70','68.14183','24.35911','68.14629','24.36329','39.796"','0.176"','79.3',' 0.18','  1.44']
      plot_1ast,_extra=_extra,[' 137866 2001 TR58','68.16883','24.13864','68.17355','24.14114','    5  ',' 5    ',' 0  ',' 0.47','  3.74']
      plot_1ast,_extra=_extra,[' 187421 2002 CM61','68.44096','24.12239','68.45783','24.13472','    5  ',' 5    ',' 0  ',' 2.39',' 19.14']
  ENDIF

  IF f2_lw3 NE 0 THEN BEGIN
      plot_1ast,_extra=_extra,[' 108755 2001 SO36','68.20242','24.13917','68.20599','24.14384','10.393"','0.242"','78.0',' 3.51',' 28.08']
      plot_1ast,_extra=_extra,[' 102783 2001 TN70','68.14632','24.36332','68.14944','24.36742','39.796"','0.176"','79.3',' 2.36',' 18.90']
      plot_1ast,_extra=_extra,[' 137866 2001 TR58','68.17358','24.14115','68.17695','24.14357','    5  ',' 5    ',' 0  ',' 6.06',' 48.51']
      plot_1ast,_extra=_extra,[' 187421 2002 CM61','68.45793','24.13480','68.47261','24.14698','    5  ',' 5    ',' 0  ','15.43','123.45']
  ENDIF
END
