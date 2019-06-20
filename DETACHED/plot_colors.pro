pro plot_colors,data,max_error=max_error,plot_err=plot_err, $
                plot_aver=plot_aver,newplot=newplot,_extra=_extra
  

  default,plot_err,0
  default,plot_aver,1
  default,newplot,0
  
  f12 = data.f12 
  f25 = data.f25 
  f60 = data.f60 
  f100= data.f100
  e12 = data.e12 
  e25 = data.e25 
  e60 = data.e60 
  e100= data.e100

  e12  = e12 <0.9
  e25  = e25 <0.9
  e60  = e60 <0.9
  e100 = e100<0.9

  ;; Now define the colors
  c12 = 2.5*alog10(f25/f12)
  c25 = 2.5*alog10(f60/f25)
  c60 = 2.5*alog10(f100/f60)
  
  ; the lower boud value of the color
  lc12 = 2.5*alog10((f25*(1.-e25))/(f12*(1.+e12)))
  lc25 = 2.5*alog10((f60*(1.-e60))/(f25*(1.+e25)))
  lc60 = 2.5*alog10((f100*(1.-e100))/(f60*(1.+e60)))

  ;; the upperbounds
  uc12 = 2.5*alog10((f25*(1.+e25))/(f12*(1.-e12)))
  uc25 = 2.5*alog10((f60*(1.+e60))/(f25*(1.-e25)))
  uc60 = 2.5*alog10((f100*(1.+e100))/(f60*(1.-e60)))

  IF keyword_set(max_error) THEN BEGIN
      good = where( (abs(uc12-lc12) LE max_error) AND (abs(uc25-lc25) LE max_error) )
      c12 = c12[good]
      c25 = c25[good]
      c60 = c60[good]

      lc12 = lc12[good]
      lc25 = lc25[good]
      lc60 = lc60[good]

      uc12 = uc12[good]
      uc25 = uc25[good]
      uc60 = uc60[good]
  ENDIF 

  IF newplot EQ 1 THEN BEGIN
      plot,c12,c25,_extra=_extra,/nodata
  ENDIF

  IF plot_err NE 0 THEN BEGIN
      grey = kleur('60grey')
      FOR i=0,n_elements(c12)-1 DO oplot,[c12[i],c12[i]],[lc25[i],uc25[i]],psym=0,_extra=_extra,color=grey,thick=1
      FOR i=0,n_elements(c12)-1 DO oplot,[lc12[i],uc12[i]],[c25[i],c25[i]],psym=0,_extra=_extra,color=grey,thick=1
  ENDIF 

  oplot,c12,c25,psym=5,_extra=_extra,symsize=0.5

  IF plot_aver EQ 1 THEN BEGIN
      elc12 = abs(c12-lc12)
      euc12 = abs(c12-uc12)
      elc25 = abs(c25-lc25)
      euc25 = abs(c25-uc25)
      avg_elc12 = mean(elc12)
      avg_euc12 = mean(euc12)
      avg_elc25 = mean(elc25)
      avg_euc25 = mean(euc25)
      
      foo_x = !x.crange[1]-1.5*avg_euc12
      foo_y = !y.crange[1]-1.5*avg_euc25
      
      perc = 1 - 1.5*( (avg_euc12/(!x.crange[1]-!x.crange[0]))>(avg_euc25/(!y.crange[1]-!y.crange[0])) )
      avg_x = (!x.crange[1]-!x.crange[0])*perc + !x.crange[0]
      avg_y = (!y.crange[1]-!y.crange[0])*perc + !y.crange[0]
      oplot,avg_x+[-avg_elc12,avg_euc12],[avg_y,avg_y],psym=0,_extra=_extra,thick=2
      oplot,[avg_x,avg_x],avg_y+[-avg_elc25,avg_euc25],psym=0,_extra=_extra,thick=2
  ENDIF 
END 
