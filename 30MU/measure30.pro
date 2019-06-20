function measure30,in,cont,zoom=zoom,_extra=_extra
  
  smooth_in = cutlines(in)
  
  IF n_elements(cont) NE 0 THEN BEGIN
      f = sh_select_range(offset(div(sh_cut(in),cont,/qui),-1d0,/qui),xr=[15,50],/quiet)
      smooth_f = sh_select_range(offset(div(sh_cut(smooth_in),cont,/qui),-1d0,/qui),xr=[15,50],/quiet)
  ENDIF ELSE BEGIN 
      f = in
      smooth_f = in
  ENDELSE 

  IF keyword_set(zoom) THEN BEGIN 
      pl,in,/auto,_extra=_extra
      pl,cont,/oplot,_extra=_extra
      
      message,/information,'Now select the waverange for the feature.'
      
      veri,xmin,/nostatus
      wait,0.2
      print,xmin
      
      veri,xmax,/nostatus
      wait,0.2
      print,xmax
          
      xmin = min([xmin,xmax],max=xmax)
      
      f = sh_select(f,f.data.wave ge xmin and f.data.wave le xmax)
  ENDIF
  
  ;; Now derive the central wavelength which divides the feature
  ;; in 2 equal halves.
  wave = f.data.wave
  flux = f.data.flux
  np = n_elements(wave)
  
  ;; the flux until point n
  intval = make_array(np,value=0d0)
  intval[1:np-2]=0.5*(wave[2:np-1]-wave[0:np-3])
  intval[0]=0.25*(wave[1]-wave[0])
  intval[np-1]=0.25*(wave[np-1]-wave[np-2])
  fluxbin = flux*intval
  cumm_fluxbin = 0d0*fluxbin+fluxbin[0]
  FOR i=1,np-1 DO cumm_fluxbin[i]=cumm_fluxbin[i-1]+fluxbin[i]
  tot_fluxbin = cumm_fluxbin[np-1]
  foo = min(abs(cumm_fluxbin - tot_fluxbin/2d0),index)
  central_wave = wave[index]

  plq,f,/auto,ymin=0,_extra=_extra
  pl,smooth_f,/oplot,thi=3,nsum=5,ps=0,_extra=_extra
  pl,central_wave*[1,1],!y.crange,/opl,_extra=_extra

  message,/information,'Indicate the peak position'
  cursor,xpeak,ypeak,2,/down
  print,xpeak,ypeak
  oplot,ps=4,[xpeak],[ypeak],col=!d.n_colors/2
  oplot,!x.crange,ypeak*[0.5,0.5],col=!d.n_colors/2
  message,/information,'Indicate the position of the left and right Half-Max'
  cursor,xhleft,yhleft,2,/down
  oplot,ps=4,[xhleft],[yhleft],col=!d.n_colors/3
  cursor,xhright,yhright,2,/down
  oplot,ps=4,[xhright],[yhright],col=!d.n_colors/3
  xhleft = min([xhleft,xhright],max=xhright)
  
;;  print,'Integrated Flux',totalflux*1e-14,' W/m/m'
  print,'---------------------------------------------------------'
  print,object(in)
  print,'peak/cont          :',ypeak,' at ',xpeak,' mu'
  print,'FWHM               :',xhright-xhleft, ' mu, from ',xhleft,' to ',xhright,' mu'
  print,'Feature center     :',central_wave, ' mu'
  
  IF n_elements(cont) NE 0 THEN BEGIN
      f_jansky = sh_calcaar(f,mult=cont,/quiet)
      flux = sh_integrate(f_jansky,/noplot,/quiet)
      print,'Integrated Flux:',flux,' W/m/m'
  ENDIF ELSE BEGIN
      flux = 0d0
  ENDELSE
  print,'---------------------------------------------------------'

  return,{featur_30_props,name:object(in),xpeak:xpeak,ypeak:ypeak, $
          fwhm:xhright-xhleft,flux:flux,cpeak:central_wave}
end
