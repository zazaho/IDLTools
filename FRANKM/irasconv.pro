PRO irasconv,aarstart,iras12=iras12,iras25=iras25,iras60=iras60,iras100=iras100
;if is_aar(aarstart) then begin
  aar = aarstart
;endif else begin
; aar = tabelmaken(aarstart,/back)
;endelse

  ;;(SH May  9 2003)
  ;;We first interpolate the results to a much finer grid
  ;;Do this in log-log especcially for the longer wavelengths
  logwave = alog10(aar.data.wave)
  logflux = alog10(aar.data.flux)
  new_logwave = alog10(2.0+0.1*dindgen(2000))
  new_logflux = shc_interpol(logwave,logflux,new_logwave)
  
  aar = define_aar(length=2000)
  aar.data.wave=10^new_logwave
  aar.data.flux=10^new_logflux
 
; Eerst IRAS gevoeligheden inlezen.

  golf = fltarr(87)
  resp = fltarr(87)
  Iras12 = fltarr(2,18)
  Iras25 = fltarr(2,32)
  Iras60 = fltarr(2,21)
  Iras100 = fltarr(2,16)
  openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS12mu', /get_lun
  readf, lun, Iras12
  free_lun, lun
  
  for i=0,17 do begin
      golf(17-i) = 1000./IRAS12(0,i)
      resp(17-i) = IRAS12(1,i)
  endfor
  
  openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS25mu', /get_lun
  readf, lun, Iras25
  free_lun, lun
  
  for i=31,0,-1 do begin
      golf(49-i) = 1000./IRAS25(0,i)
      resp(49-i) = IRAS25(1,i)
  endfor
  
  openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS60mu', /get_lun
  readf, lun, Iras60
  free_lun, lun

  for i=20,0,-1 do begin
      golf(70-i) = 1000./IRAS60(0,i)
      resp(70-i) = IRAS60(1,i)
  endfor
  
  openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS100mu', /get_lun
  readf, lun, Iras100
  free_lun, lun
  
  for i=15,0,-1 do begin
      golf(86-i) = 1000./IRAS100(0,i)
      resp(86-i) = IRAS100(1,i)
  endfor
  
  aar1 = select(aar,aar.data.wave gt 7.0 and aar.data.wave lt 140.0)
;plotaar,aar1
  aar1.data.wave = 1000./aar1.data.wave
  aar1 = sws_rebin(aar1,bin=.25,/noplot,over=1)
  
  lambda = aar1.data.wave
  jansky = aar1.data.flux
  
;oplot,lambda,jansky
;oplot,golf(0:17),resp(0:17)*1000.,psym=-1,color=100
  
  waar = where(lambda gt golf(0) and lambda lt golf(17))
  if (waar(0) ne -1) then begin 
      factor = interpol(resp(0:17),golf(0:17),lambda(waar))
      totaal1 = total(jansky(waar)*factor)
      delen1 = total(factor)
  endif else begin
      delen1 = 0
      totaal1 = 0
  endelse
  
  waar = where(lambda gt golf(18) and lambda lt golf(49))
  if (waar(0) ne -1) then begin 
      factor = interpol(resp(18:49),golf(18:49),lambda(waar))
      totaal2 = total(jansky(waar)*factor)
      delen2 = total(factor)
  endif else begin
      delen2 = 0
      totaal2 = 0
  endelse
  
  waar = where(lambda gt golf(50) and lambda lt golf(70))
  if (waar(0) ne -1) then begin 
      factor = interpol(resp(50:70),golf(50:70),lambda(waar))
      totaal3 = total(jansky(waar)*factor)
      delen3 = total(factor)
  endif else begin
      delen3 = 0
      totaal3 = 0
  endelse
  
  waar = where(lambda gt golf(71) and lambda lt golf(86))
  if (waar(0) ne -1) then begin 
      factor = interpol(resp(71:86),golf(71:86),lambda(waar))
      totaal4 = total(jansky(waar)*factor)
      delen4 = total(factor)
  endif else begin
      delen4 = 0
      totaal4 = 0
  endelse
  
;print,totaal1,totaal2,totaal3,totaal4
;print,delen1,delen2,delen3,delen4
  result1 = 1.10*totaal1/delen1
  result2 = 1.10*totaal2/delen2
  result3 = 1.05*totaal3/delen3
  result4 = 1.01*totaal4/delen4
;print,result1,result2,result3,result4
  
  lambda = 1000./lambda
  golf = 1000./golf
  plot,lambda,jansky,psym=3,xra=[lambda(n_elements(lambda)-1),lambda(0)]
;oplot,[12,25,60,100],[result1, result2, result3, result4],psym=2,color=200
  
  if (delen1 gt 0) then begin
      Print,'IRAS FLUX 12 micron:',result1
      oplot,golf(0:17),(resp(0:17)/resp(0:17))*(result1),psym=-3,color=80
      oplot,golf(0:17),resp(0:17)*(result1),psym=-3,color=80
  endif
  
  if (delen2 gt 0) then begin
      Print,'IRAS FLUX 25 micron:',result2
      oplot,golf(18:49),(resp(18:49)/resp(18:49))*(result2),psym=-3,color=100
      oplot,golf(18:49),resp(18:49)*(result2),psym=-3,color=100
  endif
  
  if (delen3 gt 0) then begin
      Print,'IRAS FLUX 60 micron:',result3
      oplot,golf(50:70),(resp(50:70)/resp(50:70))*(result3),psym=-3,color=120
      oplot,golf(50:70),resp(50:70)*(result3),psym=-3,color=120
  endif
  
  if (delen4 gt 0) then begin
      Print,'IRAS FLUX 100 micron:',result4
      oplot,golf(71:86),(resp(71:86)/resp(71:86))*(result4),psym=-3,color=140
      oplot,golf(71:86),resp(71:86)*(result4),psym=-3,color=140
  endif
  
  print,' IRAS(12)/IRAS(25) = ',result1/result2
  print,' IRAS(25)/IRAS(60) = ',result2/result3
  print,' IRAS(60)/IRAS(100) = ',result3/result4
  
  iras12 = result1
  iras25 = result2
  iras60 = result3
  iras100= result4
  
END 
