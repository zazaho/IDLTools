pro power_map_apex_bolero,vhblind,bias,vl,imagein,imageout

;1ere etape: conversion signal de sortie bolero en point milieu
;calibration signal point milieu
;variable: calib
restore,'/Users/artemis/Documents/Calib_partemis/cal_vrl_vhb_high_partemis.xdr'

wo=where(abs(calib(0,*).vhblind-vhblind) le 2.e-2)
ptmil=fltarr(16,16)
for i=0,15 do begin
	for j=0,15 do begin
		ptmil(i,j)=interpol(calib(*,wo(0)).vrl,calib(*,wo(0)).signal(i,j),imagein(i,j))
		ptmil(i,j)=ptmil(i,j)-vl
	endfor
endfor

;2eme etape: conversion point milieu en puissance incidente
;points milieux interpoles pour tous les flux entre 0 et 60pW et tous les bias entre 2.5 et 4V
;variable: ptmils
restore,'/Users/artemis/Documents/Calib_partemis/calibration_450microns_keithley.xdr'

wb=where(abs(ptmils(*,0).bias-bias) lt 2.e-2)
imageout=fltarr(16,16)
for i=0,15 do begin
	for j=0,15 do begin
		imageout(i,j)=interpol(ptmils(wb(0),*).power,ptmils(wb(0),*).ptmil(i,j),ptmil(i,j))
	endfor
endfor

return
end
