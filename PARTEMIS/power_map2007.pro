;+
; NAME:
;	POWER_MAP
;
; PURPOSE:
;
;	Converts bolometer signal from V to pW.
;
; CALLING SEQUENCE:
;	
;	POWER_MAP, Offset, Bias, Vl, Cubein, Cubeout
;	
; INPUTS:
;
;	Offset:	Vh-Vl.
;	Bias:	Bias voltage.	
;	Vl:	Vl.
;	Cubein:	Input data.
;
; OUTPUTS:
;
;	Cubeout:	Output data.
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration
;			table name
;		
; EXAMPLE:
;	
;		POWER_MAP, Offset, Bias, Vl, Cubein, Cubeout
;
; MODIFICATION HISTORY:
; 	
;-


pro power_map,offset,bias,vl,cubein,cubeout

;convert bolometer signal from V to pW


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	POWER_MAP, offset, bias, vl, cubein, cubeout'

	return
endif

COMMON obs1_configb

cubein = -cubein

taille=size(cubein)

;1ere etape: conversion signal de sortie keithley en point milieu
;calibration signal point milieu
;variable: calib

restore, work_dir + 'Calib_partemis/'+'calibration_vrl_offset_450microns.xdr'  ; restoration des fichiers de calibration

wo=where(abs(calib(0,*).offset-offset) lt 2.e-2, count0)
ptmil=cubein*0
for k=0, n_elements(cubein(0,0,*))-1 do begin
	for i=0,(taille(1)-1) do begin
	for j=0,(taille(2)-1) do begin
		ptmil(i,j,k)=interpol(calib(*,wo(0)).vrl,calib(*,wo(0)).signal(i,j),cubein(i,j,k))
		ptmil(i,j,k)=ptmil(i,j,k)-vl
	endfor
	endfor
endfor

;2eme etape: conversion point milieu en puissance incidente
;points milieux interpoles pour tous les flux entre 0 et 60pW et tous les bias entre 2.5 et 4V
;variable: ptmils

restore, work_dir + 'Calib_partemis/' + 'calibration_450microns_keithley.xdr' ; restoration des fichiers de calibration

wb=where(abs(ptmils(*,0).bias-bias) lt 2.e-2, count)
cubeout=cubein*0.
for k=0, n_elements(cubeout(0,0,*))-1 do begin
	for i=0,(taille(1)-1) do begin
	for j=0,(taille(2)-1) do begin
		cubeout(i,j,k)=interpol(ptmils(wb(0),*).power,ptmils(wb(0),*).ptmil(i,j),ptmil(i,j,k))
	endfor
	endfor
endfor

return
end
