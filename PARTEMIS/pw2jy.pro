;+
; NAME:
;	PW2JY
;
; PURPOSE:
;
;	Convert data in pW to Jansky with atmospheric extinction correction.
;	
; CALLING SEQUENCE:
;	
;	PW2JY, Donnees, Donnees_Jy, Tau=Tau, Conv=Conv
;
; INPUTS:
;
;	Donnees:	Input data structure.
;	Tau:		Optical opacity.
;	Conv:		Conversion factor pW/Jy.	
;
; OUTPUTS:
;
;	Donnees_Jy:	Output data structure.
;
; EXAMPLE:
;
;		PW2JY, Donnees, Donnees_Jy, Tau=0.8, Conv=3.*10^(-4)
;
; MODIFICATION HISTORY:
; 	
;-



pro pW2Jy, donnees, donnees_Jy, tau=tau, Conv=Conv

; convert subscan data cube in pW(atmospheric extinction corrected) to Jansky 


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	pW2Jy, donnees, donnees_Jy, tau=tau, Conv=Conv'

	return
endif

print, "tau = ", tau
print, "Conv= ", Conv			; pW/Jy

donnees_Jy=donnees

amass = 1./sin(donnees.elevatio*!pi/180.)                               ; airmass
        donnees_Jy.cube=donnees.cube*0.
	donnees_Jy.cube = donnees.cube*exp(tau*amass)/Conv		; Calibration (extinction correction + pW --> Jy conversion)
	donnees_Jy.unit = 'Jy/beam ext. corr.'


return
end
