;+
; NAME:
;	MEAN_BASELINE_SCAN
;
; PURPOSE:
;
;	Subtract the mean value to the signal and give rms noise.
;
; CALLING SEQUENCE:
;	
;	MEAN_BASELINE_SCAN, Cube, Cube_corrige, Rms_noise
;
; INPUTS:
;
;	Cube:	Initial data.		
;
; OUTPUTS:
;
;	Cube_corrige:	Data after noise removal.
;	Rms_noise:	Calculated noise.
;
; EXAMPLE:
;	
;		MEAN_BASELINE_SCAN, Cube, Cube_corrige, Rms_noise
;
; MODIFICATION HISTORY:
; 	
;-


pro mean_baseline_scan, cube, cube_corrige, rms_noise, cube_masque = cube_masque

; subtract the mean value to the signal and calculate rms noise 


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	MEAN_BASELINE_SCAN, cube, cube_corrige, rms_noise, cube_masque = cube_masque'

	return
endif

;;;;
if keyword_set(cube_masque) then begin
   cube_mask = 1-cube_masque	; cube_mask vaut 1 en dehors de la source et 0 sur la source
endif
;;;;

taille = size(cube)

time = findgen(taille(3))

cube_corrige = cube*0.
rms_noise = dblarr(taille(1),taille(2))

;
for j=0,taille(2)-1 do begin
for i=0, taille(1)-1 do begin
	signal=cube(i,j,*)
	
	count = taille(3)
	id = lindgen(count)
	
	if keyword_set(cube_masque) then begin
           id = where(cube_mask(i,j,*) eq 1, count)
	   ;	 
	   if count le 10 then begin	   
             print, 'MEAN_BASELINE_SCAN : WARNING : Not enough baseline avoiding mask_base for pixel : ', i, j, " Subtracting global mean baseline"
	     count = taille(3)
	     id = lindgen(count)
	   endif
	endif

	base = mean(signal(id))

	signal_corrige = signal - base 
	
	cube_corrige(i,j,*) = signal_corrige

;	rms_noise(i,j) = stdev(signal_corrige)
	rms_noise(i,j) = stdev(signal_corrige(id))
;
endfor
endfor 

return
end
