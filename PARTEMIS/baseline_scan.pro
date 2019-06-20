;+
; NAME:
;	BASELINE_SCAN
;
; PURPOSE:
;
;	Subtract the order 1 polynomial fitted signal to the signal and give rms
;	noise. It subtracts median baseline if there are less than 10 data points
;	available.
;
; CALLING SEQUENCE:
;	
;	BASELINE_SCAN, Cube, Cube_mask_init, Cube_corrige, Rms_noise
;
; INPUTS:
;
;	Cube:	Initial data cube (double array).
;	Cube_mask_init:	 Mask(double array).
;
; OUTPUTS:
;
;	Cube_corrige:	Baseline subtracted data(double array).
;	Rms_noise:	Calculated noise(double array).
;
; EXAMPLE:
;
;		BASELINE_SCAN, CUBE, CUBE_MASK_INIT, CUBE_CORRIGE, RMS_NOISE
;
; MODIFICATION HISTORY:
; 	
;-


pro baseline_scan, cube, cube_mask_init, cube_corrige, rms_noise



if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	BASELINE_SCAN, Cube, Cube_mask_init, Cube_corrige, Rms_noise'
	return
endif

;subtract the order 1 polynomial fitted signal to the signal and calculate rms noise 

cube_mask = 1-cube_mask_init	; cube_mask vaut 1 en dehors de la source et 0 sur la source


taille = size(cube)

time = findgen(taille(3))

cube_corrige = cube*0.
rms_noise = dblarr(taille(1),taille(2))



for j=0,taille(2)-1 do begin
      for i=0, taille(1)-1 do begin
	   signal=cube(i,j,*)
           id = where(cube_mask(i,j,*) eq 1, count)
	   
	   if count gt 10 then begin
		coeff_base = poly_fit(time(id),signal(id),1,yfit=base, $
	                         yerror = base_error)
		base = coeff_base(0)+coeff_base(1)*time
;	   if count gt 15 then begin
;		coeff_base = poly_fit(time(id),signal(id),2,yfit=base, $
;	                         yerror = base_error)
;		base = coeff_base(0)+coeff_base(1)*time+coeff_base(2)*time^2		
	   endif else begin
	   
           print, 'BASELINE_SCAN : WARNING : Not enough baseline avoiding mask_base for pixel : ', i, j, " Subtracting median baseline instead"
	        base = median(signal)
		base_error = stdev(signal)
	   endelse

	signal_corrige = signal - base 
	
	cube_corrige(i,j,*) = signal_corrige

	rms_noise(i,j) = base_error
	
       endfor
endfor 

return

end
