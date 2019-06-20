;+
; NAME:
;	EXTRACT_IMA_MOY
;
; PURPOSE:
;
;	Return average image and noise from a data cube.
;
; CALLING SEQUENCE:
;	
;	Image = EXTRACT_IMA_MOY(Cube, Ima_rms)
;
; INPUTS:
;
;	Cube:	Input data cube.
;
; OPTIONAL INPUTS:
;
;	Cube_rms:	Averaged data.
;	Clip:		Threshold to flag data.
;	Rms_weight:	Noise calculation method.	
;	
; OUTPUTS:
;
;	Ima_rms:	Averaged noise.
;
; EXAMPLE:
;
;		Rmsmoy = EXTRACT_IMA_MOY(Cube_rms,Rms_rms)
;
; MODIFICATION HISTORY:
;
;-

function extract_ima_moy, cube, ima_rms, cube_rms = cube_rms, clip = threshold, rms_weight = rms_weight

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	ima_moy = extract_ima_moy, cube, ima_rms, cube_rms = cube_rms, clip = threshold, rms_weight = rms_weight'
	return, -1
endif

taille=size(cube)

image = dblarr(taille(1),taille(2))
ima_rms = image

;;;;;;;;;;;;if keyword_set(clip) then begin
if keyword_set(threshold) then begin
 print, "Clipping above threshold : ", threshold, "... sigmas"
  if keyword_set(cube_rms) then begin

	for i=0,taille(1)-1 do begin
	for j=0,taille(2)-1 do begin
	  if (stdev(cube(i,j,*)) gt 0.) then begin
		good_id = sigma_clip(cube(i,j,*), threshold)
		weight_tot = total(1./cube_rms(i,j,good_id)^2)
		image(i,j) = total(cube(i,j,good_id)/cube_rms(i,j,good_id)^2)/weight_tot
		if keyword_set(rms_weight) then begin
		    ima_rms(i,j) = 1./sqrt(weight_tot)
		endif else begin    
		 ima_rms(i,j) = total(cube(i,j,good_id)^2/cube_rms(i,j,good_id)^2)/weight_tot
		 ima_rms(i,j) = ima_rms(i,j) - image(i,j)^2
		 ima_rms(i,j) = ima_rms(i,j)*n_elements(good_id)/(n_elements(good_id)-1)         	; Standard deviation of the distribution
		 ima_rms(i,j) = sqrt(ima_rms(i,j)*total(1./cube_rms(i,j,good_id)^4)/weight_tot^2)	; Standard deviation of the mean
		endelse
	   endif
	endfor
	endfor

  endif else begin

	for i=0,taille(1)-1 do begin
	for j=0,taille(2)-1 do begin
		good_id = sigma_clip(cube(i,j,*), threshold)
		image(i,j) = mean(cube(i,j,good_id))
;		ima_rms(i,j) = sigma(cube(i,j,good_id))        					; Standard deviation of the distribution
		ima_rms(i,j) = sigma(cube(i,j,good_id))/sqrt(n_elements(good_id))		; Standard deviation of the mean	
	endfor
	endfor

  endelse

endif else begin

  if keyword_set(cube_rms) then begin

	for i=0,taille(1)-1 do begin
	for j=0,taille(2)-1 do begin
	  if (stdev(cube(i,j,*)) gt 0.) then begin	
		weight_tot = total(1./cube_rms(i,j,*)^2)
		image(i,j) = total(cube(i,j,*)/cube_rms(i,j,*)^2)/weight_tot
		if keyword_set(rms_weight) then begin
		   ima_rms(i,j) = 1./sqrt(weight_tot)
		endif else begin
		  ima_rms(i,j) = total(cube(i,j,*)^2/cube_rms(i,j,*)^2)/weight_tot
		  ima_rms(i,j) = ima_rms(i,j) - image(i,j)^2
		  ima_rms(i,j) = ima_rms(i,j)*taille(3)/(taille(3)-1)         				; Standard deviation of the distribution
		  ima_rms(i,j) = sqrt(ima_rms(i,j)*total(1./cube_rms(i,j,*)^4)/weight_tot^2)		; Standard deviation of the mean
		endelse
	  endif		
	endfor
	endfor

  endif else begin

	for i=0,taille(1)-1 do begin
	for j=0,taille(2)-1 do begin		
		image(i,j) = mean(cube(i,j,*))
;		ima_rms(i,j) = sigma(cube(i,j,*))        					; Standard deviation of the distribution
		ima_rms(i,j) = sigma(cube(i,j,*))/sqrt(taille(3))				; Standard deviation of the mean		
	endfor
	endfor

  endelse
endelse  

;stop

return, image
end
