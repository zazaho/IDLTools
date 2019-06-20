;+
; NAME:
;	MEDIAN_DECORREL_SCAN
;
; PURPOSE:
;
;	Subtract median of correlated noise to signal.	
;
; CALLING SEQUENCE:
;
;	MEDIAN_DECORREL_SCAN, Donnees, Donnees_corrigees, Correl_matrice, cube_masque = cube_masque
;
; INPUTS:
;
;	Donnees:	Data structure.
;		
; OUTPUTS:
;
;	Donnes_corrigees:	Data after decorrelation.
;	Correl_matrice:		Correlation matrice.
;
; EXAMPLE:
;		MEDIAN_DECORREL_SCAN, Donnees, Donnees_corrigees, Correl_matrice, cube_masque = cube_masque
;
; MODIFICATION HISTORY:
; 	
;-


pro median_decorrel_scan, donnees, donnees_corrigees, correl_matrice, cube_masque = cube_masque

; subtract median of correlated noise to signal



if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	MEDIAN_DECORREL_SCAN, donnees, donnees_corrigees, correl_matrice'

	return
endif

;;;;
if keyword_set(cube_masque) then begin
   cube_mask = 1-cube_masque	; cube_mask vaut 1 en dehors de la source et 0 sur la source
endif
;;;;


cube = donnees.cube
donnees_corrigees = donnees

taille = size(cube)

bruit = dblarr(taille(3))


cube_corrige = cube*0.
correl_matrice = dblarr(taille(1),taille(2))
rms_noise = dblarr(taille(1),taille(2))


for i=0L, taille(3)-1 do begin
	image = cube(*,*,i)
	   if keyword_set(cube_masque) then begin 
	      mask  = cube_mask(*,*,i)*donnees.goodpix_ima   		;   avoiding source and bad pixels
	   endif else begin
	      mask  = donnees.goodpix_ima   		                ;   avoiding bad pixels	   
	   endelse   
	index = where(mask eq 1, count)
	   if count le 10 then begin	   
             print, 'MEDIAN_DECORREL_SCAN : WARNING : Cannot calculate median skynoise avoiding mask at sample : ', i, " Subtracting global median instead "
	     count = taille(1)*taille(2)
	     index = lindgen(count)
	   endif
	bruit(i) = median(image(index))
endfor
bruit0 = bruit

;bruit = bruit0	
ex=mean(bruit)
sx0=sigma(bruit)
	
if sx0 gt 0. then begin
 	bruit = bruit/sx0
;	bruit1 = bruit0/sx0
 	ex = ex/sx0
 	sx = 1.d0
endif else begin
	print, 'Warning : Correlated noise has zero variance'	
 	ex = 0.d0
 	sx = 1.d0
 	sx0 = 1.d0
endelse

for i=0,(taille(1)-1) do begin
 for j=0,(taille(2)-1) do begin
	pixel=cube(i,j,*)
	pixel_corrige = pixel
;	bruit = bruit0	
;	ex=mean(bruit)
;	sx0=sigma(bruit)
	
;	if sx0 gt 0. then begin
; 		bruit = bruit/sx0
;		bruit1 = bruit0/sx0
;		ex = ex/sx0
; 		sx = 1.d0
;	endif else begin
;		print, 'Warning : Correlated noise has zero variance'	
; 		ex = 0.d0
; 		sx = 1.d0
; 		sx0 = 1.d0
;	endelse
	
	pixel=pixel/sx0
	pixel_corrige=pixel_corrige/sx0
	exy = mean(bruit*pixel)
	ex=mean(bruit)
	ey=mean(pixel)
	sx=sigma(bruit)
	sy=sigma(pixel)
	;
	if (sy gt 1d-6 and sx gt 1d-6) then begin
		rho=(exy-ex*ey)/sx/sy
		a = rho*sy/sx	
		b = ey-a*ex
;		pixel_corrige = pixel_corrige-a*bruit1
		pixel_corrige = pixel_corrige-a*bruit

	endif else begin		; sy = 0.  <--> y = cte
		rho=0.d0
		a = 0.d0
		b = ey
	endelse

	correl_matrice(i,j)=rho

	cube_corrige(i,j,*) = pixel_corrige*sx0
	rms_noise(i,j) = stdev(cube_corrige(i,j,*))
;
 endfor
endfor 



donnees_corrigees.cube = cube_corrige
donnees_corrigees.rmsmoy = rms_noise

return
end
