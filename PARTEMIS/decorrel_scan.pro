;+
; NAME:
;	DECORREL_SCAN
;
; PURPOSE:
;
;	Subtract correlated noise to signal.
;
; CALLING SEQUENCE:
;
;	DECORREL_SCAN, Donnees, Cube_mask_init, Donnees_corrigees, Correl_matrice
;
;
; INPUTS:
;
;	Donnees:	Initial data structure
;	Cube_mask_init:	Mask	
;
;
; OUTPUTS:
;
;	Donnees_corrigees:	Data after decorrelation.
;	Correl_matrice:		Correlation matrice.
;
; EXAMPLE:
;		DECORREL_SCAN, Donnees_sub, Cube_masque_model, Donnees_corrigees, Correlation_matrice
;
; MODIFICATION HISTORY:
;-

pro decorrel_scan, donnees, cube_mask_init, donnees_corrigees, correl_matrice

;subtract correlated noise to signal


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	DECORREL_SCAN, donnees, cube_mask_init, donnees_corrigees, correl_matrice'
	return
endif	

cube = donnees.cube
donnees_corrigees = donnees

cube_mask = 1-cube_mask_init  ;vaut 1 en dehors de la source et 0 sur la source


taille = size(cube)

bruit = dblarr(taille(3))


cube_corrige = cube*0.
correl_matrice = dblarr(taille(1),taille(2))

for i=0, taille(3)-1 do begin
 cube_mask(*,*,i) = cube_mask(*,*,i)*donnees.goodpix_ima   ;   avoiding bad pixels
 tot =  total(cube_mask(*,*,i))
 if (tot ne 0.) then begin
   bruit(i)=total(cube(*,*,i)*cube_mask(*,*,i))/tot
 endif
endfor

bruit0 = bruit


for i=0,(taille(1)-1) do begin
 for j=0,(taille(2)-1) do begin
	pixel=cube(i,j,*)
	pixel_corrige = pixel
	id = where(cube_mask(i,j,*) eq 1, count)
	if count gt 0 then begin
	    pixel = pixel(id)
	    bruit = bruit0(id)
		endif else begin
;	    print, 'Warning : source is always seen by pixel ', i, j			
	endelse
	
	ex=mean(bruit)
	sx0=sigma(bruit)
	
	if sx0 gt 0. then begin
 		bruit = bruit/sx0
		bruit1 = bruit0/sx0
 		ex = ex/sx0
 		sx = 1.d0
	endif else begin
		print, 'Warning : Correlated noise has zero variance'	
 		ex = 0.d0
 		sx = 1.d0
 		sx0 = 1.d0
	endelse
	
	pixel=pixel/sx0
	pixel_corrige=pixel_corrige/sx0
	exy = mean(bruit*pixel)
	ex=mean(bruit)
	ey=mean(pixel)
	sx=sigma(bruit)
	sy=sigma(pixel)
	
	if (sy gt 1d-6 and sx gt 1d-6) then begin
		rho=(exy-ex*ey)/sx/sy
		a = rho*sy/sx	
		b = ey-a*ex
		pixel_corrige = pixel_corrige-a*bruit1

	endif else begin		; sy = 0.  <--> y = cte
		rho=0.d0
		a = 0.d0
		b = ey
	endelse

	correl_matrice(i,j)=rho

	if count gt 0 then begin
		cube_corrige(i,j,*) = pixel_corrige*sx0

	endif else begin
;	    print, 'Warning : source is always seen by pixel ', i, j
		
		cube_corrige(i,j,*) = cube(i,j,*) 
		correl_matrice(i,j) = 0.
	endelse

 endfor
endfor 



donnees_corrigees.cube = cube_corrige

return
end
