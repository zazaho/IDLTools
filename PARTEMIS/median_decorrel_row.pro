;+
; NAME:
;	MEDIAN_DECORREL_ROW
;
; PURPOSE:
;
;	Subtract median of correlated noise to signal.	
;
; CALLING SEQUENCE:
;
;	MEDIAN_DECORREL_ROW, Donnees, Donnees_corrigees, Correl_matrice, cube_masque = cube_masque
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
;		MEDIAN_DECORREL_ROW, Donnees, Donnees_corrigees, Correl_matrice, cube_masque = cube_masque
;
; MODIFICATION HISTORY:
; 	
;-


pro median_decorrel_row, donnees, donnees_corrigees, correl_matrice, cube_masque = cube_masque

; subtract median of correlated noise to signal, treating one row of the array at a time



if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	MEDIAN_DECORREL_ROW, donnees, donnees_corrigees, correl_matrice'

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

bruit = dblarr(taille(2),taille(3))

cube_corrige = cube*0.
correl_matrice = dblarr(taille(1),taille(2))
rms_noise = dblarr(taille(1),taille(2))
 
;nychan = (intarr(taille(1))+1)#transpose(indgen(taille(2)))
;vec_nychan = intarr(taille(1)*taille(2))
;vec_nychan(*) = nychan(*)
;
;row_cube = reform(matrix_multiply(vec_nychan,intarr(taille(3))+1,/btranspose),taille(1),taille(2),taille(3))

;timeline = findgen(taille(3))*25.e-3

;time_cube = matrix_multiply(fltarr(taille(1)*taille(2))+1.,timeline,/btranspose)
;time_cube = reform(time_cube,taille(1),taille(2),taille(3))

;goodpix_ima = donnees.goodpix_ima
;vec_goodpix_ima = fltarr(taille(1)*taille(2))
;vec_goodpix_ima(*) = goodpix_ima(*)
;
;goodpix_cube = reform(matrix_multiply(vec_goodpix_ima,fltarr(taille(3))+1.,/btranspose),taille(1),taille(2),taille(3))

goodpix_ima = donnees.goodpix_ima
good_row = intarr(taille(2))+1
for j = 0, taille(2)-1 do begin
  ind_good_row = where(goodpix_ima(*,j) eq 1, count)
  if count lt 1 then good_row(j) = 0
endfor

time_corr = 0.02     		;    2.   		; sec
nimc = nint(time_corr/25.e-3/2.)

if keyword_set(cube_masque) then begin
	for k=0L, taille(3)-1 do begin
          for j = 0, taille(2)-1 do begin          	
;	   row = cube(*,j,k)
;	   mask_row  = cube_mask(*,j,k)*goodpix_ima(*,j)   		;   avoiding source_mask and bad pixels
;	   index = where(mask_row eq 1, count)
	   kmin = max([0,k-nimc])
	   kmax = min([k+nimc,taille(3)-1])
	   row_time = cube(*,j,kmin:kmax)
	   goodpix_time = matrix_multiply(goodpix_ima(*,j),intarr(2*nimc+1)+1,/btranspose) 
	   mask_row_time = cube_mask(*,j,kmin:kmax)*goodpix_time   					;   avoiding source_mask and bad pixels
	   index = where(mask_row_time eq 1, count)
	   
;	   index = where(cube_mask eq 1 and goodpix_cube eq 1 and row_cube eq j and abs(time_cube-timeline(k)) lt 2.,count)
	   
	   if (count le 3 and good_row(j) eq 1) then begin	   
            print, 'MEDIAN_DECORREL_ROW : WARNING : Cannot calculate median skynoise avoiding mask at sample : ', k, " row ", j," Subtracting global median instead "
;	    stop
;	     count = taille(2)
;	     index = lindgen(count)
;	     index = where(goodpix_ima(*,j) eq 1, count)
;	     index = where(goodpix_cube eq 1 and row_cube eq j and abs(time_cube-timeline(k)) lt 2.,count)
	     index = where(goodpix_time eq 1, count)
	   endif
	   if count gt 0 then begin
;	     bruit(j,k) = median(row(index))
;	     bruit(j,k) = median(cube(index))
	     bruit(j,k) = median(row_time(index))
	   endif else begin
	     bruit(j,k) = 0.
	   endelse  
	  endfor 
	endfor
endif else begin
	for k=0L, taille(3)-1 do begin
          for j = 0, taille(2)-1 do begin          	
	   row = cube(*,j,k)
	   mask_row  = goodpix_ima(*,j)   		;   avoiding bad pixels
	   index = where(mask_row eq 1, count)
	   if count gt 0 then begin
	     bruit(j,k) = median(row(index))
	   endif else begin
	     bruit(j,k) = 0.
	   endelse  
	  endfor 
	endfor	
endelse
bruit0 = bruit


for j=0,(taille(2)-1) do begin
	bruit = bruit0(j,*)	
	ex=mean(bruit)
	sx0=sigma(bruit)
;
	if sx0 gt 0. then begin
 		bruit = bruit/sx0
 		ex = ex/sx0
 		sx = 1.d0
	endif else begin	
	     if good_row(j) eq 1 then print, 'MEDIAN_DECORREL_ROW : WARNING : Correlated noise for row ', j, ' has zero variance'	
 		ex = 0.d0
 		sx = 1.d0
 		sx0 = 1.d0
	endelse	
;	
 for i=0,(taille(1)-1) do begin
	pixel=cube(i,j,*)
	pixel_corrige = pixel
		
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
