pro test_decorrel, cube, mask, cube_corrige, correl_matrice, a_matrice
;
	;
	idref = where(mask eq 1)
	;
	taille = size(cube)
	;
	bruit = dblarr(taille(3))
	;
	cube_corrige = cube*0.
	correl_matrice = dblarr(16,16)
	a_matrice = dblarr(16,16)
	;
	for i=0L, taille(3)-1 do bruit(i)=total(cube(*,*,i)*mask(*,*))/total(mask)
;	for i=0L, taille(3)-1 do bruit(i)=median(cube(*,*,i))
	;
	ex=mean(bruit)
	sx0=sigma(bruit)
;
	if sx0 le 0. then begin
		print, 'Warning : Correlated noise has zero variance'
	endif
;
	bruit = bruit/sx0
	ex = ex/sx0
	sx = 1.d0
;
	;
	for i=0,15 do begin
	for j=0,15 do begin
;		pixel=cube(i,j,*)
		pixel=cube(i,j,*)/sx0
		exy = mean(bruit*pixel)
;		ex=mean(bruit)
		ey=mean(pixel)
;		sx=sigma(bruit)
		sy=sigma(pixel)
		;
		if sy gt 1d-6 then begin
			rho=(exy-ex*ey)/sx/sy
			a = rho*sy/sx	
;			b = ey-a*ex
;			pixel_corrige = pixel-a*bruit-b
			pixel_corrige = pixel-a*bruit
;
		endif else begin		; sy = 0.  <--> y = cte
			rho=0.d0
			a = 0.d0
			b = ey
			pixel_corrige = pixel
		endelse
;
		correl_matrice(i,j)=rho
		a_matrice(i,j)	= a
;
;		cube_corrige(i,j,*) = pixel_corrige
		cube_corrige(i,j,*) = pixel_corrige*sx0
	;
	endfor
	endfor 
;
;print, max(abs(correl_matrice))
;print, min(abs(correl_matrice))
;
;
return
end
