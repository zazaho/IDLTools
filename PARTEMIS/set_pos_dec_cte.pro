pro set_pos_dec_cte, datastr, echant, positions, out_ima_dim


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	SET_POS, datastr, pos_ini, pixelsize, elevation, delta_pos, position'
	return
endif

; pixelsize = taille du pixel en arcsec dans l'image re-echantillonnee
pixelsize = datastr.cdelt1/echant

;el0 = datastr.elevatio*!pi/180

; determination de la taille de l'image (re-echantillonnee) en sortie
taille = size(datastr.cube)
size_array = taille(1)
nra = n_elements(datastr.lst)
pas_ra = datastr.pas_az			; pas elementaire le long de la direction de scanning (ici RA)
champ_det = datastr.cdelt1 * taille(1)

; le facteur 3 ci-dessous est l'arrondi de 2*sqrt(2) pour prendre en compte le
; cas le plus defavorable d'une rotation a 45 deg

;out_ima_size = nra * pas_ra + 3 * champ_det    ; en arcsec
out_ima_size = nra * abs(pas_ra) + 2 * champ_det    ; en arcsec
;out_ima_size = (nra * pas_ra + champ_det)    ; en arcsec

out_ima_dim = fix(out_ima_size / pixelsize) + 1

offset = fix(1.5*champ_det/pixelsize)	     ; marge pour les rotations -> RADEC

; Determination du pixel (0,0) dans l'image finale
pos_ini = lonarr(2)
crota1 = datastr.crota1		; Roll angle of the array (deg)
;print, "pas_ra ", pas_ra, "  crota1 (deg) = ", crota1, "  sgn(pas_ra*cos(crota1*!pi/180.)) = ",  sgn(pas_ra*cos(crota1*!pi/180.))
;print, "sgn(pas_ra*sin(crota1*!pi/180.)) = ",  sgn(pas_ra*sin(crota1*!pi/180.)) 

;if pas_ra lt 0 then begin
if pas_ra*cos(crota1*!pi/180.) ge 0. then begin
	pos_ini(0) = out_ima_dim - taille(1)*echant - offset/2
;	pos_ini(1) = 0 + offset/2
endif else begin
	pos_ini(0) = 0 + offset/2
;	pos_ini(1) = out_ima_dim - taille(1)*echant - offset/2
endelse

if pas_ra*sin(crota1*!pi/180.) ge 0. then begin
	pos_ini(1) = 0 + offset/2
endif else begin
	pos_ini(1) = out_ima_dim - taille(1)*echant - offset/2
endelse

echant = nint(datastr.cdelt1/pixelsize)
npixrebin = size_array*echant

delta_ra  = datastr.datapar.longoff*3600.			;  RA, Dec offsets (arcsec) from source as a function of time
delta_dec = datastr.datapar.latoff*3600.

rot_angle = datastr.crota1*!pi/180.	 				; radians
rot_draddec2dxdy, delta_ra, delta_dec, rot_angle, deltax, deltay

;
;  After rotation :  deltax, deltay are the offsets from the pointed source in array coordinates and arcsec as a function of time

deltax = deltax - deltax(0)					;  Offsets from initial position in array coordinates and arcsec
deltay = deltay - deltay(0)
 
posx = deltax/pixelsize + pos_ini(0)				;  Offsets in units of pixels in the "final" rebinned image
posy = deltay/pixelsize + pos_ini(1)

;if min(posx) lt 0 then posx = posx+1-min(posx)
;
;if min(posy) lt 0 then posy = posy+1-min(posy)

if min(posx)-npixrebin lt 0 then posx = posx+1-min(posx)+npixrebin

if min(posy)-npixrebin lt 0 then posy = posy+1-min(posy)+npixrebin

if min(posx) lt 0 or min(posy) lt 0 then out_ima_dim = out_ima_dim + nint(max([1-min(posx),1-min(posy)]))+1

if max(posx)+npixrebin-1 gt out_ima_dim-1  then out_ima_dim = out_ima_dim + max(posx)+npixrebin-out_ima_dim+1

if max(posy)+npixrebin-1 gt out_ima_dim-1 then out_ima_dim = out_ima_dim + max(posy)+npixrebin-out_ima_dim+1


;posx = indgen(taille(3)) 
;posy = indgen(taille(3))

;posx = posx*deltax/pixelsize + pos_ini(0)
;posy = posy*deltay/pixelsize + pos_ini(1)

;positions = nint([[posx], [posy]])
positions = [[posx], [posy]]

return
end
