pro set_pos_elev_cte, datastr, echant, positions, out_ima_dim

; .r /Users/artemis/Desktop/apex_idl/apexpro/set_pos_elev_cte

if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	SET_POS, datastr, pos_ini, pixelsize, elevation, delta_pos, position'
	return
endif

; pixelsize = taille du pixel en arcsec dans l'image re-echantillonnee
pixelsize = datastr.cdelt1/echant

; determination de la taille de l'image (re-echantillonnee) en sortie
taille = size(datastr.cube)
size_array = taille(1)
naz = n_elements(datastr.lst)
pas_az = datastr.pas_az						; pas elementaire en arcsec le long de la direction de scanning (ici Az)
champ_det = datastr.cdelt1 * taille(1)

; le facteur 3 ci-dessous est l'arrondi de 2*sqrt(2) pour prendre en compte le
; cas le plus defavorable d'une rotation a 45 deg

;out_ima_size = naz * pas_az + 3 * champ_det    ; en arcsec
out_ima_size = naz * abs(pas_az) + 2 * champ_det    ; en arcsec
;out_ima_size = (naz * pas_az + champ_det)    ; en arcsec

out_ima_dim = fix(out_ima_size / pixelsize) + 1
offset = fix(1.5*champ_det/pixelsize)	     ; marge pour les rotations -> RADEC

; Determination du pixel (0,0) dans l'image finale
pos_ini = lonarr(2)
;;;;if pas_az ge 0 then begin						; KOSMA March 2006
if pas_az lt 0 then begin						; APEX March 2007 & KOSMA Jan. 2007					
	pos_ini(0) = out_ima_dim - taille(1)*echant - offset/2
	pos_ini(1) = 0 + offset/2					; KOSMA and APEX !!
endif else begin
	pos_ini(0) = 0 + offset/2
	pos_ini(1) = out_ima_dim - taille(1)*echant - offset/2		; KOSMA and APEX !!
endelse

echant = nint(datastr.cdelt1/pixelsize)
npixrebin = size_array*echant

;
;command = datastr.obslog.command
;command = strmid(command,strpos(command,'angle='))
;if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
; angle = (strsplit(command,',',/extract))(0)
; angle =  float(strmid(angle,strpos(angle,'=')+1))		; Rotation angle (deg) of scanning direction relative to Az axis
;endif
;
;if ((size(angle))(1) gt 1) then begin			;   if angle is defined ...
;  if (angle ne 0.0) then begin
;    print, "Scanning at angle ", angle, " (deg) with respect to Azimuth axis"  
;    angle = angle*!pi/180.d0				;  angle in radians
;    delta_az = cos(angle)*pas_az
;    delta_el = sin(angle)*pas_az
;  endif else begin
;    delta_az = pas_az
;    delta_el = 0.
;  endelse
;endif else begin
;  delta_az = pas_az
;  delta_el = 0.
;endelse
;

delta_az = datastr.datapar.longoff*3600.			;  Az, El offsets (arcsec) from source as a function of time  (true angles)
delta_el = datastr.datapar.latoff*3600.

el0 = datastr.elevatio						;  Elevation in degrees
rot_dazdel2dxdy, delta_az, delta_el, el0, deltax, deltay
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
;if min(posx)-npixrebin lt 0 or min(posy)-npixrebin lt 0 then out_ima_dim = out_ima_dim + nint(max([1-min(posx),1-min(posy)]))+1+npixrebin

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
