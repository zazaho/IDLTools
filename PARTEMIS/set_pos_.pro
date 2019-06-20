;+
; NAME:
;	SET_POS_
;
; PURPOSE:
;
;	Set coordinates in units of pixels in the final rebinned image (array frame) for a given subscan
;
; CALLING SEQUENCE:
;
;	SET_POS_, Datastr, Echant=Echant, Positions, Out_ima_dim
;
; INPUTS:
;
;	Datastr:	Input data structure.
;	Echant:		Rebin factor.	
;
; KEYWORD PARAMETERS:
;
;	DO_RCP:		If distorsion files are used. 
;
; OUTPUTS:
;
;	Positions:	Coordinates in pixel image.
;	Out_ima_dim:	Rebined map dimensions.
;
; EXAMPLE:
;
;		SET_POS_, Datastr, Echant=3, Positions, Out_ima_dim, /DO_RCP
;
; MODIFICATION HISTORY:
;
;-


pro set_pos_, datastr, echant=echant, positions, out_ima_dim, do_rcp = do_rcp


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	SET_POS, datastr, echant=echant, positions, out_ima_dim, do_rcp = do_rcp'
	return
endif

; set coordinates in units of pixels in the "final" rebinned image

; indice de l'image du centre du scan
n_images = n_elements(datastr.datapar.azimuth)
id_ima_milieu = n_images/2
datastr.lst_mid = datastr.lst(id_ima_milieu)


datastr.alpha_mid = datastr.datapar(id_ima_milieu).baslong
datastr.delta_mid = datastr.datapar(id_ima_milieu).baslat

ha = datastr.lst_mid*15/3600. - datastr.alpha_mid
parangle = parallactic_angle(ha, datastr.delta_mid, site_lat=datastr.sitelat)
print, " parangle (deg) = ", parangle

;;;;;;crota1 = parangle + datastr.elevatio - 180.				 
crota1 = parangle + datastr.elevatio
datastr.crota1 = crota1							; SIGNE DE CROTA1 ?

ha_vec = datastr.lst*15/3600. - datastr.alpha_mid
parangle_vec = parallactic_angle(ha_vec, datastr.delta_mid, site_lat=datastr.sitelat)

datastr.rota = parangle_vec + datastr.datapar.elevatio



; pixelsize = taille du pixel en arcsec dans l'image re-echantillonnee
pixelsize = datastr.cdelt1/echant

; determination de la taille de l'image (re-echantillonnee) en sortie
taille = size(datastr.cube)
size_array = taille(1)

if datastr.scandir eq 'RA' then begin

nra = n_elements(datastr.lst)
pas_ra = datastr.pas_az			; pas elementaire le long de la direction de scanning (ici RA)

endif
	
if datastr.scandir eq 'ALON' or datastr.scantype eq 'POINT' then begin

naz = n_elements(datastr.lst)
pas_az = datastr.pas_az		

endif
				; pas elementaire en arcsec le long de la direction de scanning (ici Az)
champ_det = datastr.cdelt1 * taille(1)

; le facteur 3 ci-dessous est l'arrondi de 2*sqrt(2) pour prendre en compte le
; cas le plus defavorable d'une rotation a 45 deg

;out_ima_size = naz * pas_az + 3 * champ_det    ; en arcsec

;if datastr.scandir eq 'ALON' or datastr.scantype eq 'POINT' then begin

;out_ima_size = naz * abs(pas_az) + 2 * champ_det    ; en arcsec
 
;endif

;if datastr.scandir eq 'RA' then begin

;out_ima_size = nra * abs(pas_ra) + 2 * champ_det    ; en arcsec

;endif

ind = where (datastr.datapar.longoff ne -999. and datastr.datapar.latoff ne -999., cnt0)

if cnt0 gt 0 then begin
  longoff_ext = max(datastr.datapar(ind).longoff*3600.)-min(datastr.datapar(ind).longoff*3600.)      ; arcsec

  latoff_ext = max(datastr.datapar(ind).latoff*3600.)-min(datastr.datapar(ind).latoff*3600.)	; arcsec

endif else begin
  
  print, "ERROR (set_pos_): longoff and latoff are undefined (-999)"
  return

endelse 

out_ima_size = max(longoff_ext, latoff_ext) + champ_det    ; arcsec

;
;

;out_ima_size = (naz * pas_az + champ_det)    ; en arcsec

out_ima_dim = fix(out_ima_size / pixelsize) + 1
offset = fix(1.5*champ_det/pixelsize)	     ; marge pour les rotations -> RADEC

; Determination du pixel (0,0) dans l'image finale
pos_ini = lonarr(2)
;;;;if pas_az ge 0 then begin						; KOSMA March 2006

if datastr.scandir eq 'ALON' or datastr.scantype eq 'POINT' then begin

   if pas_az lt 0 then begin						; APEX March 2007 & KOSMA Jan. 2007					
	  pos_ini(0) = out_ima_dim - taille(1)*echant - offset/2
	  pos_ini(1) = 0 + offset/2					; KOSMA and APEX !!
   endif else begin
	  pos_ini(0) = 0 + offset/2
	  pos_ini(1) = out_ima_dim - taille(1)*echant - offset/2		; KOSMA and APEX !!
   endelse

endif

if datastr.scandir eq 'RA' then begin

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



endif


echant = nint(datastr.cdelt1/pixelsize)
npixrebin = size_array*echant

;id = where(datastr.lst ne -999., cnt)				; eliminating data with unknown telescope positions

;;if datastr.scandir eq 'ALON' or datastr.scantype eq 'POINT' then begin

if datastr.ctype1 eq 'ALON-GLS' or datastr.ctype2 eq 'ALAT-GLS' then begin

delta_az = datastr.datapar.longoff*3600.			;  Az, El offsets (arcsec) from source as a function of time  (true angles)
delta_el = datastr.datapar.latoff*3600.

delta_az = delta_az(ind)
delta_el = delta_el(ind)

el0 = datastr.elevatio	

					;  Elevation in degrees
rot_dazdel2dxdy, delta_az, delta_el, el0, deltax, deltay


endif

;if datastr.scandir eq 'RA' or datastr.scandir eq 'DEC' then begin

if datastr.ctype1 eq 'RA---GLS' or datastr.ctype2 eq 'DEC--GLS' then begin

delta_ra  = datastr.datapar.longoff*3600.			;  RA, Dec offsets (arcsec) from source as a function of time
delta_dec = datastr.datapar.latoff*3600.

delta_ra  = delta_ra(ind)
delta_dec = delta_dec(ind)

rot_angle = datastr.crota1*!pi/180.	 				; radians
rot_draddec2dxdy, delta_ra, delta_dec, rot_angle, deltax, deltay


endif

;
;  After rotation :  deltax, deltay are the offsets from the pointed source in array coordinates and arcsec as a function of time

deltax = deltax - deltax(0)					;  Offsets from initial position in array coordinates and arcsec
deltay = deltay - deltay(0)
 
posx = deltax/pixelsize + pos_ini(0)				;  Offsets in units of pixels in the "final" rebinned image
posy = deltay/pixelsize + pos_ini(1)


if keyword_set(do_rcp) eq 1 then begin
  if min(posx)-npixrebin lt 0 then posx = posx+1-min(posx)+npixrebin

  if min(posy)-npixrebin lt 0 then posy = posy+1-min(posy)+npixrebin
endif else begin
  if min(posx) lt 0 then posx = posx+1-min(posx)

  if min(posy) lt 0 then posy = posy+1-min(posy)
endelse

if min(posx) lt 0 or min(posy) lt 0 then out_ima_dim = out_ima_dim + nint(max([1-min(posx),1-min(posy)]))+1

if max(posx)+npixrebin-1 gt out_ima_dim-1  then out_ima_dim = out_ima_dim + max(posx)+npixrebin-out_ima_dim+1

if max(posy)+npixrebin-1 gt out_ima_dim-1 then out_ima_dim = out_ima_dim + max(posy)+npixrebin-out_ima_dim+1

;posx = indgen(taille(3)) 
;posy = indgen(taille(3))

;posx = posx*deltax/pixelsize + pos_ini(0)
;posy = posy*deltay/pixelsize + pos_ini(1)

positions = [[posx], [posy]]

return
end
