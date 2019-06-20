;+
; NAME:
;	MAKE_MODEL_DATA
;
; PURPOSE:
;
;	Build a data cube for a given observation from a 2D source model.
;
; CALLING SEQUENCE:	
; 
;	Donnees_mod= MAKE_MODEL_DATA(Scan_str, Model_str)
;
; INPUTS:
;
;	Scan_str:	Input data structure.
;		
; OUTPUTS:
;
;	Model_str:	2D model output structure.
;
; EXAMPLE:
;
;		Donnees_mod= MAKE_MODEL_DATA(Donnees_obs, Model_str)
;
; MODIFICATION HISTORY:
;
;-

function make_model_data, scan_str, model_str, dx_rcp=dx_rcp, dy_rcp=dy_rcp


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	Donnees_mod= MAKE_MODEL_DATA(Donnees_obs, Model_str)'
	return, -1
endif	

; Construit un cube de donnees a partir du modele
; Retourne donnees_mod

donnees_mod = scan_str

model_image = model_str.image
;wmin = 0.2*max(model_str.weight)
;id_cut = where(model_str.image lt 1.5 or model_str.weight lt wmin, count)     ;   Typical rms in model_image ~ 1.5 Jy/beam
;if count gt 0 then model_image(id_cut) = 0.

taille = size(scan_str.cube)

linx = indgen(taille(1))
liny = indgen(taille(2))

taille(3) = min([taille(3),(size(scan_str.datapar))(3)])

; 7.5 est le milieu de la matrice
;nx_pix0 = taille(1)/2-0.5
;ny_pix0 = taille(2)/2-0.5
;
; (7,7) (<--> 120) est le pixel de reference pour le pointage
nx_pix0 = taille(1)/2-1.
ny_pix0 = taille(2)/2-1.
;
;
dx = ((linx-nx_pix0)*scan_str.cdelt1)#(intarr(taille(1))+1)	    	       ;   arcsec 
dy = transpose(((liny-ny_pix0)*scan_str.cdelt2)#(intarr(taille(2))+1))
;

;init_obs, scan_number=scan_str.scannum,  type = 'map', init_obs_str
;dx_rcp=init_obs_str.dx_rcp 
;dy_rcp=init_obs_str.dy_rcp
if keyword_set(dx_rcp) and keyword_set(dy_rcp) then begin
;
	goodpix_ima = scan_str.goodpix_ima
	ind_bad = where(goodpix_ima eq 0, cnt)
;	
	if cnt gt 0 then begin
 		dx_rcp(ind_bad) = dx(ind_bad)
 		dy_rcp(ind_bad) = dy(ind_bad) 
	endif
;
	dx = dx_rcp
	dy = dy_rcp
endif
;
;
IF (size(model_str))(2) gt 1 THEN BEGIN

 print, 'Using make_model'

 cube_alpha = scan_str.cube*0.
 cube_delta = scan_str.cube*0.
;
 rot_angle = scan_str.crota1*!pi/180.	 		; radians
 rot_dxdy2draddec, double(dx), double(dy), rot_angle, dalpha, ddelta       	; arcsec
;
 dalpha = dalpha/3600.d0		; true angle in deg
 ddelta = ddelta/3600.d0
;
 for i = 0, taille(3)-1 do begin
;	cube_alpha(*,*,i) = scan_str.datapar(i).baslong + dalpha						;  deg
	cube_alpha(*,*,i) = scan_str.datapar(i).baslong + dalpha/cos(scan_str.blatobj*!pi/180.)			;  angle on sphere in deg
	cube_delta(*,*,i) = scan_str.datapar(i).baslat  + ddelta
 endfor
;
;;;;
;;;;     POINTING CORRECTIONS :
;;;;
	 if strmid(scan_str.filename,5,4) eq '4806' then begin
	    cube_alpha = cube_alpha - 13./3600.					; deg
	    cube_delta = cube_delta - 12./3600.
	    print, "Scan ", strmid(scan_str.filename,5,4), "  Pointing corr. (arcsec) : -13, -12"
	 endif
;	 
	 if strmid(scan_str.filename,5,4) eq '4832' then begin
	    cube_alpha = cube_alpha - 4.4/3600.					; deg
	    cube_delta = cube_delta - 7.6/3600.
	    print, "Scan ", strmid(scan_str.filename,5,4), "  Pointing corr. (arcsec) : -4.4, -7.6"	    
	 endif	  
; 
; cube_x_alpha = nint((cube_alpha-model_str.alpha_ref)/model_str.cdelt1 + model_str.crpix1)	; pixels du modele 
 cube_x_alpha = nint((cube_alpha-model_str.alpha_ref)*cos(scan_str.blatobj*!pi/180.)/model_str.cdelt1 + model_str.crpix1)	; true angle in units of pixels (model)
 cube_y_delta = nint((cube_delta-model_str.delta_ref)/model_str.cdelt2 + model_str.crpix2)
;
 cube_model = scan_str.cube * 0
; cube_model = model_image(cube_x_alpha,cube_y_delta)

 cube_model = interpolate(model_image,cube_x_alpha,cube_y_delta)

ENDIF

donnees_mod.cube = cube_model

;filename='/Users/artemis/Documents/apexdata/masques/' + scan_str.filename + '.xdr'
;save, filename=filename, cube_model

return, donnees_mod
end
