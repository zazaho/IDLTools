;+
; NAME:
;	LOCATE_DUAL_BEAM
;
; PURPOSE:
;
;	Build a cube of mask, put the value 1 on the restaured(SAA mode) source :
;	both negative and positive parts, put 0 elsewhere, modify goodpix_ima
;	so that array pixels that see the source only in negative or positive before
;	restauration be set to 0.
;	
; CALLING SEQUENCE:
;	
;	Cube_masque= LOCATE_DUAL_BEAM(Scan_str, Masque_base_str)
;
; INPUTS:
;
;	Scan_str:		Input data structure.
;	Masque_base_Str:	Input mask structure.	
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project, and calibration name.
;		
; EXAMPLE:
;
;		Cube_masque= LOCATE_DUAL_BEAM(Scan_str, Masque_base_str)
;
; MODIFICATION HISTORY:
;
;-

function locate_dual_beam, scan_str, masque_base_str


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	Cube_masque= LOCATE_DUAL_BEAM(Scan_str, Masque_base_str)'
	return, -1
endif	


COMMON obs1_configb

;
;  .r /Users/artemis/Desktop/apex_idl/apexpro/locate_signal.pro
;
; Construit un cube de masques valant 1 sur la source restauree en mode saa (parties positive et negatives) et 0 ailleurs
; Retourne cube_mask
; Modifie scan_str.goodpix_ima  ---> 0 pour tous les pixels de la matrice qui ne voient la source qu'en positif ou negatif avant restauration
;

date=strsplit(scan_str.date_obs,'T',/extract)
date=date[0]
date=strsplit(date,'-',/extract)
date=fix(date[0])

taille  = size(scan_str.cube)
taille1 = size(scan_str.datapar)
n_images = min([taille(3),taille1(3)])

cube_mask = scan_str.cube * 0
cube_mask_pos = scan_str.cube * 0
cube_mask_neg = scan_str.cube * 0

linx = indgen(taille(1))
liny = indgen(taille(2))

; 7.5 est le milieu de la matrice P-ArTeMiS
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


;;;IF (size(masque_base_str))(2) le 1 THEN BEGIN					; masque_base_str is undefined
;
; (pointing_off_az, pointing_off_el) sont les pointing offsets en Az, El (arcsec) si scandir ne 'RA'

 pointing_off_az = 0.
 pointing_off_el = 0.	 

 if strupcase(scan_str.source) eq 'MOON' and date le 2007 then begin
	rayon = 0.375			; degres pour la Lune
 endif else if strupcase(scan_str.source) eq 'SUN' and date le 2007 then begin	
;	rayon = 0.458			; degres pour le Soleil	(en comptant le lobe d'erreur)
	rayon = 0.41 			; degres pour le Soleil
	pointing_off_az =  1.66*60.	; 
	pointing_off_el = -3.85*60.	;  arcsec
 endif else if strupcase(scan_str.source) eq 'VENUS' and date le 2007 then begin
	rayon = 0.025 
 endif else if strupcase(scan_str.source) eq 'SATURN'  and date le 2007 then begin
;	rayon = 56./2./3600.
	rayon = 30./2./3600.
	pointing_off_az = 0.			
;	pointing_off_el = +10		; Saturn otf map # 4260 (scandir eq 'RA')
 endif else if strupcase(scan_str.source) eq 'JUPITER' and date le 2007 then begin
	rayon = 50./2./3600.
 endif else begin
;	rayon = 0.010
	rayon = 15./3600.
 endelse

 if strmid(strupcase(scan_str.filename),0,9) eq 'APEX-4792' and date le 2007 then begin	; Jupiter # 4792
	rayon = 80./3600.		; deg
	pointing_off_az = -1.5 		; arcsec
	pointing_off_el = +8.4		; arcsec	
 endif else if strmid(strupcase(scan_str.filename),0,9) eq 'APEX-4826' and date le 2007  then begin	   ; Saturn # 4826
;	rayon = 30./3600.		; deg
	rayon = 25./3600.		; deg
;	rayon = 10./3600.		; deg
	pointing_off_az = -15. 		; arcsec
	pointing_off_el = -4.2		; arcsec
 endif

 ;	 
 if (strmid(scan_str.filename,5,5) eq '47928' or strmid(scan_str.filename,5,5) eq '48139') and date le 2007 then begin
 	pointing_off_az = 0. ; -scan_str.chopamp*3600.
 	pointing_off_el = 0.
	rayon = 20./3600.		; deg
; 	print, "Scan ", strmid(scan_str.filename,5,5), " Az, El Pointing corr. (arcsec) : ", pointing_off_az, pointing_off_el
 endif


 print, 'Source radius (arcsec) : ', rayon*3600.
 print, 'Pointing offsets (arcsec) : ', pointing_off_az, pointing_off_el

 elev = scan_str.elevatio

 for i=0, n_images - 1 do begin

  if scan_str.scandir eq 'ALON' then begin			;  Scanning in Az
	;
	; delta_az0 et delta_el0 sont les offsets Az El du centre de la matrice (en arcsec) a l'intant i 
	; par rapport a la source pointee
	;
	delta_az0 = scan_str.datapar(i).longoff*3600.
	delta_el0 = scan_str.datapar(i).latoff*3600.
; 
	rot_dxdy2dazdel, dx, dy, elev, delta_az, delta_el       ; arcsec
;
  endif else begin						; Scanning in RA
	print, "LOCATE_DUAL_BEAM ERROR: Scanning is not in Azimuth ???"
  endelse

;
;  delta_az et delta_el sont des tableaux donnant les offsets (en arcsec) des differents pixels (en arcsec) de la matrice a l'instant i 
;  par rapport a la source pointee
;
  delta_az = delta_az + delta_az0
  delta_el = delta_el + delta_el0 
  
;
; (pointing_off_az, pointing_off_el) sont les pointing offsets en Az, El (arcsec)

  delta_az = delta_az + pointing_off_az
  delta_el = delta_el + pointing_off_el
;
  delta_az_pos = delta_az + scan_str.chopamp*3600.
  delta_az_neg = delta_az - scan_str.chopamp*3600.
;  
  delta_az_neg1_saa = delta_az + 2.*scan_str.chopamp*3600.
  delta_az_neg2_saa = delta_az - 2*scan_str.chopamp*3600.  
;
  dist = sqrt(delta_az^2+delta_el^2)/3600.			; en degres

  dist_pos = sqrt(delta_az_pos^2+delta_el^2)/3600.		; en degres
  dist_neg = sqrt(delta_az_neg^2+delta_el^2)/3600.		; en degres  
  
  dist_neg1 = sqrt(delta_az_neg1_saa^2+delta_el^2)/3600.	; en degres
  dist_neg2 = sqrt(delta_az_neg2_saa^2+delta_el^2)/3600.	; en degres
      
  masque = dist*0
  id = where(dist lt rayon or dist_neg1 lt rayon or dist_neg2 lt rayon, count)
    if count ne 0 then masque(id) = 1
  cube_mask(*,*,i) = masque
  
  masque = masque*0
  id_pos = where(dist_pos lt rayon, count)
    if count ne 0 then masque(id_pos) = 1
  cube_mask_pos(*,*,i) = masque

  masque = masque*0
  id_neg = where(dist_neg lt rayon, count)
    if count ne 0 then masque(id_neg) = 1
  cube_mask_neg(*,*,i) = masque   

 endfor

;;;ENDIF

IF (size(masque_base_str))(2) gt 1 THEN BEGIN

 print, "LOCATE_DUAL_BEAM WARNING: Does not work with masque_base yet "
 return, cube_mask

 print, 'Using masque_base_str'

 cube_alpha = scan_str.cube*0.
 cube_delta = scan_str.cube*0.
;
 rot_angle = scan_str.crota1*!pi/180.	 		; radians
 rot_dxdy2draddec, double(dx), double(dy), rot_angle, dalpha, ddelta       	; arcsec
;
 dalpha = dalpha/3600.d0		; true angle in deg
 ddelta = ddelta/3600.d0
;
; for i = 0, taille(3)-1 do begin
for i = 0, n_images-1 do begin
;	cube_alpha(*,*,i) = scan_str.datapar(i).baslong + dalpha
	cube_alpha(*,*,i) = scan_str.datapar(i).baslong + dalpha/cos(scan_str.blatobj*!pi/180.)	                ; angle on sphere in deg
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
; cube_x_alpha = nint((cube_alpha-masque_base_str.alpha_ref)*/masque_base_str.cdelt1 + masque_base_str.crpix1)					; pixels masque
 cube_x_alpha = nint((cube_alpha-masque_base_str.alpha_ref)*cos(scan_str.blatobj*!pi/180.)/masque_base_str.cdelt1 + masque_base_str.crpix1)      ; true angle in units of pixels masque 
 cube_y_delta = nint((cube_delta-masque_base_str.delta_ref)/masque_base_str.cdelt2 + masque_base_str.crpix2)
;
 cube_mask = masque_base_str.image(cube_x_alpha,cube_y_delta)

ENDIF

;print, "Max(cube_mask) = ", max(abs(cube_mask))
;print, "Max(cube_mask_pos) = ", max(abs(cube_mask_pos))
;print, "Max(cube_mask_neg) = ", max(abs(cube_mask_neg))

for j = 0, taille(2)-1 do begin
  for i = 0, taille(1)-1 do begin
    if max(cube_mask_pos(i,j,*)) eq 1 and max(cube_mask_neg(i,j,*)) eq 0 then begin
	scan_str.goodpix_ima(i,j) = 0
	print, "LOCATE_DUAL_BEAM: Flagging pixel ", i, j
    endif	
    if max(cube_mask_pos(i,j,*)) eq 0 and max(cube_mask_neg(i,j,*)) eq 1 then begin
	scan_str.goodpix_ima(i,j) = 0
	print, "LOCATE_DUAL_BEAM: Flagging pixel ", i, j	
    endif
    if cube_mask_pos(i,j,0) eq 1 or cube_mask_pos(i,j,n_images-1) eq 1 or cube_mask_neg(i,j,0) eq 1 or cube_mask_neg(i,j,n_images-1) eq 1 then begin
	scan_str.goodpix_ima(i,j) = 0
	print, "LOCATE_DUAL_BEAM: Flagging pixel ", i, j    
    endif
  endfor
endfor

;;apexdata = '/mnt/local/home/pandre/partemis/apexdata/'
apexdata = work_dir + 'apexdata/'
;apexdata = '/Users/artemis/Documents/apexdata/

;;;filename='/Users/artemis/Documents/apexdata/masques/' + scan_str.filename + '.xdr'
filename= apexdata + 'masques/' + scan_str.filename + 'dual_beam.xdr'

;save, filename=filename, cube_mask

return, cube_mask
end
