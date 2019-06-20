;+
; NAME:
;	LOCATE_SIGNAL
;
; PURPOSE:
;
;	Build a mask.
;	
; CALLING SEQUENCE:
;
;	Cube_masque = LOCATE_SIGNAL(Scan_str, Masque_base_str)
;
; INPUTS:
;
;	Scan_str:	Input data structure
;
; OPTIONAL INPUT:
;
;	Masque_base_str:	Input mask structure..
;
; OUTPUT:
;
;	Cube_mask:	Output mask cube
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration name
;
; EXAMPLE:
;
;		Cube_masque = LOCATE_SIGNAL(Scan_str, Masque_base_str)
;
; MODIFICATION HISTORY:
;
;-

function locate_signal, scan_str, masque_base_str


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	Cube_masque= LOCATE_SIGNAL(Scan_str, Masque_base_str)'
	return, -1
endif	


; Construit un cube de masques valant 1 sur la source et 0 ailleurs
; Retourne cube_mask

COMMON obs1_configb

date=strsplit(scan_str.date_obs,'T',/extract)
date=date[0]
date=strsplit(date,'-',/extract)
date=fix(date[0])

taille  = size(scan_str.cube)
taille1 = size(scan_str.datapar)
n_images = min([taille(3),taille1(3)])

cube_mask = scan_str.cube * 0

linx = indgen(taille(1))
liny = indgen(taille(2))

; 7.5 est le milieu de la matrice P-ArTeMiS

; (7,7) (<--> 120) est le pixel de reference pour le pointage
nx_pix0 = taille(1)/2-1.
ny_pix0 = taille(2)/2-1.


dx = ((linx-nx_pix0)*scan_str.cdelt1)#(intarr(taille(1))+1)	    	       ;   arcsec 
dy = transpose(((liny-ny_pix0)*scan_str.cdelt2)#(intarr(taille(2))+1))

IF (size(masque_base_str))(2) gt 1 THEN BEGIN

 print, 'Using masque_base_str'

 cube_alpha = scan_str.cube*0.
 cube_delta = scan_str.cube*0.


 rot_angle = scan_str.crota1*!pi/180.	 		; radians
 rot_dxdy2draddec, double(dx), double(dy), rot_angle, dalpha, ddelta       	; arcsec

 dalpha = dalpha/3600.d0		; true angle in deg
 ddelta = ddelta/3600.d0


        for i = 0, n_images-1 do begin
	    cube_alpha(*,*,i) = scan_str.datapar(i).baslong + dalpha/cos(scan_str.blatobj*!pi/180.)	           ; angle on sphere in deg
	    cube_delta(*,*,i) = scan_str.datapar(i).baslat  + ddelta
        endfor

;;;;
;;;;     POINTING CORRECTIONS :
;;;;
	 if strmid(scan_str.filename,5,4) eq '4806'  and date le 2007  then begin
	    cube_alpha = cube_alpha - 13./3600.					; deg
	    cube_delta = cube_delta - 12./3600.
	    print, "Scan ", strmid(scan_str.filename,5,4), "  Pointing corr. (arcsec) : -13, -12"
	 endif
	 
	 if strmid(scan_str.filename,5,4) eq '4832'  and date le 2007  then begin
	    cube_alpha = cube_alpha - 4.4/3600.					; deg
	    cube_delta = cube_delta - 7.6/3600.
	    print, "Scan ", strmid(scan_str.filename,5,4), "  Pointing corr. (arcsec) : -4.4, -7.6"	    
	 endif


 cube_x_alpha = nint((cube_alpha-masque_base_str.alpha_ref)*cos(scan_str.blatobj*!pi/180.)/masque_base_str.cdelt1 + masque_base_str.crpix1)      ; true angle in units of pixels masque 
 cube_y_delta = nint((cube_delta-masque_base_str.delta_ref)/masque_base_str.cdelt2 + masque_base_str.crpix2)

 cube_mask = masque_base_str.image(cube_x_alpha,cube_y_delta)

ENDIF

IF (size(masque_base_str))(2) le 1 THEN BEGIN					; masque_base_str is undefined

 cube_dist = scan_str.cube * 0

; (pointing_off_az, pointing_off_el) sont les pointing offsets en Az, El (arcsec) si scandir ne 'RA'

 pointing_off_az = 0.
 pointing_off_el = 0.	 

 if strupcase(scan_str.source) eq 'MOON' then begin
	rayon = 0.375			; degres pour la Lune
 endif else if strupcase(scan_str.source) eq 'SUN' then begin	
;	rayon = 0.458			; degres pour le Soleil	(en comptant le lobe d'erreur)
	rayon = 0.41 			; degres pour le Soleil
	if date le 2007  then begin
	pointing_off_az =  1.66*60.	; 
	pointing_off_el = -3.85*60.	;  arcsec
	endif
 endif else if strupcase(scan_str.source) eq 'VENUS' and date le 2007  then begin
	rayon = 0.025
 endif else if strupcase(scan_str.source) eq 'VENUS' and date ge 2009  then begin
	rayon = 2.*(27./2./3600.)	
 endif else if strupcase(scan_str.source) eq 'SATURN' then begin
;	rayon = 56./2./3600.
	rayon = 30./2./3600.
     if date le 2007 then begin 
     pointing_off_az = 0.
     endif			
;	pointing_off_el = +10		; Saturn otf map # 4260 (scandir eq 'RA')
 endif else if strupcase(scan_str.source) eq 'JUPITER' and date le 2007 then begin
	rayon = 50./2./3600.
 endif else if strupcase(scan_str.source) eq 'MARS' and date le 2007 then begin
	rayon = 75./2./3600.	
 endif else begin
;	rayon = 0.010
	rayon = 15./3600.
 endelse

 if strmid(strupcase(scan_str.filename),0,9) eq 'APEX-4792' and date le 2007 then begin		   ; Jupiter # 4792
	rayon = 80./3600.		; deg
	pointing_off_az = -1.5 		; arcsec
	pointing_off_el = +8.4		; arcsec	
 endif else if strmid(strupcase(scan_str.filename),0,9) eq 'APEX-4826' and date le 2007 then begin	   ; Saturn # 4826
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

 for i=0L, n_images - 1 do begin

;  if scan_str.scandir ne 'RA' then begin			;  Scanning in Az

  if scan_str.ctype1 eq 'ALON-GLS' or scan_str.ctype2 eq 'ALAT-GLS' then begin				; longoff & latoff <--> Az,El offsets
	;
	; delta_az0 et delta_el0 sont les offsets Az El du centre de la matrice (en arcsec) a l'intant i 
	; par rapport a la source pointee
	;
	delta_az0 = scan_str.datapar(i).longoff*3600.
	delta_el0 = scan_str.datapar(i).latoff*3600.

	rot_dxdy2dazdel, dx, dy, elev, delta_az, delta_el       ; arcsec

  endif else begin						; (e.g. Scanning in RA)			; longoff & latoff <--> RA, Dec offsets
	;
	; delta_az0 et delta_el0 sont les offsets RA DEC (!!) du centre de la matrice  (en arcsec) a l'intant i 
	; par rapport a la source pointee	
	;
	delta_az0 = scan_str.datapar(i).longoff*3600.
	delta_el0 = scan_str.datapar(i).latoff*3600.	

	rot_angle = scan_str.crota1*!pi/180.	 		; radians
	rot_dxdy2draddec, dx, dy, rot_angle, delta_az, delta_el       ; arcsec

  endelse


;  delta_az et delta_el sont des tableaux donnant les offsets (en arcsec) des differents pixels (en arcsec) de la matrice a l'instant i 
;  par rapport a la source pointee

  delta_az = delta_az + delta_az0
  delta_el = delta_el + delta_el0 
  

; (pointing_off_az, pointing_off_el) sont les pointing offsets en Az, El (arcsec)

  delta_az = delta_az + pointing_off_az
  delta_el = delta_el + pointing_off_el
 

  dist = sqrt(delta_az^2+delta_el^2)/3600.		; en degres
  cube_dist(*,*,i) = dist
  masque = dist*0.
  id = where(dist lt rayon, count)
    if count ne 0 then masque(id) = 1
  cube_mask(*,*,i) = masque

 endfor

ENDIF 

;
; Mise à 1 (sur source) du masque lorsque les positions ne sont pas connues
;
idn = where(scan_str.datapar.longoff eq -999. or scan_str.datapar.latoff eq -999.,cnt)
if (cnt gt 0) then cube_mask(*,*,idn) = 1

print, "Max(cube_mask) = ", max(abs(cube_mask))


;apexdata = '/Users/andre/kosma/artemis/apexdata/'
apexdata = work_dir+'apexdata/'


filename= apexdata + 'masques/' + scan_str.filename + '.xdr'



return, cube_mask
end
