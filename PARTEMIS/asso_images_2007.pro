
;+
; NAME:
;	ASSO_IMAGES_2007
;
; PURPOSE:
;
;	Return data cube and noise rms after averaging and subtracting signal 
;	for each period (when te wobbler is used).		
;
; CALLING SEQUENCE:
;	
;	ASSO_IMAGES_2007, DONNEES, DIFF_ASSO, ERROR
;
; INPUTS:
;
;	Donnees:	Input data structure.
;			
; OUTPUTS:
;
;	Diff_asso:	Output data structure.
;	Error:		Output error (integer).
;	
; EXAMPLE:	
;
;		ASSO_IMAGES_2007, Donnees, Diff_asso, Error
;
; MODIFICATION HISTORY:
; 				
;			
;-


pro asso_images_2007, donnees, diff_asso, error



if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	ASSO_IMAGES_2007, Donnees, Diff_asso, Error
	return
endif	

cube = donnees.cube
taille = size(cube)
error = 0
diff_asso = donnees

chopbeam = donnees.chopbeam

IF (donnees.wobused eq 1) THEN BEGIN   ;  Dual-beam case --> Work on phase differences

; chopbeam = vecteur des positions : 1 = beam A, -1 = beam B, 10 = rejete
; debut = 1er indice du groupe dans le cube
; fin   = dernier indice du groupe dans le cube

ind = where(chopbeam eq 1, count)
;if (count gt 0) then begin
;  chopbeam = chopbeam(min(ind):*)
;endif else begin
if (count eq 0) then begin
  print, "WOBBLER ERROR : Never on +1 phase"
  error = -1
  return
endif  
;endelse
difference = chopbeam(1:*) - chopbeam(0:n_elements(chopbeam)-2)
debut = where(difference lt 0) + 1
;;if difference(0) eq 0 then debut = [0, debut]
fin = where(difference gt 0)


if fin(0) lt debut(0) then fin = fin(1:*)
if (debut(n_elements(debut)-1) eq (n_elements(chopbeam)-1)) then begin
	debut = debut(0:n_elements(debut)-2)
endif
;endif else begin
;	fin = [fin, n_elements(chopbeam)-1]
;endelse

if fin(n_elements(fin)-1) lt debut(n_elements(debut)-1) then begin
	fin = [fin, n_elements(chopbeam)-1]
endif

if n_elements(fin) ne n_elements(debut) then begin
   	print, "Warning: n_elements(fin) = ", n_elements(fin), "  differs from n_elements(debut) = ", n_elements(debut)
endif

;help, debut
;help, fin

;count = 1
id = where (debut eq fin, count)

while count gt 0 do begin
  id = where (debut eq fin, count)
  if count gt 0 then begin
  	print, "Warning: Skipping data with transient wobbler position"
; 	print, "ASSO_IMAGES_2007 ERROR: Data with transient wobbler position in subscan ", donnees.subscan
;	error = -1
;        return
	
  	if id(0) gt 0 then begin
	    if id(0) lt n_elements(debut)-1  then begin
  		debut = [debut(0:id(0)-1),debut(id(0)+1:*)]
		fin = [fin(0:id(0)-1),fin(id(0)+1:*)]
	    endif else begin
  		debut = debut(0:id(0)-1)
		fin = fin(0:id(0)-1)
	    endelse	
	endif else begin	
  		debut = debut(id(0)+1:*)
		fin = fin(id(0)+1:*)	
	endelse	
  endif
endwhile

;help, debut
;help, fin 

;print, "Chopbeam at start of subscan ", chopbeam

; nombre de groupes :
n_groupes = min([n_elements(debut), n_elements(fin)])

;print, "n_groupes : ", n_groupes

if (n_groupes lt 4) then begin
  print, "WOBBLER ERROR : Insufficient number of wobbler cycles"
  print, "n_groupes : ", n_groupes
  error = -1
  return
endif

cube_moy = fltarr(taille(1),taille(2),n_groupes)
cube_rms = cube_moy
cube_dif = fltarr(taille(1),taille(2),n_groupes/2)
cube_dif_rms = cube_dif
phase_dif = intarr(n_groupes/2)
lst_dif = double(phase_dif*0.)
lst = dblarr(n_groupes)
rota = dblarr(n_groupes)
longoff = dblarr(n_groupes)
latoff = dblarr(n_groupes)
baslong = dblarr(n_groupes)
baslat = dblarr(n_groupes)
baslong_dif = double(phase_dif*0.)
baslat_dif = double(phase_dif*0.)
longoff_dif = double(phase_dif*0.)
latoff_dif = double(phase_dif*0.)
rota_dif = double(phase_dif*0.)

ENDIF ELSE BEGIN			;  Total-power case --> Work on averaged data

; Determination du nombre de groupes :

delta_t = 1./donnees.freq				;  e.g. 0.025 sec  periode elementaire des images (en sec)
; pas_az = donnees.otfvlam * delta_t     			;  pas en arcsec pendant une periode elementaire
pas_az = sgn(donnees.pas_az)*donnees.otfvlam * delta_t     	;  pas en arcsec pendant une periode elementaire
n_ima_gr = fix(abs(donnees.cdelt1)/2./pas_az)		;  nb d'images pour un deplacement de 1/2 pixel

n_groupes = taille(3)/n_ima_gr

cube_moy = fltarr(taille(1),taille(2),n_groupes)
cube_rms = cube_moy
lst = dblarr(n_groupes)
longoff = dblarr(n_groupes)
latoff = dblarr(n_groupes)
rota = dblarr(n_groupes)
baslong = dblarr(n_groupes)
baslat = dblarr(n_groupes)

debut = lindgen(n_groupes)*n_ima_gr
fin = lindgen(n_groupes)*n_ima_gr

ENDELSE


;  CALCUL DES MOYENNES ET DES RMS SUR LES GROUPES

;for i=0, n_elements(debut)-1 do begin
for i=0, n_groupes-1 do begin
	cube_moy(*,*,i) = extract_ima_moy(donnees.cube(*,*,debut(i):fin(i)), ima_rms)
	cube_rms(*,*,i) = ima_rms
	lst(i) = (donnees.lst(debut(i))+donnees.lst(fin(i)))/2.
	longoff(i) = median(donnees.datapar(debut(i):fin(i)).longoff)
	latoff(i) = median(donnees.datapar(debut(i):fin(i)).latoff)
	baslong(i) = median(donnees.datapar(debut(i):fin(i)).baslong)
	baslat(i) = median(donnees.datapar(debut(i):fin(i)).baslat)	
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF (donnees.wobused eq 1) THEN BEGIN   ;  Dual-beam case --> Work on phase differences

;for i=0, n_elements(debut)/2-1 do begin
for i=0, n_groupes/2-1 do begin
	cube_dif(*,*,i) = cube_moy(*,*,2*i)-cube_moy(*,*,2*i+1)
	cube_dif_rms(*,*,i) = sqrt(cube_rms(*,*,2*i)^2+cube_rms(*,*,2*i+1)^2)
	phase_dif(i) = chopbeam(debut(2*i))
	lst_dif(i) = (lst(2*i) + lst(2*i+1))/2.
	longoff_dif(i) = (longoff(2*i) + longoff(2*i+1))/2.
	latoff_dif(i) = (latoff(2*i) + latoff(2*i+1))/2.
	baslong_dif(i) = (baslong(2*i) + baslong(2*i+1))/2.
	baslat_dif(i) = (baslat(2*i) + baslat(2*i+1))/2.	
endfor

;delta_t = dlst2dtloc((lst_dif(1) - lst_dif(0)))  ;  periode du wobbler
delta_t = donnees.wobcycle			  ;  periode du wobbler
;pas_az = donnees.otfvlam * delta_t               ;  pas en arcsec pendant une periode du wobbler
pas_az = sgn(donnees.pas_az)*donnees.otfvlam * delta_t     	;  pas en arcsec pendant une periode du wobbler
print, "donnees.pas_az = ", donnees.pas_az
print, "pas_az = ", pas_az

lst_asso = lst_dif

ENDIF ELSE BEGIN			;  Total-power case --> Work on averaged data

delta_t = n_ima_gr * delta_t				;  periode en sec pour chaque groupe
pas_az = donnees.otfvlam * delta_t     			;  pas en arcsec pendant un groupe d'images

lst_asso = lst

ENDELSE

IF (donnees.wobused eq 1) THEN BEGIN   ;  Dual-beam case --> Work on phase differences

rmsmoy=extract_ima_moy(cube_dif_rms,rms_rms)

reduce_donneestruct, donnees, n_groupes/2, diff

diff.cube = cube_dif
diff.rmsmoy = rmsmoy
diff.lst = lst_dif
diff.pas_az = pas_az

struct_expand, diff, 'phase', phase_dif, diff, pos=8
struct_expand, diff, 'rms', cube_dif_rms, diff, pos=2

for i=0, n_groupes/2-1 do begin
	diff.datapar(i).longoff = longoff_dif(i)
	diff.datapar(i).latoff  = latoff_dif(i)
;
	diff.datapar(i).baslong = baslong_dif(i)
	diff.datapar(i).baslat  = baslat_dif(i)		
endfor

id_ima_milieu = (n_groupes/2)/2
diff.lst_mid = diff.lst(id_ima_milieu)

diff.alpha_mid = diff.datapar(id_ima_milieu).baslong
diff.delta_mid = diff.datapar(id_ima_milieu).baslat

if donnees.scandir eq 'ALON' then begin
	daz_mid = diff.datapar(id_ima_milieu).longoff
	del_mid = diff.datapar(id_ima_milieu).latoff
;	print, "daz_mid (arcsec) = ", daz_mid*3600., "  del_mid (arcsec) = ", del_mid*3600.
;
	par_ang = donnees.crota1 - donnees.elevatio	 			;  Parallactic angle in degrees
	rot_dazdel2draddec, daz_mid, del_mid, par_ang, dra_mid, ddec_mid
;
	diff.alpha_mid = double(donnees.blongobj) + dra_mid/cos(donnees.blatobj*!pi/180.)		; angle on sphere in deg
	diff.delta_mid = double(donnees.blatobj)  + ddec_mid
endif

;diff = {cube:cube_dif, phase:phase_dif, rms:cube_dif_rms, rmsmoy:rmsmoy,$
;        lst:lst_dif, chopamp:donnees.chopamp, pas_az: pas_az, $		
;	cdelt1: donnees.cdelt1, elevatio : donnees.elevatio, $
;	lst_mid : donnees.lst_mid, alpha_mid:donnees.alpha_mid, $
;	delta_mid: donnees.delta_mid, otfvlam : donnees.otfvlam, $
;	filename : donnees.filename, source : donnees.source, lamdel : donnees.lamdel, betdel : donnees.betdel, $
;	goodpix_ima:donnees.goodpix_ima}

diff_asso = diff

ENDIF ELSE BEGIN			;  Total-power case --> Work on averaged data
;	
;pas_az = donnees.otfvlam * delta_t/2.     ;  en arcsec		
rmsmoy = extract_ima_moy(cube_rms,rms_rms)	

reduce_donneestruct, donnees, n_groupes, asso

asso.cube = cube_dif
asso.rmsmoy = rmsmoy
asso.lst = lst
asso.pas_az = pas_az

struct_expand, asso, 'phase', chopbeam(debut), asso
struct_expand, asso, 'rms', cube_rms, asso, pos=2

;asso = {cube:cube_moy, phase:chopbeam(debut), rms:cube_rms, $
;        rmsmoy:rmsmoy, lst:lst, chopamp:donnees.chopamp, $
;        pas_az: pas_az, cdelt1: donnees.cdelt1, $
;	elevatio : donnees.elevatio, lst_mid : donnees.lst_mid, $
;	alpha_mid:donnees.alpha_mid, delta_mid: donnees.delta_mid, $
;	otfvlam : donnees.otfvlam, filename : donnees.filename, $
;	source : donnees.source, lamdel : donnees.lamdel, betdel : donnees.betdel, goodpix_ima:donnees.goodpix_ima}

diff_asso = asso

ENDELSE

return
end
