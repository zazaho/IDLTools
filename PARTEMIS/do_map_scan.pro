;+
; NAME:
;	DO_MAP_SCAN
;
; PURPOSE:
;
;	Create a map for a given subscan.	
;
; CALLING SEQUENCE:
;
;	DO_MAP_SCAN, Datastr, Positions, Map_out_dim, Carte_scan
;
; INPUTS:
;
;	Datastr:	Input data structure.
;	Positions:	Coordinates in pixel image.	
;
; OPTIONAL INPUTS:
;
;	Echant:		Rebin factor.
;	Dx_rcp:		Pixels offsets in x direction.
;	Dy_rcp:		Pixels offsets in y direction.	
;	
; KEYWORD PARAMETERS:
;
;	DO_RCP:		Use distorsion files. 
;
; OUTPUTS:
;
;	Map_out_dim:	Rebined map dimensions.
;	Carte_scan:	Subscan map.
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project name and calibration
;			file.
;
; EXAMPLE:
;
;		DO_MAP_SCAN, Donnees_red, Positions, Out_ima_dim, Echant=3, Dx_rcp=Dx_rcp, Dy_rcp=Dy_rcp, /DO_RCP, Carte_scan
;
; MODIFICATION HISTORY:
;
;-

pro do_map_scan, datastr, positions, map_out_dim, echant=echant, dx_rcp=dx_rcp, dy_rcp=dy_rcp, do_rcp=do_rcp, carte_scan


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	DO_MAP_SCAN, Datastr, Positions, Map_out_dim, Echant= Echant, Dx_rcp= Dx_rcp, Dy_rcp= Dy_rcp, Do_rcp= Do_rcp, Carte_scan'
	return
endif	


;create a map for a given subscan

COMMON obs1_configb

; set_pos_  calcule le tableau "positions" (npairwobb) contenant les coordonnees
; x0,y0 du pixel (0,0) de la camera dans l'image re-echantillonnee de sortie

if keyword_set(do_rcp) eq 1 then begin 	

pfov_ref=6.2
						
 ;set_pos_, datastr, echant=echant, positions, map_out_dim, /do_rcp         ; on tient compte de la distorsion optique
 set_pos_, datastr, echant=echant*abs(datastr.cdelt1)/pfov_ref, positions, map_out_dim, /do_rcp 
endif else begin
 set_pos_, datastr, echant=echant, positions, map_out_dim
endelse

 

; indice de l'image du centre du scan et du temps sideral local associe
n_images = n_elements(datastr.datapar.azimuth)                  
id_ima_milieu = n_images/2                                       
datastr.lst_mid = datastr.lst(id_ima_milieu)
;print, "id_ima_milieu = ", id_ima_milieu


datastr.alpha_mid = datastr.datapar(id_ima_milieu).baslong
datastr.delta_mid = datastr.datapar(id_ima_milieu).baslat


; recherche de l'image dont le lst est le plus proche de lst_mid
;delta_lst = abs(datastr.lst - datastr.lst_mid)
;bidon = min(delta_lst, id_min_lst)
;print, "id_min_lst = ", id_min_lst
;
id_min_lst = id_ima_milieu

;delta_lst = datastr.lst(id_min_lst) - datastr.lst_mid
;delta_az = datastr.otfvlam*dlst2dtloc(delta_lst)

alpha_ref = datastr.alpha_mid
delta_ref = datastr.delta_mid


rms_noise = datastr.rmsmoy
cube = datastr.cube

taille = size(cube)
size_array = taille(1)

;ind = where(datastr.lst ne -999., cnt)						         ; eliminating data with unknown telescope positions

ind = where (datastr.datapar.longoff ne -999. and datastr.datapar.latoff ne -999., cnt)

if cnt eq 0 then begin
  
  print, "ERROR (do_map_scan): longoff and latoff are undefined (-999)"
  return

endif 

;npairwobb = min([taille(3),(size(datastr.datapar))(3)])
npairwobb = cnt

ndim = map_out_dim
npixrebin = size_array*echant 

image_finale = fltarr(ndim,ndim)
masque = fltarr(ndim,ndim)

weight = double(rms_noise*0.)		; 16x16 array (p-ArTeMiS) / n1xn2 array

index = where(rms_noise gt 0., count)
if (count gt 0) then begin
   weight(index) = 1.d0/rms_noise(index)^2
endif else begin
	print, 'WARNING   WARNING'
	print, '% DO_MAP_SCAN : rms noise = 0 pour tous les pixels ' 
	weight = weight +1
endelse

weight = weight * datastr.goodpix_ima

if keyword_set(do_rcp) EQ 0 then begin 
weight_rebin = rebin(weight, npixrebin, npixrebin, /sample)

cdelt1 = -datastr.cdelt1 / echant / 3600.     ; increment en degres	cdelt1 est negatif pour etre correct apres rotation vers RA-DEC
cdelt2 = -cdelt1

endif

if keyword_set(do_rcp) EQ 1 then begin 


cdelt1 = -pfov_ref / echant / 3600.     ; increment en degres	cdelt1 est negatif pour etre correct apres rotation vers RA-DEC
cdelt2 = -cdelt1

endif

crota1 = datastr.crota1
							; calcule maintenant dans traite_otf_scan
IF keyword_set(do_rcp) EQ 1 THEN BEGIN                 ; si l'on tient compte de la distorsion optique

kern_size = 3

x_kern0 = (indgen(kern_size)-kern_size/2)#((intarr(kern_size))+1)
y_kern0 = (intarr(kern_size)+1)#(indgen(kern_size)-kern_size/2)

x_kern_grid0 = x_kern0(0:kern_size^2-1)#(intarr(taille(1)*taille(2))+1)				;  25x256
y_kern_grid0 = y_kern0(0:kern_size^2-1)#(intarr(taille(1)*taille(2))+1)


ndim_sub = npixrebin+kern_size-echant+2

ind_kern0 = (intarr(kern_size)+1)#((indgen(kern_size)-kern_size/2)*ndim_sub) + (indgen(kern_size)-kern_size/2)#(intarr(kern_size)+1)

ind_kern0 = ind_kern0(0:kern_size^2-1)					; 25 array


x_r0 = dx_rcp(0:taille(1)*taille(2)-1)/cdelt2/3600. 				 		; in units of pixels of the resampled grid	n1xn2 (256 array for p-ArTeMiS)
y_r0 = dy_rcp(0:taille(1)*taille(2)-1)/cdelt2/3600. 


x_subim_cube0 = (fltarr((kern_size)*(kern_size))+1.)#x_r0					;  25x256 array  (p-ArTeMiS)

y_subim_cube0 = (fltarr((kern_size)*(kern_size))+1.)#y_r0					;  25x256 array  (p-ArTeMiS)



   FOR i=0L,npairwobb-1L DO BEGIN

;	image = cube(*,*,i)
	image = cube(*,*,ind(i))
	x0_r = positions(i,0)							; offsets of array center in units of pixels of the resampled grid
	y0_r = positions(i,1)


        x0_i = nint(x0_r)
	y0_i = nint(y0_r)

;;;;;;;;;;;;;;;;;;;;;;;;;;; ndim_sub = npixrebin+kern_size-echant + 2
		
	x0 = x0_i - (echant*(taille(1)/2-1)+echant/2) - (kern_size-echant)/2 - 1	; extreme position of array's bottom left corner in resampled grid
	y0 = y0_i - (echant*(taille(2)/2-1)+echant/2) - (kern_size-echant)/2 - 1	


	x_r = x_r0 + x0_r 							; in units of pixels of the resampled grid	n1xn2 (256 array for p-ArTeMiS)
	y_r = y_r0 + y0_r

	x_r_grid = x_subim_cube0 + x0_r									; 25x256 array     (p-ArTeMiS)
	y_r_grid = y_subim_cube0 + y0_r
	
	x_i = nint(x_r)											; 256 array        (p-ArTeMiS)
	y_i = nint(y_r)	

	x_i_grid = (intarr((kern_size)*(kern_size))+1.)#x_i						; 25x256 array     (p-ArTeMiS)
	
	x_kern_grid = x_kern_grid0 + x_i_grid
	
	y_i_grid = (intarr((kern_size)*(kern_size))+1.)#y_i						; 25x256 array     (p-ArTeMiS)

	y_kern_grid = y_kern_grid0 + y_i_grid
	
	dx = (x_kern_grid - x_r_grid)*datastr.cdelt2/echant			; in arcsec      	  25x256 array     (p-ArTeMiS)
	dy = (y_kern_grid - y_r_grid)*datastr.cdelt2/echant

	dr =  sqrt(dx^2+dy^2)										; in arcsec
	att = grid_kernel(dr)                                                                           ;  25x256 array ?  (p-ArTeMiS)


	ind_subim = (y_i-y0)*ndim_sub + (x_i-x0)							;  n1xn2 array (16x16 array for p-ArTeMiS)
	
	ind_subim = ind_subim(0:taille(1)*taille(2)-1)								;  256 array (p-ArTeMiS)
	
	ind_grid = ind_kern0#(intarr(taille(1)*taille(2))+1) + (intarr(kern_size^2)+1)#ind_subim		;  25x256 array  (p-ArTeMiS)

	for k=0,kern_size^2-1 do begin
	
		subim_grid_k = fltarr(ndim_sub,ndim_sub)
		subim_grid_k(ind_grid(k,*)) = image(0:taille(1)*taille(2)-1)
		
		weight_subim_grid_k = fltarr(ndim_sub,ndim_sub)		
		weight_subim_grid_k(ind_grid(k,*)) = weight(0:taille(1)*taille(2)-1)*att(k,0:taille(1)*taille(2)-1)
		
		image_finale(x0:x0+ndim_sub-1,y0:y0+ndim_sub-1) = image_finale(x0:x0+ndim_sub-1,y0:y0+ndim_sub-1) + subim_grid_k*weight_subim_grid_k
     		masque(x0:x0+ndim_sub-1,y0:y0+ndim_sub-1) = masque(x0:x0+ndim_sub-1,y0:y0+ndim_sub-1) +  weight_subim_grid_k

	endfor

   ENDFOR        ; (0 to npairwobb-1)

ENDIF ELSE BEGIN    ; on ne tient pas compte de la distorsion optique


   for i=0,npairwobb-1 do begin
;	image = rebin(cube(*,*,i), npixrebin, npixrebin, /sample)
	image = rebin(cube(*,*,ind(i)), npixrebin, npixrebin, /sample)
	x0 = positions(i,0)
	y0 = positions(i,1)
	image_finale(x0:x0+npixrebin-1,y0:y0+npixrebin-1) = image_finale(x0:x0+npixrebin-1,y0:y0+npixrebin-1) + image*weight_rebin
	masque(x0:x0+npixrebin-1,y0:y0+npixrebin-1) = masque(x0:x0+npixrebin-1,y0:y0+npixrebin-1) + weight_rebin
   endfor


ENDELSE   ;  (do_rcp)

xref0 = positions(id_min_lst, 0)                                ; pixel de reference dans la matrice non reechantillonee : (7,7)  <--> 120 (p-ArTeMiS) ?
yref0 = positions(id_min_lst, 1)


if keyword_set(do_rcp) EQ 1 then begin                          ; si l'on tient compte de la distorsion optique

xref = xref0
yref = yref0 

endif else begin

xref = xref0 + echant*(taille(1)/2-1)+echant/2			; pixel de reference dans la matrice non reechantillonee : (7,7)  <--> 120 (p-ArTeMiS)
yref = yref0 + echant*(taille(2)/2-1)+echant/2

endelse
dra_scan = -999
ddec_scan = -999
daz_scan = -999
del_scan = -999

IF datastr.scandir eq 'RA' THEN BEGIN                   ; scan en ascension droite

	; le pixel de l'image rebinnee (xref, yref) est en (alpha_ref, delta_ref) 
	;                                            et en (dra_scan, ddec_scan) par rapport a la source pointee
	; 	dra_scan et ddec_scan sont les offsets (en arcsec) du milieu du scan par rapport a la source pointee
	;

	dra_scan  = datastr.datapar(id_min_lst).longoff*3600.
	ddec_scan = datastr.datapar(id_min_lst).latoff*3600.

ENDIF ELSE BEGIN                                        ; scan en elevation

	; le pixel de l'image rebinnee (xref, yref) est en (alpha_ref, delta_ref) 
	;                                            et en (daz_scan, del_scan) par rapport a la source pointee
	;
	; 	daz_scan et del_scan sont les offsets (en arcsec) du milieu du scan par rapport a la source pointee
	;	(calculable a partir de lamdel, betdel, otfvlam et lst_mid)
	;
	;  daz_scan = datastr.lamdel + datastr.otfvlam*dlst2dtloc(datastr.lst_mid - datastr.lst(0))	; arcsec
	;  del_scan = datastr.betdel

	daz_scan = datastr.datapar(id_min_lst).longoff*3600.
	del_scan = datastr.datapar(id_min_lst).latoff*3600.

ENDELSE


map_out = image_finale*0
id = where(masque ne 0)
map_out(id) = image_finale(id)/masque(id)

carte_scan = {filename :datastr.filename, image : map_out, weight : masque, crpix1 : xref, $
              crpix2 : yref, alpha_ref : alpha_ref, delta_ref : delta_ref, elevatio:datastr.elevatio, $
              cdelt1 : cdelt1, cdelt2 : cdelt2, crota1 : crota1, $
              scandir : datastr.scandir, dra_scan : dra_scan, ddec_scan:ddec_scan, $
	      daz_scan : daz_scan, del_scan : del_scan, source:datastr.source}

return
end	 
