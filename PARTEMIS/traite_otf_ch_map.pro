pro traite_otf_ch_map, scan_number, cube_ch_map_str, champ_masque=champ_masque, dir_dat=dir_dat, newreduc=newreduc, nopowermap=nopowermap, tau=tau, flat=flat

; Usage:
; traite_otf_ch_map, 47388, saturn_ch_map_47388_str, tau=0.81, /newreduc
; traite_otf_ch_map, 50010, mars_ch_map_50010_str, tau=0.8
; traite_otf_ch_map, 48969, mars_ch_map_48969_str, tau=0.8
; traite_otf_ch_map, 48139, mars_ch_map_48139_db_str, tau=1.73
; flat_mars_47518 = flat_mars_47525*0.
; traite_otf_ch_map, 47518, mars_ch_map_47518_str, tau=0.8, flat = flat_mars_47518
; traite_otf_ch_map, 47525, mars_ch_map_47525_str, tau=0.83			   ; flat_mars = (flat_mars_47518*flat_mars_47520*flat_mars_47525)^(1./3.)
; save, file = '/Users/artemis/Desktop/Calib_partemis/flat_mars_16nov07.xdr', flat_mars , /verb
; for i= 0, 255 do atv, mars_ch_map_47525_str.ch_map(*,*,i)
; max_mars_47525 = fltarr(256)
; conv_mars_47525 = fltarr(256)
; for i= 0, 255 do max_mars_47525(i) = max(mars_ch_map_47525_str.ch_map(*,*,i))
; for i= 0, 255 do conv_mars_47525(i) = max_mars_47525(i)/4718.					;  Expected peak flux density of Mars : 4718. Jy/9.4"-beam
; conv_mars_47525 = reform(conv_mars_47525,16,16)
; flat_mars_47525 = conv_mars_47525/conv_mars_47525(119)
; atv, (exp(0.8/sin(50*!pi/180.))/sqrt(40.))*sky_47519_noise_str(2).rms_noise_corr/conv_mars_47525
;
; traite_otf_ch_map, 47214, jupiter_ch_map_47214_str, /newreduc
; writefits,'/Users/artemis/Documents/apexdata/map_otf_fits/jupiter_otf_ch_map_47214.fits', jupiter_ch_map_47214_str.ch_map 
; gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/jupiter_otf_ch_map_47214.gdf", jupiter_ch_map_47214_str.ch_map, /write
; for i= 0, 255 do atv, jupiter_ch_map_47214_str.ch_map(*,*,i)
;
; traite_otf_ch_map, 4826, saturn_ch_map_4826_str
; traite_otf_ch_map, 4826, saturn_ch_map_4826_str, /newreduc, tau=0.6
; writefits,'/Users/artemis/Documents/apexdata/map_otf_fits/saturn_otf_ch_map_4826.fits', saturn_ch_map_4826_str.ch_map
; gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/saturn_otf_ch_map_4826.gdf", saturn_ch_map_4826_str.ch_map, /write
; for i= 0, 255 do atv, saturn_ch_map_4826_str.ch_map(*,*,i)
;
; restore, '/Users/artemis/Desktop/Calib_partemis/goodpix_ima_save.xdr'
; goodpix_ima = goodpix_ima*0 +1
; goodpix_ima(0:7,9:*) = 0
; save, goodpix_ima, file='/Users/artemis/Desktop/Calib_partemis/goodpix_ima.xdr'
;
; traite_otf_ch_map, 4792, jupiter_ch_map_4792_str, /newreduc, tau=0.5
; for i= 0, 255 do atv, jupiter_ch_map_4792_str.ch_map(*,*,i)
; gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/jupiter_otf_ch_map_4792.gdf", jupiter_ch_map_4792_str.ch_map, /write
; atv, jupiter_ch_map_4792_str.ch_map(*,*,10)
;
;
; toto_n6334 = findfile('/Users/artemis/Documents/apexdata/map_otf_xdr/T-78.F-0006-2006/otf_subscan*4806*.xdr')
;
; restore, toto(12), /verb
; atv, carte_scan.image
; tvkosma, donnees_red.cube(*,*,400)
; for i= 300, 500 do tvkosma, donnees_red.cube(*,*,i)
;
; writefits, '/Users/artemis/Documents/apexdata/map_otf_fits/jupiter_otf_az_4792.fits', jupiter_4792_str.image
;

COMMON obs1_configb, work_dir, project_name, calibration_table     ; chargement des common variables depuis obs1_config


dir=find_all_dir(work_dir + 'apexdata/map_otf_xdr/' + project_name)  ; si le repertoire ou l'on souhaite stocker les donnees reduites n'existe pas, il est cree

if dir EQ '' then begin

print, 'creating directory'+ work_dir + 'map_otf_xdr/' + project_name

spawn, 'mkdir' + ' ' + work_dir + 'apexdata/map_otf_xdr/' + project_name

endif


init_obs, scan_number=scan_number, type='chmap', init_obs_str         ; initialisation et recuperation des fichiers de calibration, des chemins d'acces, de la liste
                                              			   ; des subscans  pour le scan donne     


if keyword_set(project) EQ 0 then begin                            ; si le nom de projet n'est pas specifie en appel, on utilise le nom de projet specifie dans 
                                                                   ; obs1_config
project=project_name

endif


if keyword_set(champ_masque) then begin
   restore, champ_masque
   image_modele = champ_masque
endif   


chaine = ''

champ  = [{subscan_name : '', long_min : 0. , long_max : 0., $
                        lat_min : 0. , lat_max : 0. , lat_mean : 0. }]
champ1 = [champ]

parang0 = 0.

;
;restore, Calib_partemis+'goodpix_ima_mars_16nov07.xdr'		;  <-- goodpix_ima_mars
;restore, Calib_partemis+'flat_mars_16nov07.xdr'			;  <-- flat_mars
;goodpix_ima = goodpix_ima_mars
;flat_jup = flat_mars

for i=0, n_elements(subscan_liste)-1 do begin

    if keyword_set(nopowermap) then begin
;	donnees = read_apex(scan_number, subscan_liste(i), filetype, dir_orig=dirdat, /nopowermap)
        donnees=read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), filetype, goodpix_ima=init_obs_str.goodpix_ima, /nopowermap)
    endif else begin	
;	donnees = read_apex(scan_number, subscan_liste(i), filetype, dir_orig=dirdat)
        donnees=read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), goodpix_ima=init_obs_str.goodpix_ima, filetype)
    endelse
    
    donnees_uncal = donnees
    
    if keyword_set(tau) and donnees.unit eq 'pW' then begin
    	amass = 1./sin(donnees.elevatio*!pi/180.)
	donnees.cube = donnees.cube*exp(tau*amass)
	donnees.unit = 'pW (ext. corr.)' 
    endif            

     donnees.goodpix_ima = init_obs_str.goodpix_ima                                    ; affectation du tableau goodpix_ima (bon pixel : 1, mauvais pixel : 0)


     filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(init_obs_str.scan_number),1)+'_'+init_obs_str.subscan_liste(i)+'.xdr'
	
     fichier_otf_scan = findfile(filename ,count=count)


;IF init_obs_str.type eq 'chmap' THEN BEGIN                 ; si le type est CHMAP

        if count eq 0 or keyword_set(newreduc) then begin           ; si les donnees reduites n'existent pas ou si l'on souhaite realiser une nouvelle reduction
		traite_otf_scan, donnees,image_modele, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv  

	endif else begin
		restore, filename=filename
		print, '<----- ' + filename
	;	subscan_name(i) = subscanname
	endelse

;ENDIF

   echant = 3    ; facteur de rebin pour la construction du scan				

   if keyword_set(do_rcp) then begin          ; si l'on souhaite utiliser les fichiers de distorsion optique
	
   do_map_scan, donnees_red, positions, out_ima_dim, echant=echant, dx_rcp=init_obs_str.dx_rcp, dy_rcp=init_obs_str.dy_rcp, $           ; creation d'une carte pour un subscan 
   /do_rcp, carte_scan

   endif else begin                           ; si l'on ne souhaite pas utiliser les fichiers de distorsion optique

   do_map_scan, donnees_red, positions, out_ima_dim, echant=echant, carte_scan                                                          ; creation d'une carte pour un subscan 

   endelse

   if count eq 0 or keyword_set(newreduc) or init_obs_str.type eq 'decorrel' then begin                                                
        if error_scan eq 0 then begin
			subscanname = init_obs_str.subscan_name(i)

			save, filename=filename, subscanname, carte_scan, donnees_uncal, donnees, donnees_red                        ; sauvegarde des donnees pour toute 
			print, '-----> ' + filename                                                                                  ; nouvelle reduction
	endif else begin
			print, "Error treating subscan ", i	   
        endelse	   
				;  --> Map construction in RA-DEC	
   endif

create_champ, donnees=donnees, carte_scan=carte_scan, error_scan=error_scan, subscan_liste=init_obs_str.subscan_liste, subscan_name=init_obs_str.subscan_name, i=i, type=init_obs_str.type, parang0, champ
; remplit la struture champ

;if init_obs_str.type eq 'chmap' then begin					; type 'CHMAP'

               champ1 = [champ1, champ]

;endif

	
	IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body  +  Scanning in Az
									;  --> Map construction in Az-El	

	elev = carte_scan.elevatio		; degrees

; 	daz_scan et del_scan sont les offsets (en arcsec) du milieu du scan par rapport a la source pointee
;			     calcules par do_map_scan

	daz_scan = carte_scan.daz_scan			; en arcsec
	del_scan = carte_scan.del_scan			; arcsec

	IF i eq n_elements(subscan_liste)/2 THEN BEGIN
;		parang0 = carte_scan.crota1 - carte_scan.elevatio + 180.		; Parallactic angle at central scan      KOSMA and APEX !! 
		parang0 = carte_scan.crota1 - carte_scan.elevatio		; Parallactic angle at central scan      APEX !
	ENDIF
;

	rot_angle = -elev		        		; degrees	;  APEX
;	rot_angle = -(elev - 180.)		        	; degrees	; clockwise  (KOSMA and APEX !!)

	ENDIF ELSE BEGIN						;  Normal source or Scanning in RA
									;  --> Map construction in RA-DEC	


	  rot_angle = carte_scan.crota1*!pi/180.	 ; radians
	

	ENDELSE	
	
endfor


champ0 = champ1(1:n_elements(champ1)-2)

champ1 = champ1(2:*)

long_min = min(champ1(*).long_min)
long_max = max(champ1(*).long_max)
lat_min = min(champ1(*).lat_min)
lat_max = max(champ1(*).lat_max)

cdelt2 = abs(min(champ1(*).lat_mean-champ0(*).lat_mean))

if donnees.wobused eq 1 then begin
	cdelt2 = abs(donnees.otfvlam*donnees.wobcycle)/3600.
endif

cdelt1 = cdelt2

print, "Adopted pixel size (arcsec) : ", cdelt2*3600.

; definition de la grille pour les cartes par canaux reconstruites dans les coordonnees de balayage.
;
x_size_grid = 2.5*abs((long_max - long_min)/cdelt1)
y_size_grid = 2.5*abs((lat_max - lat_min)/cdelt2)

long_ref = (long_max + long_min)/2
lat_ref = (lat_max + lat_min)/2
crpix1 = x_size_grid/2.
crpix2 = y_size_grid/2.

nchan = 16*16
print, "Total number of channels : ",nchan 
ch_map_cube = fltarr(x_size_grid, y_size_grid, nchan)
weight_ch_map = ch_map_cube

IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body  +  Scanning in Az
									;  --> Map construction in Az-El	
	print, 'Parallactic angle (deg) at central scan = ', parang0
ENDIF

nxchan = indgen(16)#(transpose(intarr(16))+1)
nychan = (intarr(16)+1)#transpose(indgen(16))

for i=0, n_elements(champ1.subscan_name)-1 do begin
;	filename='/Users/artemis/Documents/apexdata/map_otf_xdr/T-78.F-0006-2006/otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
;	filename='/Users/artemis/Documents/apexdata/map_otf_xdr/E-080.C-0722A-2007/otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	filename= apexdata+'map_otf_xdr/E-080.C-0722A-2007/otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	
	restore, filename  ;  (subscan_name, carte_scan, donnees)
;	subscan_name(i) = subscanname

	taille = size(donnees_red.cube)
;	nrecord = taille(3)
	nrecord = taille(3)-1								; To avoid potential position glitch at end of scan

	pos_long = nint((donnees_red(*).datapar.longoff-long_ref)/cdelt1 + crpix1)
	pos_lat = nint((donnees_red(*).datapar.latoff-lat_ref)/cdelt2 + crpix2)

	IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body
									;  --> Map construction in Az-El	
	        rot_angle = -carte_scan.elevatio        				; degrees	;  APEX
;		rot_angle = -(carte_scan.elevatio - 180.)		        	; degrees (clockwise)  KOSMA and APEX !!

		parang  = carte_scan.crota1 - carte_scan.elevatio			; Parallactic angle	APEX
;		parang  = carte_scan.crota1 - carte_scan.elevatio + 180.		; Parallactic angle	(KOSMA and APEX !!!)
		dparang = parang - parang0						; Delta parang / central scan

		rot_angle = rot_angle - dparang

	ENDIF ELSE BEGIN						;  Normal source or Planet in RA
									;  --> Map construction in RA-DEC			
	        rot_angle = -carte_scan.crota1	            ;  -(parang + El)  (degrees)   APEX
		parang  = carte_scan.crota1 - carte_scan.elevatio		; Parallactic angle    APEX
;	        rot_angle = -carte_scan.crota1	            ;  -(parang + El - 180)  (degrees)   (+ for Nasmyth B/KOSMA and Nasmyth A/APEX !!)
;		parang  = carte_scan.crota1 - carte_scan.elevatio + 180.		; Parallactic angle	(KOSMA and APEX !!!)
	ENDELSE	

;
	for ichan = 0, nchan-1 do begin
;	
	  nx = nxchan(ichan)		; nx, ny = coordonnees du pixel dans la camera *avant re-echantillonnage"
	  ny = nychan(ichan)		;
	  channel = [nx,ny]	    
;
		for irec =0,nrecord-1 do begin			
			x_long = pos_long(irec)
			y_lat  = pos_lat(irec)
			
			weight_elem = 1.
;			weight_elem = 1./(donnees_red.rmsmoy(nx,ny))^2
			ch_map_cube(x_long,y_lat,ichan) = ch_map_cube(x_long,y_lat,ichan) + donnees_red.cube(nx,ny,irec)*weight_elem
			weight_ch_map(x_long,y_lat,ichan) = weight_ch_map(x_long,y_lat,ichan)+weight_elem
		endfor
;
	endfor
;
endfor

;
;for ichan = 0, nchan-1 do begin
;	nx = nxchan(ichan)		; nx, ny = coordonnees du pixel dans la camera *avant re-echantillonnage"
;	ny = nychan(ichan)		; 
;	weight1 = weight_ch_map(*,*,ichan)
;	ch_map1 = ch_map_cube(*,*,ichan)
;	id = where(weight1 ne 0., count)
;	if count ne 0 then ch_map1(id) = ch_map1(id)/weight1(id)
;endfor

id = where(weight_ch_map gt 0., count)
if count ne 0 then ch_map_cube(id) = ch_map_cube(id)/weight_ch_map(id)

cube_ch_map_str = {name:obsname, ch_map:ch_map_cube, weight:weight_ch_map, crpix1:crpix1, crpix2:crpix2, $
                  scansys:donnees_red.scandir, long_ref:long_ref, lat_ref:lat_ref, cdelt1:cdelt1, cdelt2:cdelt2, $
	          subscan_liste:subscan_liste, source:donnees_red.source}
		  
 max_mars  = fltarr(256)
 conv_mars = fltarr(256)
 for i= 0, 255 do max_mars(i) = max(cube_ch_map_str.ch_map(*,*,i))
 for i= 0, 255 do conv_mars(i) = max_mars(i)/4718.					;  Expected peak flux density of Mars : 4718. Jy/9.4"-beam
 print, "Median_Conv (pW/Jy) = ", median(conv_mars)
 conv_mars = reform(conv_mars,16,16)
 flat_mars = conv_mars/conv_mars(119)
 atv, flat_mars
; atv, (exp(0.8/sin(50*!pi/180.))/sqrt(40.))*sky_47519_noise_str(2).rms_noise_corr/conv_mars

if keyword_set(flat)  then begin
   flat = flat_mars 
endif 	  

return
end
