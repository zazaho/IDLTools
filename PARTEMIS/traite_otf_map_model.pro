;+
; NAME:
;	TRAITE_OTF_MAP_MODEL
;
; PURPOSE:
;
;	Reduce data for a given observation and build a map.	
;
; CALLING SEQUENCE:
;
;	TRAITE_OTF_MAP_MODEL, Scan_number=Scan_number, Type=Type, Mapradecstr
;                            
; INPUTS:
;
;	Scan_number:	Observation ID number.
;	Type:		MAP, CHMAP or DECORREL
;		
; OPTIONAL INPUTS:
;
;	Champ_masque:	Existing mask.
;	Champ_base:	Existing mask.
;	Model:		Existing model.
;	Rmode:		Wobbler restoration mode(SAA, EKH OR TP).
;	Project:	Alternative sub-directory name to store reduced data.
;	
; KEYWORD PARAMETERS:
;
;	MED_NOISE_REM:	Remove median noise.
;		
;	MED_BASE:	Remove baseline.
;
;	DO_RCP:		Use optical distorsion file.
;
; OUTPUTS:
;
;	Mapradecstr:	Map structure.
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name
;		
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;-

pro traite_otf_map_model, scan_number=scan_number, model=model, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, med_noise_rem = med_noise_rem,$
med_base = med_base, rmode=rmode, project=project, do_rcp=do_rcp, rms_corr=rms_corr, rms_uncorr=rms_uncorr

if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	TRAITE_OTF_MAP_MODEL, scan_number=scan_number, model=model, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, med_noise_rem = med_noise_rem,med_base = med_base, rmode=rmode, project=project, do_rcp=do_rcp'

	return
endif

COMMON obs1_configb, work_dir, project_name, calibration_table     ; chargement des common variables depuis obs1_config


dir=find_all_dir(work_dir + 'apexdata/map_otf_xdr/' + project_name)  ; si le repertoire ou l'on souhaite stocker les donnees reduites n'existe pas, il est cree

if dir EQ '' then begin

print, 'creating directory'+ work_dir + 'map_otf_xdr/' + project_name

spawn, 'mkdir' + ' ' + work_dir + 'apexdata/map_otf_xdr/' + project_name

endif


init_obs, scan_number=scan_number, type=type, init_obs_str         ; initialisation et recuperation des fichiers de calibration, des chemins d'acces, de la liste
                                              			   ; des subscans  pour le scan donne     

if not keyword_set(model) then begin
	print, 'A sky model must be provided using the model keyword'
	print, 'Calling sequence : '
	print, '	TRAITE_OTF_MAP_MODEL, scan_number=scan_number, model=model, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, med_noise_rem = med_noise_rem,med_base = med_base, rmode=rmode, project=project, do_rcp=do_rcp'

	return

endif else begin

    restore, init_obs_str.apexdata + 'map_otf_fits/' + model, /verb
    print, "Sky model (model_str) :"
    help, model_str, /str
 
endelse


if keyword_set(project) EQ 0 then begin                            ; si le nom de projet n'est pas specifie en appel, on utilise le nom de projet specifie dans 
                                                                   ; obs1_config
project=project_name

endif


if keyword_set(champ_masque) then begin                            ; utilisation d'un masque

   restore, init_obs_str.apexdata + 'map_otf_fits/'+champ_masque
   masque_modele_str = masque_str

endif

if keyword_set(champ_base) then begin                              ; utilisation d'un masque

   restore, init_obs_str.apexdata + 'map_otf_fits/' + champ_base                
   masque_base_str = masque_str
   print, "masque_base_str :"
   help, masque_base_str, /str
   
endif


IF init_obs_str.type eq 'decorrel' or init_obs_str.type eq 'map' THEN BEGIN                 ; type 'DECORREL' ou 'MAP'

 chaine = ''

 champ  = [{subscan_name : '', alpha_min : 0. , alpha_max : 0., $
                        delta_min : 0. , delta_max : 0., cdelt1: 0. , cdelt2:0. }]   ; champ est different suivant le type 'decorrel'/'map' ou 'chmap'

 scan_min_longoff = fltarr(n_elements(init_obs_str.subscan_liste))
 scan_max_longoff = fltarr(n_elements(init_obs_str.subscan_liste))                   ; necessaire pour map_make_direct
 scan_min_alpha = fltarr(n_elements(init_obs_str.subscan_liste))
 scan_max_alpha = fltarr(n_elements(init_obs_str.subscan_liste))    
 error_otf_scan = fltarr(n_elements(init_obs_str.subscan_liste))

 if not keyword_set(rmode) then rmode='TP' else rmode=strupcase(rmode)               ; si rmode n'est pas specifie, rmode doit etre egal a TP
 print, "keyword_set(rmode) ", keyword_set(rmode), " Restoration Mode :", rmode

ENDIF


if init_obs_str.type eq 'chmap' then begin					    ; type 'CHMAP'

chaine = ''

champ  = [{subscan_name : '', long_min : 0. , long_max : 0., $                      ; champ suivant le type 'CHMAP', on ne se preoccupe pas des dimensions
                        lat_min : 0. , lat_max : 0. , lat_mean : 0. }]              ; du champ de vue
endif



champ1 = [champ]

parang0 = 0.


FOR i=0, n_elements(init_obs_str.subscan_liste)-1 DO BEGIN

    ; Restore existing calibrated data
    
;	donnees=read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima) 	

       file_real= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(init_obs_str.scan_number),1)+'_'+init_obs_str.subscan_liste(i)+'.xdr'
	
       fichier_otf = findfile(file_real ,count=count)

       if count ge 1 then begin
		restore, filename=file_real			;  Restoring subscanname, carte_scan, donnees_uncal, donnees, donnees_red from real data/reduction
		print, '<----- ' + file_real
       endif else begin
        	print, ' Real data must have been reduced'
		print, 'Calling sequence : '
		print, '	TRAITE_OTF_MAP_MODEL, scan_number=scan_number, model=model, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, med_noise_rem = med_noise_rem,med_base = med_base, rmode=rmode, project=project, do_rcp=do_rcp'

		return       		
       endelse

    
IF  datatype(donnees) EQ 'STC' then begin           

    IF donnees.unit eq 'pW' or donnees.unit ne 'Jy/beam ext. corr.' THEN BEGIN
        print, ' Real data must have been calibrated and flat-fielded'
	print, 'Calling sequence : '
	print, '	TRAITE_OTF_MAP_MODEL, scan_number=scan_number, model=model, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, med_noise_rem = med_noise_rem,med_base, rmode=rmode, project=project, do_rcp=do_rcp'
	return       
    ENDIF 
    
	donnees.goodpix_ima = init_obs_str.goodpix_ima                                    ; affectation du tableau goodpix_ima (bon pixel : 1, mauvais pixel : 0)
 
	donnees_reelles = donnees_red
	carte_scan_reelle = carte_scan
;
	if keyword_set(do_rcp) then begin
		donnees_mod = make_model_data(donnees_reelles,model_str,dx_rcp=init_obs_str.dx_rcp, dy_rcp=init_obs_str.dy_rcp)
	endif else begin
		donnees_mod = make_model_data(donnees_reelles,model_str)
	endelse

	if keyword_set(rms_uncorr) then begin
		cube_mod = donnees_mod.cube
		taille = size(cube_mod)
		for jj=0,(taille(2)-1) do begin
		  for ii=0,(taille(1)-1) do begin
		    cube_mod(ii,jj,*) = cube_mod(ii,jj,*) + randomn(seed,taille(3))*rms_uncorr
		  endfor
		endfor		
		donnees_mod.cube = cube_mod 
	endif

	if keyword_set(rms_corr) then begin
		cube_mod = donnees_mod.cube
		taille = size(cube_mod)
		sky_noise = 1.e5+randomn(seed,taille(3))*rms_corr			;      20 pW <--> ~  1e5 Jy/beam
		for jj=0,(taille(2)-1) do begin
		  for ii=0,(taille(1)-1) do begin
		    cube_mod(ii,jj,*) = cube_mod(ii,jj,*) + sky_noise
		  endfor
		endfor		
		donnees_mod.cube = cube_mod 
	endif


	donnees = donnees_mod

    
    if init_obs_str.type eq 'decorrel' or init_obs_str.type eq 'map' then begin        ; type 'DECORREL' ou 'MAP'
    
	command = donnees.obslog.command                                ;  recuperation de donnees additionnelles dans les OBSLOG
	if strpos(command,'angle=') ne -1L then begin			; 'angle=' is contained in command
	  command = strmid(command,strpos(command,'angle='))
	endif else begin
	  command = -1
	endelse  
	if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
	 	angle = (strsplit(command,',',/extract))(0)
		angle =  float(strmid(angle,strpos(angle,'=')+1))	; Rotation angle (deg) of scanning direction relative to Az axis
	endif

	id = where(donnees.datapar.longoff ne -999. and donnees.datapar.latoff ne -999.) 

	    scan_min_alpha(i) = min(donnees.datapar(id).ra)	; deg
	    scan_max_alpha(i) = max(donnees.datapar(id).ra)

	if ((size(angle))(1) gt 1) then begin			;   if angle is defined ...
	  if (angle ne 0.0) then begin
	    print, "Scanning at angle ", angle, " (deg) with respect to Azimuth axis"  
	    angle = angle*!pi/180.d0				;  angle in radians
	    scan_min_longoff(i) = min(cos(angle)*donnees.datapar(id).longoff+sin(angle)*donnees.datapar(id).latoff)*3600.	; min offset along scanning direction (arcsec)   
	    scan_max_longoff(i) = max(cos(angle)*donnees.datapar(id).longoff+sin(angle)*donnees.datapar(id).latoff)*3600.	; max offset along scanning direction (arcsec)
	  endif else begin
	    scan_min_longoff(i) = min(donnees.datapar(id).longoff*3600.)	; arcsec
	    scan_max_longoff(i) = max(donnees.datapar(id).longoff*3600.)	; arcsec
	  endelse
	endif else begin
	    scan_min_longoff(i) = min(donnees.datapar(id).longoff*3600.)	; arcsec
	    scan_max_longoff(i) = max(donnees.datapar(id).longoff*3600.)	; arcsec
	endelse    

    	 endif	
	
       filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_mod_'+strtrim(string(init_obs_str.scan_number),1)+'_'+init_obs_str.subscan_liste(i)+'.xdr'
	
       fichier_otf_scan = findfile(filename ,count=count)

;donnees_save = donnees	

if init_obs_str.type eq 'decorrel' then begin               ; si le type est DECORREL

	   if keyword_set(med_noise_rem) then begin         ; si l'on souhaite soustraire le bruit median correle
	   
	    if keyword_set(med_base) then begin
	      if keyword_set(rmode) then begin              ; si rmode est specifie
		traite_otf_scan, donnees, masque_base_str, med_noise_rem = med_noise_rem, /med_base, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv	      
	      endif else begin
		traite_otf_scan, donnees, masque_base_str, med_noise_rem = med_noise_rem, /med_base, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv
	      endelse
	    endif else begin
	      if keyword_set(rmode) then begin              ; si rmode est specifie
		traite_otf_scan, donnees, masque_base_str, med_noise_rem = med_noise_rem, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv	      
	      endif else begin
		traite_otf_scan, donnees, masque_base_str, med_noise_rem = med_noise_rem, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv
	      endelse
	    endelse  	

	   endif else begin  
	  
	      if keyword_set(rmode) then begin               ; si rmode est specifie

		traite_otf_scan, donnees, masque_base_str, med_noise_rem = med_noise_rem, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv     

	      endif else begin	   
;
		traite_otf_scan, donnees, masque_base_str, med_noise_rem = med_noise_rem, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv   

	      endelse
	      	         		   	
	   endelse
	   error_otf_scan(i) = error_scan
	   	   	   
endif


IF init_obs_str.type eq 'chmap' THEN BEGIN                 ; si le type est CHMAP

		traite_otf_scan, donnees,image_modele, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv

ENDIF



IF init_obs_str.type eq 'map' THEN BEGIN		   ; si le type est MAP

		if keyword_set(med_base) then begin        ; si l'on souhaite soustraire la mediane des lignes de base
		  if keyword_set(rmode) then begin         ; si rmode est specifie
		   traite_otf_scan, donnees, masque_base_str, model=model_str, donnees_red, rms_noise, error=error_scan, /med_base, rest=rmode, conv=init_obs_str.Conv  
		  endif else begin
		   traite_otf_scan, donnees, masque_base_str, model=model_str, donnees_red, rms_noise, error=error_scan, /med_base, conv=init_obs_str.Conv  
		  endelse 
		endif else begin
		  if keyword_set(rmode) then begin         ; si rmode est specifie
		   traite_otf_scan, donnees, masque_base_str, model=model_str, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv  		  
		  endif else begin		
		   traite_otf_scan, donnees, masque_base_str, model=model_str, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv  
		  endelse 	
		endelse
	
	       error_otf_scan(i) = error_scan
	
ENDIF


IF init_obs_str.type eq 'decorrel' or (init_obs_str.type eq 'chmap' and (count eq 0 or keyword_set(newreduc))) or (init_obs_str.type eq 'map' and (count eq 0 or keyword_set(newreduc)or keyword_set(rmode)))$

THEN BEGIN                                 ; pour toute nouvelle reduction

echant = 3    ; facteur de rebin pour la construction du scan				

   if keyword_set(do_rcp) then begin          ; si l'on souhaite utiliser les fichiers de distorsion optique
	
   do_map_scan, donnees_red, positions, out_ima_dim, echant=echant, dx_rcp=init_obs_str.dx_rcp, dy_rcp=init_obs_str.dy_rcp, $           ; creation d'une carte pour un subscan 
   /do_rcp, carte_scan

   endif else begin                           ; si l'on ne souhaite pas utiliser les fichiers de distorsion optique

   do_map_scan, donnees_red, positions, out_ima_dim, echant=echant, carte_scan                                                          ; creation d'une carte pour un subscan 

   endelse

;;;;;;  IF THE NOISE HAS NOT BEEN SIMULATED, USE NOISE AND THUS WEIGHTS OF REAL DATA

if not keyword_set(rms_uncorr) and not keyword_set(rms_corr) then begin
   carte_scan.weight = carte_scan_reelle.weight
endif   

;;;;;;


;        donnees = donnees_save                                                
        if error_scan eq 0 then begin
			subscanname = init_obs_str.subscan_name(i)

       		if count ge 1 and keyword_set(newreduc) and init_obs_str.type eq 'decorrel' then begin           ; les données réduites existaient et l'on souhaitait itérer la décorrélation

			save, filename=filename, subscanname, carte_scan, donnees_uncal, donnees_cal, donnees_red0, donnees_red      ; sauvegarde des differentes versions des donnees
			print, '-----> ' + filename
			print, 'Saving both iterations of donnees_red'	 
                endif else begin

			save, filename=filename, subscanname, carte_scan, donnees_uncal, donnees, donnees_red                        ; sauvegarde des donnees pour toute 
			print, '-----> ' + filename                                                                                  ; nouvelle reduction
		endelse	
	endif else begin
			print, "Error treating subscan ", i	   
        endelse	   
				;  --> Map construction in RA-DEC

ENDIF

create_champ, donnees=donnees, carte_scan=carte_scan, error_scan=error_scan, subscan_liste=init_obs_str.subscan_liste, subscan_name=init_obs_str.subscan_name, i=i, type=init_obs_str.type, parang0, champ
; remplit la structure champ


if init_obs_str.type eq 'decorrel' or init_obs_str.type eq 'map' then begin     ; type 'DECORREL' ou 'MAP'

    if error_scan eq 0 then begin		
		
		champ1 = [champ1, champ]
    endif
	
endif
	
if init_obs_str.type eq 'chmap' then begin					; type 'CHMAP'

               champ1 = [champ1, champ]

endif

ENDIF ; si les rawdata sont correctes
	
ENDFOR


if init_obs_str.type eq 'decorrel' or init_obs_str.type eq 'map' then begin     ; type 'DECORREL' ou 'MAP'

med_scan_min = median(scan_min_longoff)		; arcsec
med_scan_max = median(scan_max_longoff)		; arcsec

;offset_str = {scan_min_longoff:scan_min_longoff, scan_max_longoff:scan_max_longoff, med_scan_min:med_scan_min, med_scan_max:med_scan_max}

offset_str = {scan_min_longoff:scan_min_longoff, scan_max_longoff:scan_max_longoff, med_scan_min:med_scan_min, med_scan_max:med_scan_max, scan_min_alpha:scan_min_alpha, scan_max_alpha:scan_max_alpha}

; map making 

map_make, scan_list=init_obs_str.scan_number, offset_str=offset_str, donnees=donnees, carte_scan=carte_scan, champ1=champ1,$
error_otf_scan=error_otf_scan, parang0=parang0, project_=project, mapradecstr, removedrift=0, /model
	
endif


if init_obs_str.type eq 'chmap' then begin                                     ; type 'CHMAP'

map_make_ch, champ1=champ1, donnees2=donnees, donnees_red=donnees_red, carte_scan=carte_scan, parang0=parang0, $
init_obs_str=init_obs_str, project=project, mapradecstr, /model

; map making dans le cas chmap, une carte est cree pour chaque pixel 

endif

return
end
