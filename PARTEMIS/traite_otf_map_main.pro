;+
; NAME:
;	TRAITE_OTF_MAP_MAIN
;
; PURPOSE:
;
;	Reduce data for a given observation and build a map.	
;
; CALLING SEQUENCE:
;
;	TRAITE_OTF_MAP_MAIN, Scan_number=Scan_number, Type=Type, Mapradecstr
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
;	Dir_dat:	Data path.
;	Tau:		Sky opacity.
;	Flat:		Flat field array.
;	Rmode:		Wobbler restoration mode(SAA, EKH OR TP).
;	Project:	Alternative sub-directory name to store reduced data.
;	
; KEYWORD PARAMETERS:
;
;	MED_NOISE_REM:	Remove median noise.
;		
;	MED_BASE:	Remove baseline.
;		
;	NEWREDUC:	Do new data reduction.
;
;	NOPOWERMAP:	Do not calibrate from V to pW.
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
;		TRAITE_OTF_MAP_MAIN, Scan_number= 4792, Type= 'MAP', Jupiter_4792_str, Tau=0.49, /NEWREDUC, /DO_RCP
;	
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48990, Type= 'MAP', Hd97048_48990_tp_str, Tau=0.71, Rmode = 'TP', /DO_RCP, /NEWREDUC
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48990, Type= 'DECORREL', Hd97048_48990_tp_decorrel_str, Tau=0.71, Rmode = 'TP', /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48990, Type= 'DECORREL', Hd97048_48990_saa_decorrel_str, Tau=0.71, Rmode = 'SAA', /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48990, Type= 'DECORREL', Hd97048_48990_wob_decorrel_str, Tau=0.71, Rmode = 'EKH', /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 50010, Type= 'MAP', Map_50010_str, Tau=0.8,/NEWREDUC, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 50010, Type= 'CHMAP', Mars_ch_map_50010_str, Tau=0.8,/NEWREDUC 
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 49020, Type= 'MAP', Map_49020_str, Tau=1.1,/NEWREDUC, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 49020, Type= 'DECORREL', Map_decorrel_49020_str, Tau=1.1
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 49020, Type= 'DECORREL', Map_decorrel_mod_49020_str, Champ_base='G34_mask_str.xdr', Model='G34_model_str.xdr', Champ_masque='G34_mask_str.xdr', Tau=1.1, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 47543, Type= 'DECORREL', Map_decorrel_47543_str, Champ_base='Bhr71_mask_str.xdr', Model='Bhr71_model_str.xdr', Champ_masque='Bhr71_mask_str.xdr', Tau=0.6
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48973, Type= 'DECORREL', S255_48973_decorrel_str, Champ_base='S255_mask_base_str.xdr', Model='S255_model_str.xdr', Champ_masque='S255_mask_str.xdr',Tau=0.825, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 49170, Type= 'DECORREL', S255_49170_decorrel_str, Champ_base='S255_mask_base_str.xdr', Model='s255_model_str.xdr', Champ_masque='S255_mask_str.xdr', Tau=0.9,/DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48814, Type= 'DECORREL', Ngc2264c_48814_decorrel_str, Champ_base='Ngc2264c_mask_base_str.xdr', Model='Ngc2264c_model2_str.xdr', Champ_masque='Ngc2264c_mask2_str.xdr', Tau=1.04,/DO_RCP 
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 48971, Type= 'DECORREL', Ngc2264c_48971_decorrel_str, Champ_base='Ngc2264c_mask_base_str.xdr', Model='ngc2264c_model2_str.xdr', Champ_masque='Ngc2264c_mask2_str.xdr', Tau=0.91,/DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 4825, Type= 'DECORREL', N3576_4825_dec_mod2_str, Champ_base='N576_4825_mask_base.xdr', Model='n3576_4825_model2_str.xdr', Champ_masque='n3576_4825_mask.xdr', Tau=0.65, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 4828, Type= 'DECORREL', N3576_4828_dec_mod2_str, Champ_base='N3576_4828_mask_base.xdr', Model='n3576_4828_model2_str.xdr', Champ_masque='n3576_4828_mask.xdr', Tau=0.8, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 47527, Type= 'DECORREL', N3576_47527_dec_mod2_str, Champ_base='N3576_47527_mask_base.xdr', Model='n3576_47527_model2_str.xdr', Champ_masque='N3576_47527_mask.xdr', Tau=0.79, /DO_RCP
;
;		TRAITE_OTF_MAP_MAIN, Scan_number= 47536, Type= 'DECORREL', N3576_47536_dec_mod2_str, Champ_base='n3576_47536_mask_base.xdr', Model='N3576_47536_model2_str.xdr', Champ_masque='N3576_47536_mask.xdr', Tau=0.73, /DO_RCP
;
; MODIFICATION HISTORY:
;
;-

pro traite_otf_map_main , scan_number=scan_number, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, model=model, med_noise_rem = med_noise_rem,$
med_base = med_base, dir_dat=dir_dat, tau=tau, flat=flat, newreduc=newreduc, nopowermap=nopowermap, rmode=rmode, project=project, do_rcp=do_rcp


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	TRAITE_OTF_MAP_MAIN , scan_number=scan_number, type=type, mapradecstr, champ_masque=champ_masque,champ_base=champ_base, model=model, med_noise_rem = med_noise_rem, med_base = med_base, dir_dat=dir_dat, tau=tau, flat=flat, newreduc=newreduc, nopowermap=nopowermap, rmode=rmode, project=project, do_rcp=do_rcp'

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

 if init_obs_str.type eq 'decorrel' then begin                                       ; type 'DECORREL' 



  if keyword_set(model) then begin            				             ; utilisation d'un modele de source

    restore, init_obs_str.apexdata + 'map_otf_fits/' + model, /verb
    print, "model_str :"
    help, model_str, /str
    
  endif
 
 endif

ENDIF




if init_obs_str.type eq 'chmap' then begin					    ; type 'CHMAP'

chaine = ''

champ  = [{subscan_name : '', long_min : 0. , long_max : 0., $                      ; champ suivant le type 'CHMAP', on ne se preoccupe pas des dimensions
                        lat_min : 0. , lat_max : 0. , lat_mean : 0. }]              ; du champ de vue
endif



champ1 = [champ]

parang0 = 0.


FOR i=0, n_elements(init_obs_str.subscan_liste)-1 DO BEGIN

    if keyword_set(nopowermap) then begin         ; do not calibrate data from V to pW
    
        donnees=read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima,/nopowermap)
	
    endif else begin	                          ; calibrate data from V to pW or restore existing calibrated data
    
	donnees=read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)

    endelse
 
    
IF  datatype(donnees) EQ 'STC' then begin   
    
      if  ((init_obs_str.date[0] NE '') AND (fix(init_obs_str.date[0]) LE 2007)) then begin
    	if (donnees.source eq 'BHR71-MM') then begin
		donnees.movefram = 0
		Print, "Observing BHR71-MM, changing movefram to 0"
	endif
      endif
	if init_obs_str.type eq 'decorrel' or init_obs_str.type eq 'map' then begin          ; type 'DECORREL' ou 'MAP'
	 
	donnees.cdelt1=init_obs_str.pfov                                                     ; affectation de la valeur de la taille du pixel a donnees
	donnees.cdelt2=init_obs_str.pfov
	
	 ;if scan_number lt 5000 then begin
         ;  donnees.cdelt1	= 3.75	;  3.6  ;  3.65		    ; taille du pixel P-Artemis 450 microns a APEX en arcsec
         ;  donnees.cdelt2	= 3.75	;  3.6  ;  3.65		    ; en mars 2007
	 ;endif
	 
	 ;if scan_number gt 5000 and scan_number lt 47913 then begin
         ;  donnees.cdelt1	= 5.85	 			    ; taille du pixel P-Artemis 450 microns a APEX en arcsec
         ;  donnees.cdelt2	= 5.85   			    ; au debut du run de novembre 2007
	 ;endif  

	 ;if scan_number ge 47913 then begin
         ;  donnees.cdelt1	= 6.2					; taille du pixel P-Artemis 450 microns a APEX en arcsec
         ;  donnees.cdelt2	= 6.2					; pour la deuxieme partie du run de novembre 2007
	 ;endif
	
	endif
    
    donnees_uncal = donnees
        

    IF keyword_set(tau) and donnees.unit eq 'pW' THEN BEGIN                           ; si la valeur de tau(opacite atmospherique) est fournie et si
                                                                                       ; les donnees sont en pW
    	
;	print, "Calling pW2Jy"
	
	pW2Jy, donnees, donnees_Jy,tau=tau, Conv=init_obs_str.Conv                     ; conversion de pW en Jy et correction d'extinction
	
	donnees= donnees_Jy	
	
	if init_obs_str.type eq 'decorrel' or init_obs_str.type eq 'map' then begin    ; division des donnees par un flat field pour les type 'DECORREL' ou 'MAP'
          flat_cube = donnees.cube*0.
	  for kk=0L, (size(donnees.cube))(3)-1 do flat_cube(*,*,kk) = init_obs_str.flat_field
	  donnees.cube = donnees.cube/flat_cube		; Flat field correction
	  donnees.unit = 'Jy/beam ext. corr. and flat fielded'
	endif
	  		
    ENDIF
    
     donnees.goodpix_ima = init_obs_str.goodpix_ima                                    ; affectation du tableau goodpix_ima (bon pixel : 1, mauvais pixel : 0)
 
    
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


	
       filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(init_obs_str.scan_number),1)+'_'+init_obs_str.subscan_liste(i)+'.xdr'
	
       fichier_otf_scan = findfile(filename ,count=count)

;donnees_save = donnees	

if init_obs_str.type eq 'decorrel' then begin               ; si le type est DECORREL

       if count ge 1 and keyword_set(newreduc) then begin           ; si les données réduites existent déjà et que l'on souhaite itérer la décorrélation
		restore, filename=filename
		print, '<----- ' + filename
	;	subscan_name(i) = subscanname
		donnees_cal  = donnees 
		donnees_red0 = donnees_red
		donnees = donnees_red
		print, 'Iterating skynoise decorrelation'
       endif

	   if keyword_set(med_noise_rem) then begin         ; si l'on souhaite soustraire le bruit median correle
	   
	    if keyword_set(med_base) then begin
	      if keyword_set(rmode) then begin              ; si rmode est specifie
		traite_otf_scan, donnees, masque_base_str, model=model_str, masque_model=masque_modele_str, med_noise_rem = med_noise_rem, /med_base, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv	      
	      endif else begin
		traite_otf_scan, donnees, masque_base_str, model=model_str, masque_model=masque_modele_str, med_noise_rem = med_noise_rem, /med_base, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv
	      endelse
	    endif else begin
	      if keyword_set(rmode) then begin              ; si rmode est specifie
		traite_otf_scan, donnees, masque_base_str, model=model_str, masque_model=masque_modele_str, med_noise_rem = med_noise_rem, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv	      
	      endif else begin
		traite_otf_scan, donnees, masque_base_str, model=model_str, masque_model=masque_modele_str, med_noise_rem = med_noise_rem, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv
	      endelse
	    endelse  	

	   endif else if keyword_set(model) then begin      ; si un model est specifie
	   
	      if keyword_set(rmode) then begin
		traite_otf_scan, donnees, masque_base_str, model=model_str, masque_model=masque_modele_str, donnees_red, rms_noise, error=error_scan, rest=rmode, conv=init_obs_str.Conv    
	      endif else begin	      	                    ; si rmode est specifie
		traite_otf_scan, donnees, masque_base_str, model=model_str, masque_model=masque_modele_str, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv
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

        if count eq 0 or keyword_set(newreduc) then begin           ; si les donnees reduites n'existent pas ou si l'on souhaite realiser une nouvelle reduction
		traite_otf_scan, donnees,image_modele, donnees_red, rms_noise, error=error_scan, conv=init_obs_str.Conv  

	endif else begin
		restore, filename=filename
		print, '<----- ' + filename
	;	subscan_name(i) = subscanname
	endelse

ENDIF



IF init_obs_str.type eq 'map' THEN BEGIN		   ; si le type est MAP

  if count eq 0 or keyword_set(newreduc) or keyword_set(rmode) then begin ; si les donnees reduites n'existent pas ou si l'on souhaite realiser une nouvelle reduction ou si rmode
                                                                        ; est specifie
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

   endif else begin
   
		restore, filename=filename                 ; restauration des donnees reduites existantes
		print, '<----- ' + filename
	
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

   if count eq 0 or keyword_set(newreduc) or init_obs_str.type eq 'decorrel' then begin
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
   endif

ENDIF


create_champ, donnees=donnees, carte_scan=carte_scan, error_scan=error_scan, subscan_liste=init_obs_str.subscan_liste, subscan_name=init_obs_str.subscan_name, i=i, type=init_obs_str.type, parang0, champ
; remplit la struture champ


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
error_otf_scan=error_otf_scan, parang0=parang0, project_=project, mapradecstr, removedrift=0

	
	
	
endif

if init_obs_str.type eq 'chmap' then begin                                     ; type 'CHMAP'


map_make_ch, champ1=champ1, donnees2=donnees, donnees_red=donnees_red, carte_scan=carte_scan, parang0=parang0, $
init_obs_str=init_obs_str, project=project, mapradecstr

; map making dans le cas chmap, une carte est cree pour chaque pixel 



if keyword_set(flat)  then begin                                              ; affectation de flat_field a init_obs_str     ; !!! BUG --> flat to be calculated for ch_map !!!!
   flat = init_obs_str.flat_field
endif 	  


endif

return
end
