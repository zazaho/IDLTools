;+
; NAME:
;	COMBINE_OTF_MAP
;
; PURPOSE:
;
;	Combine a list of reduced data from different observations for a single
;	object on a map. Uncorrelated 1/f noise can be removed as an option.
;
; CALLING SEQUENCE:
;
;	COMBINE_OTF_MAP, Scan_list, Mapradecstr
;
; INPUTS:
;
;	Scan_list:	List of observations.
;
; OPTIONAL INPUTS:
;
;	Project_list:	List of projects.
;			
; KEYWORD PARAMETERS:
;
;	DO_RCP:		Use optical distorsion file.
;		
;	REMOVEDRIFT:	Subtract 1/f uncorrelated noise.
;		
; OUTPUTS:
;
;	Mapradecstr:	Output map structure.
;	
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project name and calibration
;			table.
;	
; EXAMPLE:
;	
;		COMBINE_OTF_MAP, [49007,49008], Gum31_5_comb_str
;
;		COMBINE_OTF_MAP, [47536,48822,48825,49000,49281,47527], Ngc3576_comb_mask_str
;
;		COMBINE_OTF_MAP, [48814], Ngc2264c_48814_rdrift, /REMOVEDRIFT, /DO_RCP
;
;		COMBINE_OTF_MAP, [48814,48971], Ngc2264c_48814_48971_rdrift, /REMOVEDRIFT, /DO_RCP
;
; MODIFICATION HISTORY:
; 	
;-



pro combine_otf_map, scan_list, mapradecstr, removedrift=removedrift, do_rcp=do_crp, project_list=project_list



if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	COMBINE_OTF_MAP, scan_list, mapradecstr, removedrift=removedrift, do_rcp=do_crp, project_list=project_list'
	return
endif


COMMON obs1_configb, work_dir, project_name, calibration_table  

init_obs, scan_number=scan_list[0], type='map', init_obs_str

; Usage:
; combine_otf_map, [49007,49008], gum31_5_comb_str
; combine_otf_map, [48986,49182], chamms1_comb_str
; combine_otf_map, [49170,48973], s255_comb_decorrel_str
; combine_otf_map, [49170,48973], s255_comb_str
; combine_otf_map, [48814,49174,48971], ngc2264c_comb_str
; combine_otf_map, [49286,49021,48832], g327_comb_mask_str
; combine_otf_map, [47536,48822,48825,49000,49281,47527], ngc3576_comb_mask_str
; combine_otf_map, [47527,47536],  ngc3576_comb_47527_47536_str
; combine_otf_map, [47536,48822,48825,49000,47527], ngc3576_comb_str
; combine_otf_map, [47536,48822], ngc3576_test_comb_str




print, 'scan list : ', scan_list

numb_scan = n_elements(scan_list)

;;;;
;;;; 1/f uncorrelated noise removal (H. Roussel)
;;;;

if not keyword_set(removedrift) then removedrift='0' else  removedrift='1'

if removedrift eq '1' then begin


;fichier_otf_scan = findfile(filename2 ,count=count)

	;if count eq 0 then begin


dir_in=init_obs_str.apexdata+'map_otf_xdr/'+init_obs_str.project_name+'/'

dir_rcp=init_obs_str.work_dir+'Calib_partemis'+'/'

;remove_uncordrifts, visu=1, dir_in=dir_in, num_scans=scan_list, nsubscans=150, pfov=6.2, $
;                         dir_rcp=dir_rcp

;if not keyword_set(pfov) then pfov = 6.2

if init_obs_str.pfov EQ '' then init_obs_str.pfov = 6.2


;help, pfov

remove_uncordrifts, visu=1, dir_in=dir_in, num_scans=scan_list, nsubscans=150, pfov=init_obs_str.pfov, $
                         dir_rcp=dir_rcp


echant = 3    ; facteur de rebin pour la construction du scan			
	
	
for iscan = 0, numb_scan-1 do begin
  scan_number = scan_list(iscan)
  
init_obs, scan_number=scan_number, type='map', init_obs_strr
  
obsname = 'APEX-' + strtrim(string(scan_number),1)				;  Nov 2007

dirdef = init_obs_str.apexdata + 'basic_xdr/'



project_name=init_obs_str.project_name

;if keyword_set(project) EQ 1 then begin
if keyword_set(project_list) EQ 1 then begin

project=project_list[0]

   if n_elements(project_list) GT 1 then begin
   project=project_list[iscan]
   endif

endif else begin

project=project_name

endelse

;basic_name = dirdef + project_name +'_' + strtrim(string(scan_number),1)

;subscan_liste = findfile(basic_name + '_*', count=nfiles)

;sc_liste_pos=strlen(basic_name)+1;           ; ou extraire le subscan

;subscan_liste = strmid(subscan_liste,sc_liste_pos,3)                              ;  for all home directories

subscan_liste = init_obs_strr.subscan_liste

;for i = 0, n_elements(subscan_liste)-1 do subscan_liste(i) = (strsplit(subscan_liste(i),'.',/extract))(0)
;subscan_liste = subscan_liste(sort(long(subscan_liste)))

subscan_name = project_name +'/' + strtrim(string(scan_number), 2) + '_' + subscan_liste

  for i=0, n_elements(subscan_liste)-1 do begin
  
;  donnees=read_apex(scan_number, subscan_liste(i), filetype, calibration_camera=init_obs_str.calibration_camera, project_list=project_name)
  
    
;	command = donnees.obslog.command
;	command = strmid(command,strpos(command,'angle='))
;	if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
;	 	angle = (strsplit(command,',',/extract))(0)
;		angle =  float(strmid(angle,strpos(angle,'=')+1))	; Rotation angle (deg) of scanning direction relative to Az axis
;	endif
  
  
  filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
  filename2=init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_nodrifts_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr' ; contient donnees_red, produit
                                                                                                                                             ; par remove_uncordrifts
  
  restore, filename=filename
  donnees_red_init = donnees_red
  restore, filename=filename2


if keyword_set(do_rcp) then begin
	
do_map_scan, donnees_red, positions, out_ima_dim, echant=echant, dx_rcp=init_obs_strr.dx_rcp, dy_rcp=init_obs_strr.dy_rcp, $
/do_rcp, carte_scan

endif else begin

do_map_scan, donnees_red, positions, out_ima_dim, echant=echant, carte_scan

endelse
	


subscanname = subscan_name(i)

save, filename=filename2, subscanname, carte_scan, donnees_uncal, donnees, donnees_red_init, donnees_red    ; ecrase le filename2 precedent
print, '-----> ' + filename2
	


endfor

endfor

endif



;;;;
;;;;
;;;;

dirdef = init_obs_str.apexdata + 'basic_xdr/'

chaine = ''

champ_scan  = [{scan_name : '', alpha_min : 0. , alpha_max : 0., $
                        delta_min : 0. , delta_max : 0., cdelt1: 0. , cdelt2:0. }]
champ1 = [champ_scan]

parang0 = 0.

scan_min_longoff = fltarr(150,numb_scan)
scan_max_longoff = fltarr(150,numb_scan)

med_scan_min = fltarr(numb_scan)
med_scan_max = fltarr(numb_scan)

for iscan = 0, numb_scan-1 do begin
  scan_number = scan_list(iscan)
  print, 'scan number : ', scan_number
  
  champ  = [{subscan_name : '', alpha_min : 0. , alpha_max : 0., $
                        delta_min : 0. , delta_max : 0., cdelt1: 0. , cdelt2:0. }]
  champ2 = [champ]


init_obs, scan_number=scan_number, type='map', init_obs_strr

obsname = 'APEX-' + strtrim(string(scan_number),1)				;  Nov 2007

project_name=init_obs_str.project_name


;if keyword_set(project) EQ 1 then begin
if keyword_set(project_list) EQ 1 then begin

project=project_list[0]

   if n_elements(project_list) GT 1 then begin
   project=project_list[iscan]
   endif

endif else begin

project=project_name

endelse


;basic_name = dirdef + project_name +'_' + strtrim(string(scan_number),1)

;subscan_liste = findfile(basic_name + '_*', count=nfiles)

;sc_liste_pos=strlen(basic_name)+1;           ; ou extraire le subscan

;subscan_liste = strmid(subscan_liste,sc_liste_pos,3)                              ;  for all home directories


;for i = 0, n_elements(subscan_liste)-1 do subscan_liste(i) = (strsplit(subscan_liste(i),'.',/extract))(0)
;subscan_liste = subscan_liste(sort(long(subscan_liste)))

subscan_liste =init_obs_strr.subscan_liste

subscan_name = project_name +'/' + strtrim(string(init_obs_str.scan_number), 2) + '_' + subscan_liste

  for i=0, n_elements(subscan_liste)-1 do begin
  

  ;  donnees=read_apex(scan_number, subscan_liste(i), filetype, calibration_camera=init_obs_str.calibration_camera)
   
   
        if removedrift eq '0' then begin
	filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	endif else begin
	filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_nodrifts_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	endelse
	
	
	
	fichier_otf_scan = findfile(filename ,count=count)

	if count eq 0 then begin
		print, 'Warning:  Unreduced scan ????'
		traite_otf_scan, donnees,image_modele, carte_scan, donnees_red, rms_noise
		subscanname = subscan_name(i)
		save, filename=filename, subscanname, carte_scan, donnees, donnees_red
		print, '-----> ' + filename
	endif else begin
		restore, filename=filename
		print, '<----- ' + filename
	;	subscan_name(i) = subscanname
;	    if donnees.unit ne 'Jy/beam (flat field corr.)' then begin
	    if datatype(donnees) NE 'STC'  then donnees = donnees_red
;	    
	    if donnees.unit ne 'Jy/beam ext. corr.' then begin
	       print, 'Warning:  Uncalibrated data ????'
	       print, donnees.unit	
            endif
	endelse
   
   
   
    
	command = donnees.obslog.command
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

	if ((size(angle))(1) gt 1) then begin			;   if angle is defined ...
	  if (angle ne 0.0) then begin
	    print, "Scanning at angle ", angle, " (deg) with respect to Azimuth axis"  
	    angle = angle*!pi/180.d0				;  angle in radians
	    scan_min_longoff(i,iscan) = min(cos(angle)*donnees.datapar(id).longoff+sin(angle)*donnees.datapar(id).latoff)*3600.	; min offset along scanning direction (arcsec)   
	    scan_max_longoff(i,iscan) = max(cos(angle)*donnees.datapar(id).longoff+sin(angle)*donnees.datapar(id).latoff)*3600.	; max offset along scanning direction (arcsec)
	  endif else begin
	    scan_min_longoff(i,iscan) = min(donnees.datapar(id).longoff*3600.)	; arcsec
	    scan_max_longoff(i,iscan) = max(donnees.datapar(id).longoff*3600.)	; arcsec
	  endelse
	endif else begin
	    scan_min_longoff(i,iscan) = min(donnees.datapar(id).longoff*3600.)	; arcsec
	    scan_max_longoff(i,iscan) = max(donnees.datapar(id).longoff*3600.)	; arcsec
	endelse    
    
        ;if removedrift eq '0' then begin
	;filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	;endif else begin
	;filename= init_obs_str.apexdata+'map_otf_xdr/'+project+'/otf_subscan_nodrifts_'+strtrim(string(scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	;endelse
	
	
	
	;fichier_otf_scan = findfile(filename ,count=count)

	;if count eq 0 then begin
	;	print, 'Warning:  Unreduced scan ????'
	;	traite_otf_scan, donnees,image_modele, carte_scan, donnees_red, rms_noise
	;	subscanname = subscan_name(i)
	;	save, filename=filename, subscanname, carte_scan, donnees, donnees_red
	;	print, '-----> ' + filename
	;endif else begin
	;	restore, filename=filename
	;	print, '<----- ' + filename
	;;	subscan_name(i) = subscanname
	;    if donnees.unit ne 'Jy/beam (flat field corr.)' then begin
	;       print, 'Warning:  Uncalibrated data ????'
	;       print, donnees.unit	
        ;    endif
	;endelse
	
	create_champ, donnees=donnees, carte_scan=carte_scan, error_scan=0, subscan_liste=subscan_liste, subscan_name=subscan_name, i=i, type='map', parang0, champ
	
	
	champ2 = [champ2, champ]
		
  endfor    

  champ2 = champ2(1:*)
  med_scan_min(iscan) = median(scan_min_longoff(0:n_elements(subscan_liste)-1,iscan))		; arcsec
  med_scan_max(iscan) = median(scan_max_longoff(0:n_elements(subscan_liste)-1,iscan))		; arcsec
  ind = where((abs(scan_min_longoff(*,iscan)-med_scan_min(iscan)) lt 20.) and (abs(scan_max_longoff(*,iscan)-med_scan_max(iscan)) lt 20.), count)
  
  cdelt1_min = min(champ2.cdelt1)   &   cdelt1_max = max(champ2.cdelt1)
  cdelt2_min = min(champ2.cdelt2)   &   cdelt2_max = max(champ2.cdelt2)

  if cdelt1_min ne cdelt1_max or cdelt2_min ne cdelt2_max then begin
	print, ' CDELT1 ou CDELT2 non constants !!!'
	print, 'ON STOPPE ICI...
	return	
        stop
  endif else begin
	champ_scan.cdelt1 = cdelt1_min
	champ_scan.cdelt2 = cdelt2_min
  endelse
  
  champ_scan.scan_name = scan_number    
  champ_scan.alpha_min = min(champ2(ind).alpha_min)
  champ_scan.alpha_max = max(champ2(ind).alpha_max)
  champ_scan.delta_min = min(champ2(ind).delta_min)
  champ_scan.delta_max = max(champ2(ind).delta_max)

  champ1 = [champ1, champ_scan]  

endfor

;offset_str = {scan_min_longoff:scan_min_longoff, scan_max_longoff:scan_max_longoff, med_scan_min:med_scan_min, med_scan_max:med_scan_max}

offset_str = {scan_min_longoff:scan_min_longoff, scan_max_longoff:scan_max_longoff, med_scan_min:med_scan_min, med_scan_max:med_scan_max, scan_min_alpha:-999., scan_max_alpha:-999.}

print, removedrift
print, removedrift


map_make, scan_list=scan_list, offset_str=offset_str, donnees=donnees_red, carte_scan=carte_scan, champ1=champ1,$
error_otf_scan=0*indgen(150), parang0=parang0, project_=project, mapradecstr, removedrift=removedrift       


	       
return
end
