;+
; NAME:
;	MAP_MAKE
;
; PURPOSE:
;
;	Build a map from combining a list of subscan map.
;
; CALLING SEQUENCE:
;
;	MAP_MAKE, Scan_list=Scan_list, Offset_str=Offset_str, Donnees=Donnees, Carte_scan=Carte_scan, Champ1=Champ1, Error_otf_scan=Error_otf_scan, Parang0=Parang0, Mapradecstr
;
; INPUTS:
;
;	Scan_list:	List of scans.
;	Offset_str:	Parameters structure.
;	Donnees:	Input data structure.
;	Carte_scan:	Subscan map.
;	Error_otf_scan:	Error value.
;	Parang0:	Parallactic angle.
;	Champ1:		Contains the dimensions of the field of view in RA/DEC.		
;
; OPTIONAL INPUTS:
;
;	Project_:	Alternative project directory name for data storage purpose.
;	Removedrift:	Use 1/f uncorrelated noise subtracted data.				
;
; OUTPUTS:
;
;	Mapradecstr:	Map structure.
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name.
;		
; EXAMPLE:
;
;	MAP_MAKE, Scan_list=Scan_number, Offset_str=Offset_str, Donnees=Donnees, Carte_scan=Carte_scan, Champ1=Champ1, Error_otf_scan=Error_otf_scan, Parang0=Parang0, Project_=Project, Mapradecstr, Removedrift=0
;
;	MAP_MAKE, Scan_list=Scan_list, Offset_str=Offset_str, Donnees=Donnees_red, Carte_scan=Carte_scan, Champ1=Champ1, Error_otf_scan=0*indgen(150), Parang0=Parang0, Project_=Project, Mapradecstr, Removedrift=1       
;	
; MODIFICATION HISTORY:
; 	
;-

pro map_make, scan_list=scan_list, offset_str=offset_str, donnees=donnees, carte_scan=carte_scan, champ1=champ1,$
error_otf_scan=error_otf_scan, parang0=parang0, project_=project, mapradecstr, removedrift=removedrift, model=model

; build a map from combining a list of subscan map



if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	MAP_MAKE, scan_list=scan_list, offset_str=offset_str, donnees=donnees, carte_scan=carte_scan, champ1=champ1, error_otf_scan=error_otf_scan, parang0=parang0, project_=project, mapradecstr, removedrift=removedrift'
	return
endif


COMMON obs1_configb

date=strsplit(donnees.date_obs,'T',/extract)
date=date[0]
date_test=strsplit(date,'-',/extract)
date_test=fix(date_test[0])

numb_scan = n_elements(scan_list)

champ1 = champ1(1:*)

cdelt1_min = min(champ1.cdelt1)   &   cdelt1_max = max(champ1.cdelt1)
cdelt2_min = min(champ1.cdelt2)   &   cdelt2_max = max(champ1.cdelt2)
print, cdelt1_min
print, cdelt1_max
print, cdelt2_min
print, cdelt2_max
if cdelt1_min ne cdelt1_max or cdelt2_min ne cdelt2_max then begin
	print, ' CDELT1 ou CDELT2 non constants !!!'
	print, 'ON STOPPE ICI...'
        print, '.C POUR CONTINUER...'
        cdelt1 = cdelt1_min
	cdelt2 = cdelt2_min
        stop
	;return	
endif else begin
	cdelt1 = cdelt1_min
	cdelt2 = cdelt2_min
endelse


if numb_scan le 1 then begin

 ind = where((abs(offset_str.scan_min_longoff-offset_str.med_scan_min) lt 60.) and (abs(offset_str.scan_max_longoff-offset_str.med_scan_max) lt 60.), count_ss)
; print, "Taking ", count_ss, " subscans into account out of a total of ", n_elements(subscan_list)

 alpha_min = min(champ1(ind).alpha_min)
 alpha_max = max(champ1(ind).alpha_max)
 delta_min = min(champ1(ind).delta_min)
 delta_max = max(champ1(ind).delta_max)

endif else begin

 alpha_min = min(champ1(*).alpha_min)
 alpha_max = max(champ1(*).alpha_max)
 delta_min = min(champ1(*).delta_min)
 delta_max = max(champ1(*).delta_max)

endelse

; definition de la grille finale RADEC


alpha_cen = (alpha_max + alpha_min)/2.d0
delta_cen = (delta_max + delta_min)/2.d0

;;x_size_grid = 1.1*abs((alpha_max - alpha_min)*cos(delta_cen*!pi/180.)/carte_scan.cdelt1)
;;y_size_grid = 1.1*abs((delta_max - delta_min)/carte_scan.cdelt2)

;x_size_grid = 1.2*abs((alpha_max - alpha_min)*cos(delta_cen*!pi/180.)/carte_scan.cdelt1)
;y_size_grid = 1.2*abs((delta_max - delta_min)/carte_scan.cdelt2)

x_size_grid = 1.35*abs((alpha_max - alpha_min)*cos(delta_cen*!pi/180.)/carte_scan.cdelt1)
y_size_grid = 1.35*abs((delta_max - delta_min)/carte_scan.cdelt2)

;help, x_size_grid
;help, y_size_grid
;help, alpha_cen
;help, delta_cen
;help, alpha_min
;help, alpha_max
;
map_radec = fltarr(x_size_grid, y_size_grid)
weight_radec = map_radec

IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body  +  Scanning in Az
									;  --> Map construction in Az-El	
	print, 'Parallactic angle (deg) at central scan = ', parang0
;
	alpha_ref = 0.d0  ; double(alpha_cen);   alpha_cen		;    Central/Ref Azimuth
	delta_ref = 0.d0  ; double(delta_cen); 0.d0  ;   delta_cen		;    Central/Ref Elevation
	crpix1 = (alpha_ref - alpha_cen)/cdelt1 + x_size_grid/2.
	crpix2 = (delta_ref - delta_cen)/cdelt2 + y_size_grid/2.		
ENDIF ELSE BEGIN
     	;  Normal source or Planet in RA
	alpha_ref = double(donnees.blongobj)
	delta_ref = double(donnees.blatobj)
	crpix1 = (alpha_ref - alpha_cen)*cos(delta_ref*!pi/180.d0)/cdelt1 + x_size_grid/2.d0		; true angle on sky	
	crpix2 = (delta_ref - delta_cen)/cdelt2 + y_size_grid/2.d0	
ENDELSE


if not keyword_set(project_) then begin

project=project_name

endif else begin

project=project_

endelse

FOR iscan = 0, numb_scan-1 DO BEGIN     ; pour tous les scans

scan_number = scan_list(iscan)
print, 'scan number : ', scan_number

if numb_scan gt 1 then begin 
 ind = where((abs(offset_str.scan_min_longoff(*,iscan)-offset_str.med_scan_min(iscan)) lt 20.) and (abs(offset_str.scan_max_longoff(*,iscan)-offset_str.med_scan_max(iscan)) lt 20.), count_ss)
endif

apexdata = work_dir+'apexdata/'

otf_dat = apexdata + 'map_otf_xdr/'+project+'/'

if keyword_set(model) then begin
	otf_liste = findfile(otf_dat + 'otf_subscan_mod_' + strtrim(string(scan_number),1)+'_*.xdr', count=n_otf)	
endif else begin
	otf_liste = findfile(otf_dat + 'otf_subscan_' + strtrim(string(scan_number),1)+'_*.xdr', count=n_otf)
endelse

obsname = 'APEX-' + strtrim(string(scan_number),1)

dirdef = apexdata + 'basic_xdr/'
  

;basic_name = dirdef + project_name + '_' + strtrim(string(scan_number),1)
basic_name = dirdef + '*' + '_' + strtrim(string(scan_number),1)


subscan_list = findfile(basic_name + '_*', count=nfiles)
  
  ;sc_liste_pos=strlen(basic_name)+1;           ; ou extraire le subscan
sc_liste_pos=min(strlen(subscan_list))-1-4					  ; BUG ! (if one wants to select only subscans > 9)
;sc_liste_pos=min(strlen(subscan_list))-2-4					  ; To select subscans > 17 of scan 22909

  subscan_list = strmid(subscan_list,sc_liste_pos,3)                              ;  for all home directories

for i = 0, n_elements(subscan_list)-1 do begin

subscan_list(i) = (strsplit(subscan_list(i),'.',/extract))(0)

subscan_list(i) = (strsplit(subscan_list(i),'_',/extract))

endfor

subscan_list = subscan_list(sort(long(subscan_list)))	

     if numb_scan le 1 then begin
        scan_max_weight = fltarr(n_elements(subscan_list))
        scan_mean_weight = fltarr(n_elements(subscan_list))
     endif

print, "Taking ", count_ss, " subscans into account out of a total of ", n_elements(subscan_list)     

FOR i = 0, n_elements(subscan_list)-1 DO BEGIN          ; pour tous les subscans
	
	if removedrift eq '0' then begin	
		if keyword_set(model) then begin
	  		filename=otf_dat+'otf_subscan_mod_'+strtrim(string(scan_number),1)+'_'+subscan_list(i)+'.xdr'		
		endif else begin
	  		filename=otf_dat+'otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_list(i)+'.xdr'
		endelse
;	  filename=otf_dat+'otf_subscan_'+strtrim(string(scan_number),1)+'_'+subscan_list(i)+'.xdr'
	endif else begin
	  filename=otf_dat+'otf_subscan_nodrifts_'+strtrim(string(scan_number),1)+'_'+subscan_list(i)+'.xdr'   ; donnees reduite avec soustraction
	endelse                                                                                              ; du bruit en 1/f non correle
	restore, filename  	;  (subscan_name, carte_scan, donnees)                                       ; restauration des donnees reduites

	print, "Subscan # ", subscan_list(i), "   Max weight : ", max(carte_scan.weight), "   Mean weight : ", mean(carte_scan.weight)
 	print, "                    Min carte_scan : ", min(carte_scan.image), "    Max carte_scan : ", max(carte_scan.image)
	
        if numb_scan lt 1 then begin
	scan_max_weight(i)  = max(carte_scan.weight)
	scan_mean_weight(i) = mean(carte_scan.weight)	
        endif else begin
        ;ind_weight = where(carte_scan.weight lt 0.01*max(carte_scan.weight), count_w)
        ;if count_w gt 0 then carte_scan.weight(ind_weight) = 0.
        endelse

	
	IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body
									;  --> Map construction in Az-El	

	        rot_angle = -carte_scan.elevatio        				; degrees	;  APEX


		parang  = carte_scan.crota1 - carte_scan.elevatio			; Parallactic angle	APEX

		dparang = parang - parang0						; Delta parang / central scan

		rot_angle = rot_angle - dparang

	ENDIF ELSE BEGIN						;  Normal source or Planet in RA
									;  --> Map construction in RA-DEC
	        rot_angle = -carte_scan.crota1	                                ;  -(parang + El)  (degrees)   APEX
		parang  = carte_scan.crota1 - carte_scan.elevatio		; Parallactic angle    APEX												

	ENDELSE	
	
        

	carte_rot = rot(carte_scan.image, rot_angle, 1, $
	                carte_scan.crpix1, carte_scan.crpix2, /interp)
	weight_rot = rot(carte_scan.weight, rot_angle, 1, $
	                carte_scan.crpix1, carte_scan.crpix2, /interp)
	
	dim = size(carte_scan.image)

	IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body + Scanning in Az
										;  --> Map construction in Az-El

;	delta_az0, delta_el0 sont les offsets Az, El du pixel (0,0) de carte_rot
;				   par rapport au centre de carte_rot 								

	delta_az0 = (-(dim(1)-1)/2.)*abs(carte_scan.cdelt1) 	; angle vrai degrees	carte_scan.cdelt1 est defini negatif dans do_map_scan  
	delta_el0 = (-(dim(2)-1)/2.)*carte_scan.cdelt2

;	delta_azim0, delta_elev0 sont les offsets Az, El du pixel (0,0) de carte_rot
;	    (en degres)			          par rapport a la source pointee

	delta_azim0  = carte_scan.daz_scan/3600. + delta_az0			; angle vrai degrees
	delta_elev0  = carte_scan.del_scan/3600. + delta_el0	

	x0 = nint((delta_azim0 - alpha_ref)/cdelt1 + crpix1)			; angle vrai en unites de pixels
	y0 = nint((delta_elev0 - delta_ref)/cdelt2 + crpix2)
;
; 	Dans le cas "planete", alpha_ref, delta_ref sont les offsets (Az, El) du centre de la carte reconstruite
;				par rapport a la source pointee

	print, 'rot_angle (deg) : ', rot_angle
	print, 'delta_parang /central scan (deg) : ', dparang
	print, 'crpix1    : ', carte_scan.crpix1,    ' crpix2    : ', carte_scan.crpix2
	print, 'scan_alpha_ref : ', carte_scan.alpha_ref, '   scan_delta_ref : ', carte_scan.delta_ref
	print, 'delta_az0 (deg) : ', delta_az0,  ' delta_el0 (deg) : ', delta_el0
	print, 'daz_scan (arcsec) : ', carte_scan.daz_scan,  ' del_scan (arcsec) : ', carte_scan.del_scan
	print, 'delta_azim0 (arcsec)  : ', delta_azim0*3600.,   ' delta_elev0 (arcsec)   : ', delta_elev0*3600.										
		
	ENDIF ELSE BEGIN						;  Normal source
									;  --> Map construction in RA-DEC
	delta_a0 = (-(dim(1)-1)/2.)*carte_scan.cdelt1         ; true angle on sky in deg !!                   carte_scan.cdelt1 is negative (defined by do_map_scan) 
	delta_d0 = (-(dim(2)-1)/2.)*carte_scan.cdelt2

	alpha_0  = carte_scan.alpha_ref + delta_a0/cos(carte_scan.delta_ref*!pi/180.)	   ; angle on sphere
	delta_0  = carte_scan.delta_ref + delta_d0	
        
;;;;
;;;;     POINTING CORRECTIONS :
;;;;

    if date_test le 2007 then begin
	 if scan_number eq 4806 then begin
	    alpha_0 = alpha_0 - 13./3600./cos(delta_0*!pi/180.)			; angle on sphere in deg
	    delta_0 = delta_0 - 12./3600.	
	 endif
;	 
	 if scan_number eq 4832 then begin
	    alpha_0 = alpha_0 - 4.4/3600./cos(delta_0*!pi/180.)			; angle on sphere in deg
	    delta_0 = delta_0 - 7.6/3600.	
	 endif	 
    endif

	x0 = nint((alpha_0 - alpha_ref)*cos(delta_ref*!pi/180.)/cdelt1 + crpix1)	   ; true angle on sky expressed in units of pixels
	y0 = nint((delta_0 - delta_ref)/cdelt2 + crpix2)


	print, 'rot_angle (deg) : ', rot_angle						;;;;;	NB:  rot_angle = -carte_scan.crota1
	print, 'Parallactic angle (deg) : ', parang
	print, 'crpix1    : ', carte_scan.crpix1,    ' crpix2    : ', carte_scan.crpix2
	print, 'scan_alpha_ref : ', carte_scan.alpha_ref, ' scan_delta_ref : ', carte_scan.delta_ref
	print, 'delta_a0  : ', delta_a0,  ' delta_d0  : ', delta_d0
	print, 'alpha_0   : ', alpha_0,   ' delta_0   : ', delta_0
	
	crota1 = -rot_angle
	dx_0 = +10.			; Offsets (arcsec) of flat-field centroid in array coordinates
	dy_0 = +8.
	rot_dxdy2draddec, dx_0, dy_0, crota1, dra_0, ddec_0
	print, 'RA, Dec Offsets of most sensitive part of the array: delta_RA (arcsec) = ',dra_0, '        delta_DEC (arcsec) = ', ddec_0
	
	ENDELSE
         
	if (numb_scan gt 1) then begin
	  off_max = 20
	endif else begin   
	  off_max = 60
	endelse
	
	if (strpos(donnees.obslog.strokemode,'ARC') eq 0) then begin
	  off_max = 36000.
	endif
	
        IF (abs(offset_str.scan_min_longoff(i,iscan)-offset_str.med_scan_min(iscan)) lt off_max) and (abs(offset_str.scan_max_longoff(i,iscan)-offset_str.med_scan_max(iscan))$
	 lt off_max and error_otf_scan(i) eq 0) THEN BEGIN
	
	IF donnees.movefram ne 1 and donnees.scantype ne 'POINT' THEN BEGIN	     ; Ignoring regions of low weight when the source is NOT a Planet (or a strong source)
	  max_weight=max(weight_rot)
;	  ind=where(weight_rot lt 0.5*max_weight, count)
;	  ind = where(weight_rot lt 0.3*max_weight, count)
	  ind = where(weight_rot lt 0.01*max_weight, count)
;	  
	  if count gt 0 then weight_rot(ind)=0.
	
	ENDIF
	
		dimens = size(carte_rot)
		x1 = x0 + dimens(1) - 1
		y1 = y0 + dimens(2) - 1
		print, x0,y0,x1, y1
		
		
;	 	if scan_number eq 4832 then begin
;	   		weight_rot = weight_rot*2.7
;	 	endif
;		
;		if scan_number eq 48830 then begin				; BHR71-MM
;	   		weight_rot = weight_rot*12.6/9.5
;	 	endif
		
			
		map_radec(x0:x1, y0:y1) = map_radec(x0:x1, y0:y1) + carte_rot*weight_rot
		weight_radec(x0:x1, y0:y1) = weight_radec(x0:x1, y0:y1) + weight_rot
;        stop     
	ENDIF ELSE BEGIN
		print, "Skipping subscan ", subscan_list(i), " out of bounds or error_otf_scan : ", error_otf_scan(i)
	ENDELSE
ENDFOR  ; (pour tous les subscans)

ENDFOR ; (pour tous les scans)

proj=donnees_red.filename
proj=strsplit(proj,'-',/extract)
taille=size(proj)
proj=proj(taille(1)-4:taille(1)-1)
proj=strjoin(proj,'-')


id = where(weight_radec gt 0., count)
if count ne 0 then map_radec(id) = map_radec(id)/weight_radec(id)                ; division de la carte par la carte de poids

if count gt 0 then begin

 taille = size(map_radec)
 nx_pix = indgen(taille(1))#(transpose(intarr(taille(2)))+1)
 ny_pix = (intarr(taille(1))+1)#transpose(indgen(taille(2)))
; 
 min_x = min(nx_pix(id))
 max_x = max(nx_pix(id))
;
 min_y = min(ny_pix(id))
 max_y = max(ny_pix(id))
 
 map_radec_sub =  map_radec(min_x-2:max_x+2,min_y-2:max_y+2)
 weight_radec_sub = weight_radec(min_x-2:max_x+2,min_y-2:max_y+2)
 
 crpix1_sub = crpix1-min_x+2
 crpix2_sub = crpix2-min_y+2
  
endif

print, numb_scan
if numb_scan gt 1 then begin
obsname = 'APEX' 
for i = 0, n_elements(scan_list)-1 do obsname = obsname+'-'+strtrim(string(scan_list(i)),2)

;mapradecstr = {name:obsname, project_name:proj, date:date, image:map_radec, weight:weight_radec,crpix1:crpix1, crpix2:crpix2, $                 ; structure pour plusieurs scans combines
;                alpha_ref:double(alpha_ref), delta_ref:double(delta_ref), cdelt1:cdelt1, cdelt2:cdelt2, $
;	        scan_list:scan_list, source:carte_scan.source}

mapradecstr = {name:obsname, project_name:proj, date:date, image:map_radec_sub, weight:weight_radec_sub,crpix1:crpix1_sub, crpix2:crpix2_sub, $    ; structure pour plusieurs scans combines
                alpha_ref:double(alpha_ref), delta_ref:double(delta_ref), cdelt1:cdelt1, cdelt2:cdelt2, $
	        scan_list:scan_list, source:carte_scan.source, unit:donnees_red.unit}		

endif else begin

;mapradecstr = {name:obsname, project_name:proj, date:date, image:map_radec, weight:weight_radec,crpix1:crpix1, crpix2:crpix2, $                  ; structure pour un scan donne
;               alpha_ref:double(alpha_ref), delta_ref:double(delta_ref), cdelt1:cdelt1, cdelt2:cdelt2, $
;	       subscan_list:subscan_list, source:carte_scan.source, unit:donnees_red.unit, $
;	       scan_max_weight: scan_max_weight, scan_mean_weight: scan_mean_weight, $
;	       scan_min_longoff: offset_str.scan_min_longoff, scan_max_longoff: offset_str.scan_max_longoff, $
;	       scan_min_alpha: offset_str.scan_min_alpha, scan_max_alpha: offset_str.scan_max_alpha }

mapradecstr = {name:obsname, project_name:proj, date:date, image:map_radec_sub, weight:weight_radec_sub,crpix1:crpix1_sub, crpix2:crpix2_sub, $    ; structure pour un scan donne
               alpha_ref:double(alpha_ref), delta_ref:double(delta_ref), cdelt1:cdelt1, cdelt2:cdelt2, $
	       subscan_list:subscan_list, source:carte_scan.source, unit:donnees_red.unit, $
	       scan_max_weight: scan_max_weight, scan_mean_weight: scan_mean_weight, $
	       scan_min_longoff: offset_str.scan_min_longoff, scan_max_longoff: offset_str.scan_max_longoff, $
	       scan_min_alpha: offset_str.scan_min_alpha, scan_max_alpha: offset_str.scan_max_alpha }	       
	       
; NB : map_radec est en Az, El dans le cas d'une planete !!

endelse


return
end
