;+
; NAME:
;	MAP_MAKE_CH
;
; PURPOSE:
;
;	Build a channel map.
;
; CALLING SEQUENCE:
;	
;	MAP_MAKE_CH, Champ1=Champ1, Donnees2=Donnees2, Donnees_red=Donnees_red, Carte_scan=Carte_scan, Parang0=Parang0, Init_obs_str=Init_obs_str, Cube_ch_map_str
;
; INPUTS:
;
;	Champ1:		Contains the dimensions of the field of view in RA/DEC.
;	Donnees2: 	Input data structure.
;	Donnees_red:	Input data structure.
;	Carte_scan:	Subscan map.
;	Init_obs_str:	Contains calibration files for the observation.
;	Parang0:	Parallactic angle.		
;
; OPTIONAL INPUTS:
;
;	Project:	Project name directory
;		
; EXAMPLE:
;
;		MAP_MAKE_CH, Champ1=Champ1, Donnees2=Donnees2, Donnees_red=Donnees_red, Carte_scan=Carte_scan, Parang0=Parang0, Init_obs_str=Init_obs_str, Project=Project, Cube_ch_map_str
;
; MODIFICATION HISTORY:
;
;-

pro map_make_ch, champ1=champ1, donnees2=donnees2, donnees_red=donnees_red, carte_scan=carte_scan, parang0=parang0, $
init_obs_str=init_obs_str, project=project, cube_ch_map_str

; build a channel map 

taille = size(donnees_red.cube)
subscan_liste=init_obs_str.subscan_liste
subscan_name=init_obs_str.subscan_name
apexdata = init_obs_str.work_dir+'apexdata/'
obsname = 'APEX-' + strtrim(string(init_obs_str.scan_number),1)

if n_elements(champ1)-2 GE 1 then begin

champ0 = champ1(1:n_elements(champ1)-2)

champ1 = champ1(2:*)

endif else begin

champ0 = champ1(1:n_elements(champ1)-2)

champ1 = champ1(1:*)

endelse

long_min = min(champ1(*).long_min)
long_max = max(champ1(*).long_max)
lat_min = min(champ1(*).lat_min)
lat_max = max(champ1(*).lat_max)

cdelt2 = abs(min(champ1(*).lat_mean-champ0(*).lat_mean))

if cdelt2 lt 1. then begin

 command = donnees2.obslog.command                                ;  recuperation de donnees additionnelles dans les OBSLOG
 command = strmid(command,strpos(command,'ystep='))
 if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
	 	ystep = (strsplit(command,',',/extract))(0)
		ystep =  float(strmid(ystep,strpos(ystep,'=')+1))	; Cross-scan step (arcsec)
		print, 'ystep = ', ystep, ' (arcsec) '
	cdelt2 = ystep/3600.			; in deg	
 endif

if cdelt2 eq 0. then begin
   print, 'ERROR : cdelt2=0, number of subscans <= 1 ?'
   stop
endif

endif
if donnees2.wobused eq 1 then begin               ; wobbler utilise
	cdelt2 = abs(donnees2.otfvlam*donnees2.wobcycle)/3600.
endif

cdelt1 = cdelt2

print, "Adopted pixel size (arcsec) : ", cdelt2*3600.

; definition de la grille pour les cartes par canaux reconstruites dans les coordonnees2 de balayage.
;
x_size_grid = 2.5*abs((long_max - long_min)/cdelt1)
y_size_grid = 2.5*abs((lat_max - lat_min)/cdelt2)

long_ref = (long_max + long_min)/2
lat_ref = (lat_max + lat_min)/2
;long_ref = 0.
;lat_ref = 0.
crpix1 = x_size_grid/2.
crpix2 = y_size_grid/2.

nchan = taille(1)*taille(2)
print, "Total number of channels : ",nchan 
ch_map_cube = fltarr(x_size_grid, y_size_grid, nchan)
weight_ch_map = ch_map_cube

IF donnees2.movefram eq 1 and donnees2.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body  +  Scanning in Az
									;  --> Map construction in Az-El	
	print, 'Parallactic angle (deg) at central scan = ', parang0
ENDIF

;nxchan = indgen(16)#(transpose(intarr(16))+1)
;nychan = (intarr(16)+1)#transpose(indgen(16))

nxchan = indgen(taille(1))#(transpose(intarr(taille(2)))+1)
nychan = (intarr(taille(2))+1)#transpose(indgen(taille(1)))

FOR i=0, n_elements(champ1.subscan_name)-1 DO BEGIN

	filename= apexdata+'map_otf_xdr/'+project+'/otf_subscan_'+strtrim(string(init_obs_str.scan_number),1)+'_'+subscan_liste(i)+'.xdr'
	
	restore, filename  ;  (subscan_name, carte_scan, donnees2)                      ; restauration des donnees reduites


	taille = size(donnees_red.cube)

	nrecord = taille(3)-1								; To avoid potential position glitch at end of scan

;	pos_long = nint((donnees_red(*).datapar.longoff-long_ref)/cdelt1 + crpix1)
;	pos_lat = nint((donnees_red(*).datapar.latoff-lat_ref)/cdelt2 + crpix2)

	ind = where (donnees_red.datapar.longoff ne -999. and donnees_red.datapar.latoff ne -999., cnt)		; eliminating data with unknown telescope positions
	
	pos_long = nint((donnees_red.datapar(ind).longoff-long_ref)/cdelt1 + crpix1)
	pos_lat = nint((donnees_red.datapar(ind).latoff-lat_ref)/cdelt2 + crpix2)	
	
	nrecord = n_elements(pos_long)


	IF donnees2.movefram eq 1 and donnees2.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body
									;  --> Map construction in Az-El	
	        rot_angle = -carte_scan.elevatio        				; degrees	;  APEX
;		rot_angle = -(carte_scan.elevatio - 180.)		        	; degrees (clockwise)  KOSMA and APEX !!

		parang  = carte_scan.crota1 - carte_scan.elevatio			; Parallactic angle	APEX
		dparang = parang - parang0						; Delta parang / central scan

		rot_angle = rot_angle - dparang

	ENDIF ELSE BEGIN						;  Normal source or Planet in RA
									;  --> Map construction in RA-DEC			
	        rot_angle = -carte_scan.crota1	            ;  -(parang + El)  (degrees)   APEX
		parang  = carte_scan.crota1 - carte_scan.elevatio		; Parallactic angle    APEX
;		parang  = carte_scan.crota1 - carte_scan.elevatio + 180.	; Parallactic angle    (KOSMA and APEX !!!)
	ENDELSE	


	for ichan = 0, nchan-1 do begin

	  nx = nxchan(ichan)		; nx, ny = coordonnees2 du pixel dans la camera *avant re-echantillonnage"
	  ny = nychan(ichan)		;
	  channel = [nx,ny]	    

		for irec =0,nrecord-1 do begin			
			x_long = pos_long(irec)
			y_lat  = pos_lat(irec)
			weight_elem = 1.                     ; ch_map_cube contient une carte du champ par pixel
			ch_map_cube(x_long,y_lat,ichan) = ch_map_cube(x_long,y_lat,ichan) + donnees_red.cube(nx,ny,irec)*weight_elem
			weight_ch_map(x_long,y_lat,ichan) = weight_ch_map(x_long,y_lat,ichan)+weight_elem
		endfor

	endfor

ENDFOR

proj=donnees_red.filename
proj=strsplit(proj,'-',/extract)
taille=size(proj)
proj=proj(taille(1)-4:taille(1)-1)
proj=strjoin(proj,'-')

date=strsplit(donnees_red.date_obs,'T',/extract)
date=date(0)


id = where(weight_ch_map gt 0., count)
if count ne 0 then ch_map_cube(id) = ch_map_cube(id)/weight_ch_map(id)

cube_ch_map_str = {name:obsname, project_name:proj, date:date, ch_map:ch_map_cube, weight:weight_ch_map, crpix1:crpix1, crpix2:crpix2, $
                  scansys:donnees_red.scandir, long_ref:long_ref, lat_ref:lat_ref, cdelt1:cdelt1, cdelt2:cdelt2, $
	          subscan_liste:subscan_liste, source:donnees_red.source}



return

end
