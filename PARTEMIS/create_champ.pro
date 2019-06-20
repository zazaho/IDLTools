;+
; NAME:
;	CREATE_CHAMP
;
; PURPOSE:
;
;	Calculate and fill a structure for a given subscan containing the
;	dimensions of the field of view in RA/DEC.
;	
; CALLING SEQUENCE:
;	
;	CREATE_CHAMP, Donnees, Carte_scan, Error_scan, Subscan_liste, Subscan_name, I, Type, Parang0, Champ
;
; INPUTS:
;
;	Donnees:	Input data structure.
;	Carte_scan:	Subscan map(structure).
;	Error_scan:	Error value.
;	Subscan_liste:	List of subscans.
;	Subscan_name:	Subscan name.
;	I:		Index.
;	Type:		MAP, CHMAP, or DECORREL
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;	Parang0:	Parallactic angle.
;	Champ:		Contains the dimensions of the field of view in 
;			RA/DEC.
;	
; EXAMPLE:
;
;		CREATE_CHAMP, DONNEES=DONNEES, CARTE_SCAN=CARTE_SCAN, ERROR_SCAN=ERROR_SCAN, SUBSCAN_LISTE=SUBSCAN_LISTE, SUBSCAN_NAME=SUBSCAN_NAME, I=I, TYPE='MAP', PARANG0, CHAMP
;
; MODIFICATION HISTORY:
;
;-

pro create_champ, donnees=donnees, carte_scan=carte_scan, error_scan=error_scan, subscan_liste=subscan_liste,$
subscan_name=subscan_name, i=i, type=type, parang0, champ


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	CREATE_CHAMP, DONNEES=DONNEES, CARTE_SCAN=CARTE_SCAN, ERROR_SCAN=ERROR_SCAN, SUBSCAN_LISTE=SUBSCAN_LISTE, SUBSCAN_NAME=SUBSCAN_NAME, I=I, TYPE=TYPE, PARANG0, CHAMP'
	return
endif

;calculate and fill a structure for a given subscan containing the dimensions of the field of view in RA/DEC 
;calling sequence



IF ((type eq 'map') OR (type eq 'decorrel')) THEN BEGIN                            ; si le type est MAP ou DECORREL

champ  = [{subscan_name : '', alpha_min : 0. , alpha_max : 0., $                   ; initialisation de la structure champ
                        delta_min : 0. , delta_max : 0., cdelt1: 0. , cdelt2:0. }]


taille = size(carte_scan.image)

	nx = indgen(taille(1))#(transpose(intarr(taille(2)))+1)
	ny = (intarr(taille(1))+1)#transpose(indgen(taille(2)))

	delta_x = (nx-carte_scan.crpix1)*carte_scan.cdelt1
	delta_y = (ny-carte_scan.crpix2)*carte_scan.cdelt2

	
	IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body  +  Scanning in Az
									;  --> Map construction in Az-El	

	elev = carte_scan.elevatio		; degrees
	rot_dxdy2dazdel, delta_x, delta_y, elev, delta_az, delta_el
;
;	delta_az, delta_el sont des tableaux donnant les offsets en Az, El (en degres) des pixels
;	de carte_scan (reperes en coordonnees x, y matrice) par rapport au centre de carte_scan 

; 	daz_scan et del_scan sont les offsets (en arcsec) du milieu du scan par rapport a la source pointee
;			     calcules par do_map_scan

	daz_scan = carte_scan.daz_scan			; en arcsec
	del_scan = carte_scan.del_scan			; arcsec

	      IF i eq n_elements(subscan_liste)/2 THEN BEGIN
		       parang0 = carte_scan.crota1 - carte_scan.elevatio 		; Parallactic angle at central scan      APEX ! 
;;;;		       parang0 = carte_scan.crota1 - carte_scan.elevatio + 180.		; Parallactic angle at central scan    KOSMA ????
	      ENDIF
;
;	delta_azim, delta_elev sont des tableaux donnant les offsets en Az, El (en degres) des pixels
;	de carte_scan (reperes en coordonnees x, y matrice) par rapport au centre de la source pointee 

	delta_azim  = delta_az  + daz_scan/3600.		; en degres
	delta_elev  = delta_el  + del_scan/3600.

;	Apres rotation, delta_azim, delta_elev sont des tableaux donnant les offsets en Az, El (en degres)
;	des points de carte_scan (reperes en coordonnees Az,EL) par rapport au centre de la source pointee

	rot_angle = -elev		        		; degrees	create_champ.pro;  APEX
	
	delta_azim = rot(delta_azim, rot_angle, 1, $
	                carte_scan.crpix1, carte_scan.crpix2, /interp)

	delta_elev = rot(delta_elev, rot_angle, 1, $
	                carte_scan.crpix1, carte_scan.crpix2, /interp)	

	     if (error_scan eq 0) then begin
		     champ.subscan_name = subscan_name(i)
		     champ.alpha_min = min(delta_azim)     &   champ.alpha_max = max(delta_azim)  
		     champ.delta_min = min(delta_elev)     &   champ.delta_max = max(delta_elev)  
		     champ.cdelt1 = abs(carte_scan.cdelt1) &   champ.cdelt2 = carte_scan.cdelt2
	     endif

	ENDIF ELSE BEGIN						;  Normal source or Scanning in RA
									;  --> Map construction in RA-DEC	


	  rot_angle = carte_scan.crota1*!pi/180.	 ; radians				;;;  12 May 2007
	  rot_dxdy2draddec, delta_x, delta_y, rot_angle, delta_alpha, delta_delta		

	  alpha = delta_alpha/cos(carte_scan.delta_ref*!pi/180.) + carte_scan.alpha_ref    	; angle on sphere 
	  delta = delta_delta + carte_scan.delta_ref
	
	  alpha = rot(alpha, -carte_scan.crota1, 1, $
	  carte_scan.crpix1, carte_scan.crpix2, /interp)

	  delta = rot(delta, -carte_scan.crota1, 1, $
	  carte_scan.crpix1, carte_scan.crpix2, /interp)

	       if (error_scan eq 0) then begin				
		   champ.subscan_name = subscan_name(i)
		   champ.alpha_min = min(alpha)     &   champ.alpha_max = max(alpha)  
		   champ.delta_min = min(delta)     &   champ.delta_max = max(delta)  
		   champ.cdelt1 = carte_scan.cdelt1 &   champ.cdelt2 = carte_scan.cdelt2
		
	       endif
	ENDELSE	                  ;(donnees.movefram EQ 1)

ENDIF                             ;(type MAP ou DECORREL)




IF type eq 'chmap' THEN BEGIN                                               ; type CHMAP

champ  = [{subscan_name : '', long_min : 0. , long_max : 0., $
                        lat_min : 0. , lat_max : 0. , lat_mean : 0. }]

id = where(donnees.datapar.longoff ne -999. and donnees.datapar.latoff ne -999.)

champ.subscan_name = subscan_name(i)
champ.long_min = min(donnees.datapar(id).longoff)     &   champ.long_max = max(donnees.datapar(id).longoff)  
champ.lat_min = min(donnees.datapar(id).latoff)     &   champ.lat_max = max(donnees.datapar(id).latoff)  
champ.lat_mean = mean(donnees.datapar(id).latoff)  
	

	
       IF donnees.movefram eq 1 and donnees.scandir ne 'RA' THEN BEGIN		;  Planet or Moving body  +  Scanning in Az
									;  --> Map construction in Az-El	
	     IF i eq n_elements(subscan_liste)/2 THEN BEGIN
		   ;parang0 = carte_scan.crota1 - carte_scan.elevatio + 180.		; Parallactic angle at central scan      KOSMA and APEX !! 
		   parang0 = carte_scan.crota1 - carte_scan.elevatio		        ; Parallactic angle at central scan      APEX !
	     ENDIF

       ENDIF					

		
ENDIF

return

end
