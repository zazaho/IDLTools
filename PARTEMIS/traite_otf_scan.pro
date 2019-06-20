;+
; NAME:
;	TRAITE_OTF_SCAN
;
; PURPOSE:
;
;	Eliminate noise from subscan data.
;	
; CALLING SEQUENCE:
;	
;	TRAITE_OTF_SCAN, Donnees, Donnees_red, Rms_noise, Error, Conv=Conv
;
;
; INPUTS:
;
;	Donnees:	Data before reduction.
;	Conv:		Conversion factor in Jy/beam/pW
;
; OPTIONAL INPUTS:
;
;	Masque_base_str:	Mask model.
;	Model:			Model.
;	Masque_model:		Mask model.
;	Rest:			Wobbler restauration mode(SAA, EKH or TP).	
;	
; KEYWORD PARAMETERS:
;
;	MED_BASE:	Remove median baseline.
;
;	MED_NOISE_REM:	Remove median correlated noise.
;		
;
; OUTPUTS:
;
;	Donnees_red:	Reduced data.
;	Rms_noise:	Calculated noise.
;	Error:		Error= 0, 1, -1
;
;
; EXAMPLE:
;
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, /MED_NOISE_REM, Donnees_red, Rms_noise, Error= Error_scan, Rest= RMODE, Conv= Conv	      
;	    
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, /MED_NOISE_REM, Donnees_red, Rms_noise, Error= Error_scan, Conv= Conv
;	      
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, Model= Model_str, Masque_model= Masque_modele_str, Donnees_red, Rms_noise, Error= Error_scan, Rest= Rmode, Conv= Conv    
;	         	         
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, Model= Model_str, Masque_model= Masque_modele_str, Donnees_red, Rms_noise, Error= Error_scan, Conv= Conv	      
;
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, /MED_NOISE_REM, Donnees_red, Rms_noise, Error= Error_scan, Rest= RMODE, Conv= Conv     
;   
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, /MED_NOISE_REM, Donnees_red, Rms_noise, Error= Error_scan, Conv= Conv   
;
;		TRAITE_OTF_SCAN, Donnees, Image_modele, Donnees_red, Rms_noise, Error= Error_scan, Conv= Conv  
;
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, Model= Model_str, Donnees_red, Rms_noise, Error= Error_scan, /MED_BASE, Rest= RMODE, Conv= Conv  
;		
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, Model= Model_str, Donnees_red, Rms_noise, Error= Error_scan, /MED_BASE, Conv= Conv  
;		  
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, Model= Model_str, Donnees_red, Rms_noise, Error= Error_scan, Rest= RMODE, Conv= Conv  		  
;				
;		TRAITE_OTF_SCAN, Donnees, Masque_base_str, Model= Model_str, Donnees_red, Rms_noise, Error= Rrror_scan, Conv= Conv  
;			
;
; MODIFICATION HISTORY:
; 	
;-

pro traite_otf_scan, donnees, masque_base_str, med_base=med_base, med_noise_rem=med_noise_rem, model=model_str, masque_model=masque_model_str,donnees_red,rms_noise,error=error,$
 rest=rest, conv=conv


;;;;;; masque_base_str est supposee etre une structure avec astrometrie et une image de 0 et de 1
;;;;;; ne sert pas dans locate_signal si non defini (!!)

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	TRAITE_OTF_SCAN, donnees, masque_base_str, med_base=med_base, med_noise_rem=med_noise_rem, model=model_str, masque_model=masque_model_str,donnees_red,rms_noise,error=error,  rest=rest, conv=conv'
	return
endif

if not keyword_set(rest) then rest ='TP' else rest=strupcase(rest)
print, "keyword_set(rest) ", keyword_set(rest), "  Mode :", rest 

date=strsplit(donnees.date_obs, /EXTRACT, 'T')
date=date[0]
date=strsplit(donnees.date_obs, /EXTRACT, '-')

cube = donnees.cube

; Determination de CHOPPOS, vecteur de positions du woobler
; l'operation consiste à determiner les valeurs suivantes :
;	position A du woobler      : +1
;	position B                 : -1
;	image rejetee = transition : 10

;freq_acq = donnees.freq_acq      ;  40.
;freq_chop = donnees.freq_chop
nchan = donnees.nchannels

;pos_chopper, cube, freq_chop, freq_acq, choppos, rejet=6

; Determination du nombre de positions en azimut, demi-periode du wobbler
;deltapos = choppos(0:n_elements(choppos)-2)-choppos(1:*)
;bidon = where(deltapos ne 0, nrecord)


;Construction d'un cube de masques pour l'elimination du bruit corrélé dans le scan traite
;------------------------------------------------------------------------------------------
; image_modele (ou masque_ciel) est une image "masque" dans laquelle 1 represente l'objet et 0 represente le ciel.
; cette image peut avoir ete obtenue apres un premier traitement sans masque de decorrelation
; sur l'ensemble des donnees.

;
;	FOURIER FILTERING
;

if keyword_set(model_str) then begin
	print, "Using model_str"
	donnees_mod = make_model_data(donnees, model_str)	

	donnees_sub = donnees
	donnees_sub.cube = donnees.cube - donnees_mod.cube
	
	donnees_copy = donnees
	donnees = donnees_sub
endif

if (strpos(donnees.obslog.strokemode,'ARC') eq 0 and rest eq 'FILT_SP') then begin		;;;; spiral scan and rmode = 'FILT_SP' or 'filt_sp' in traite_otf_map_main

    command = donnees.obslog.strokemode

    ang_vel = strsplit(command,',',/extract)
    if n_elements(ang_vel) eq 3 then begin
      ang_vel = float(ang_vel(2))						; Angular velocity (deg/sec) of spiral scan
    endif else begin
      ang_vel = 80.
      print, "Angular velocity is undefined. Adopting ang_vel = 80 deg/sec"
    endelse

    spiral_freq = abs(ang_vel)/360.

    print, "Filtering spiral scan - Angular velocity = ",ang_vel, " (deg/sec) - Spiral frequency (Hz) = ", spiral_freq
    filter_spiral, donnees, donnees_filtered, rms_noise, rms_noise_filtered, spiral_freq=spiral_freq
    donnees = donnees_filtered
    
endif else begin

 if (strpos(donnees.obslog.command,'otf') eq 0 and rest eq 'KILL_HIGH') then begin		;;;; otf scan and rmode = 'KILL_HIGH' or 'kill_high' in traite_otf_map_main
	;lambda = 450.e-6			        ;  Wavelength in m
	;tel_diam = 12.				        ;  m
	;l_to_d = (lambda/tel_diam)/(!pi/180./3600.)     ;  lambda/D in arcsec  ~ 7.75  ;   donnees.scanxvel = scanning velocity in arcsec/sec
;
	;freq_max = donnees.scanxvel/l_to_d		;  Maximum meaningful frequency (in Hz) in data stream

   ;   Elimination de la COMPOSANTE DE BRUIT a haute frequence (> freq_max)

    kill_high, donnees, donnees_high, rms_noise, rms_noise_high

    donnees = donnees_high

 endif else begin
 
   ;   Elimination de la COMPOSANTE DE BRUIT a 50Hz (<--> 10 Hz)

    kill_50Hz, donnees, donnees_50Hz, rms_noise, rms_noise_50Hz

    donnees = donnees_50Hz
    
 endelse
    
endelse

if (keyword_set(model_str) and rest eq 'FILT_SP') then begin

	filter_low, donnees, donnees_filt, rms_noise, rms_noise_filt

;	donnees.cube = donnees.cube + donnees_mod.cube
	donnees.cube = donnees_filt.cube + donnees_mod.cube

endif


;donnees_save = donnees				; saving copy of donnees after frequency filtering and before 1st baseline subtraction

;
; 1ere SOUSTRACTION DE LIGNE DE BASE ET ESTIMATION DU BRUIT DE CHAQUE CANAL (pour le scan considere)
;

if keyword_set(med_base) then begin

 IF (size(masque_base_str))(2) gt 1 THEN BEGIN				;   masque_base_str is defined
 
  print, 'masque_base_str is defined; using it'

  cube_masque = locate_signal(donnees, masque_base_str)
; En sortie de locate_signal, a chaque instant cube_masque vaut 1 sur la source et 0 ailleurs

  cube = donnees.cube
  med_baseline_scan, cube, cube_base, rms_noise, cube_masque = cube_masque
 
 ENDIF ELSE BEGIN							;   masque_base_str is undefined

  cube = donnees.cube
  med_baseline_scan, cube, cube_base, rms_noise
 
 ENDELSE
 
endif else begin

 cube_masque = locate_signal(donnees, masque_base_str)
; En sortie de locate_signal, a chaque instant cube_masque vaut 1 sur la source et 0 ailleurs

 cube = donnees.cube

 baseline_scan, cube, cube_masque, cube_base, rms_noise

endelse

cube = cube_base

;;;if (strpos(donnees.obslog.strokemode,'ARC') ne 0) then begin							;;;;;  EFFECTIVE BASELINE SUBTRACTION ONLY IF SPIRAL MODE WAS **NOT** USED
;														;;;;;  OR SPIRAL MODE AND MEDIAN BASELINE
;														;;;;;  OR NO DECORRELATION
;if (strpos(donnees.obslog.strokemode,'ARC') ne 0 or (rest ne 'FILT_SP' and keyword_set(med_base)) or (not keyword_set(model_str) and not keyword_set(med_noise_rem))) then begin 
if (strpos(donnees.obslog.strokemode,'ARC') ne 0 or keyword_set(med_base) or (not keyword_set(model_str) and not keyword_set(med_noise_rem))) then begin

   print, 'Baseline subtraction'
  
   donnees.cube = cube

endif

donnees.rmsmoy = rms_noise

donnees_save = donnees				; saving copy of donnees after 1st baseline subtraction

;
;

donnees_corrigees = donnees

; DECORRELATION - 

print, "keyword_set(model_str) = ", keyword_set(model_str)
if keyword_set(model_str) and not keyword_set(med_noise_rem) then begin
;	print, "Using model_str"

;	donnees_mod = make_model_data(donnees, model_str)	

	donnees_sub = donnees
	donnees_sub.cube = donnees.cube - donnees_mod.cube

	cube_masque_model = cube_masque

	print, "keyword_set(masque_model_str) = ", keyword_set(masque_model_str)
	if keyword_set(masque_model_str) then begin
		print, "Using masque_model_str"
		cube_masque_model = locate_signal(donnees_sub, masque_model_str)
	endif
	
	decorrel_scan, donnees_sub, cube_masque_model, donnees_corrigees, correlation_matrice
	
	donnees_corrigees.cube = donnees_corrigees.cube + donnees_mod.cube
	cube = donnees_corrigees.cube
endif

if keyword_set(med_noise_rem) then begin

print, "med_noise_rem = ", med_noise_rem

  if med_noise_rem eq 10 or med_noise_rem eq 20 then begin
  
     	if keyword_set(masque_model_str) then begin
;		print, "Using masque_model_str"		
		cube_masque_model = locate_signal(donnees, masque_model_str)
		cube_masque_noise = cube_masque_model
	        print, "Avoiding masque_model (cube_masque_model) in median correlated skynoise filtering"		
     	endif else begin
     		IF (size(masque_base_str))(2) lt 1 THEN BEGIN				;   masque_base_str is not defined
         		cube_masque = locate_signal(donnees, masque_base_str)
     		ENDIF
	        print, "Avoiding masque_base (cube_masque) in median correlated skynoise filtering"
		cube_masque_noise = cube_masque		
	endelse
		
      if med_noise_rem eq 10 then begin
	median_decorrel_scan, donnees, donnees_corrigees, correlation_matrice, cube_masque = cube_masque_noise	 ;  NB: median_decorrel_scan recomputes donnees.rmsmoy (rms_noise)
      endif else begin	
	median_decorrel_row, donnees, donnees_corrigees, correlation_matrice, cube_masque = cube_masque_noise	 ;  NB: median_decorrel_scan recomputes donnees.rmsmoy (rms_noise)
	print, "Filtering median skynoise on a row by row basis"
      endelse	
	cube = donnees_corrigees.cube
  endif else begin  
	print, "Median noise removal"
;	
      if med_noise_rem eq 2 then begin
	median_decorrel_row, donnees, donnees_corrigees, correlation_matrice	 				;  NB: median_decorrel_scan recomputes donnees.rmsmoy (rms_noise)
	print, "Filtering median skynoise on a row by row basis"
      endif else begin	
	median_decorrel_scan, donnees, donnees_corrigees, correlation_matrice	 				;  NB: median_decorrel_scan recomputes donnees.rmsmoy (rms_noise)
      endelse
	
	cube = donnees_corrigees.cube
  endelse
endif

        donnees_red  = donnees_corrigees

; ASSOCIATION des images AB AB AB AB ... OU MOYENNAGE DES DONNEES sur des periodes de temps raisonnables

error = 0
if donnees.wobused eq 1 then begin

  if rest eq 'TP' then begin
;     ind = where(donnees_red.chopbeam ne 10, count)
;     reduce_donneestruct, donnees_red, count, donnees_flag_wob
;     donnees_flag_wob.cube = donnees_red.cube(*,*,ind)
;     donnees_flag_wob.rmsmoy = donnees_red.rmsmoy
;     donnees_flag_wob.lst = donnees_red.lst(ind)
;     donnees_flag_wob.pas_az = donnees_red.pas_az
;     donnees_flag_wob.datapar.longoff = donnees_red.datapar(ind).longoff
;     donnees_flag_wob.datapar.latoff =  donnees_red.datapar(ind).latoff
;     donnees_red = donnees_flag_wob
     cube = donnees_red.cube     
  endif else begin
	asso_images_2007, donnees_red, diff_asso, error
	donnees_red = diff_asso          ;  i.e. diff if dual-beam (phase differences) ; asso if total power (averaged data)
        
	print, "Wobbler error = ", error
	if (error eq -1) then return
  endelse	
endif

; EVENTUELLEMENT CORRECTION DU FLAT FIELD

print, "keyword_set(model_str) = ", keyword_set(model_str)
;if keyword_set(model_str) then begin
;
; Nouvelle SOUSTRACTION DE LIGNE DE BASE ET ESTIMATION DU BRUIT DE CHAQUE CANAL APRES DECORRELATION EVENTUELLE
;
if keyword_set(med_base) then begin


;     IF (size(masque_base_str))(2) gt 1 THEN BEGIN				;   masque_base_str is defined
     IF (size(cube_masque))(2) gt 1 THEN BEGIN				;   cube_masque is defined
 
;        print, 'masque_base_str is defined; using it'
        print, 'cube_masque is defined; using it'

;;;    cube_masque = locate_signal(donnees, masque_base_str)  ;;; calcule à la première soustraction de ligne de base
;;;   A chaque instant cube_masque vaut 1 sur la source et 0 ailleurs

       cube = donnees_red.cube
       med_baseline_scan, cube, cube_base, rms_noise, cube_masque = cube_masque
 
     ENDIF ELSE BEGIN							       ;   masque_base_str is undefined

  	cube = donnees_red.cube
  	med_baseline_scan, cube, cube_base, rms_noise
 
     ENDELSE

	cube = cube_base

	donnees_red.rmsmoy = rms_noise

;	donnees_red.cube = cube
	
endif else begin

	cube = donnees_red.cube

	baseline_scan, cube, cube_masque, cube_corrige, rms_noise

	cube = cube_corrige

	donnees_red.rmsmoy = rms_noise

;	donnees_red.cube = cube
endelse

;;;if (strpos(donnees.obslog.strokemode,'ARC') ne 0) then begin			;;;;;  BASELINE SUBTRACTION ONLY IF SPIRAL MODE WAS **NOT** USED
;										;;;;;  OR SPIRAL MODE AND MEDIAN BASELINE
;										;;;;;  OR NO DECORRELATION
;if (strpos(donnees.obslog.strokemode,'ARC') ne 0 or (rest ne 'FILT_SP' and keyword_set(med_base)) or (not keyword_set(model_str) and not keyword_set(med_noise_rem))) then begin
if (strpos(donnees.obslog.strokemode,'ARC') ne 0 or keyword_set(med_base) or (not keyword_set(model_str) and not keyword_set(med_noise_rem))) then begin
        print, 'Baseline subtraction'
        donnees_red.cube = cube
endif

goodpix_ima = donnees_red.goodpix_ima
rms_noise = donnees_red.rmsmoy
index = where(goodpix_ima gt 0)
rms_noise_av = mean(rms_noise(index))
print, "Mean rms noise over valid pixels after decorrelation (if any) : ", rms_noise_av


; EKH RESTORATION DANS LE CAS OU LE WOBBLER EST UTILISE

IF donnees.wobused eq 1 and rest ne 'TP' THEN BEGIN
  
  ; Restauration par EKH
  ; Le traitement se fait en bouclant sur tous les pixels (sur les nchannels)

  ;nchan = donnees_red.nchannels
  ;nrecord = n_elements(donnees_red.cube(0,0,*))
  ;;nrecord = n_elements(diff.phase)


 if rest eq 'SAA' then begin
  resstruct = shift_and_add(donnees_red)
  
 endif else begin
  resstruct = ekh_restore(donnees_red)
 endelse
   cube = resstruct.cube

  ; Eventuellement Nouvelle Soustraction de Ligne de Base plus nouvelle estimation du bruit par canal
  ; (A eviter lorsqu'une source forte se trouve dans le champ)
 
;;;;;;;;  cube_masque = locate_signal(resstruct, masque_base_str)
  cube_masque = locate_dual_beam(resstruct, masque_base_str)
  
  ; En sortie de locate_dual_beam, a chaque instant cube_masque vaut 1 sur la source restauree en mode saa (parties positive et negatives) et 0 ailleurs
  ; (pour l'instant ne marche que pour une source circulaire).
  ; goodpix_ima est mis a 0 sur les pixels de la matrice qui ne voient la source qu'en positif ou negatif avant restauration
  
  
  donnees_red = resstruct

  baseline_scan, cube, cube_masque, cube_base, rms_noise

  cube = cube_base



ENDIF

donnees_red.cube = cube

;;;;
;;;;     POINTING CORRECTIONS :
;;;;

;
; indice de l'image du centre du scan
n_images = n_elements(donnees_red.datapar.azimuth)
id_ima_milieu = n_images/2


if  ((date[0] NE '') AND (fix(date[0]) LE 2007)) then begin

if donnees_red.scannum eq 4825 then begin
;   donnees_red.datapar.longoff = donnees_red.datapar.longoff -3.5/3600.        ; EQ scan : RA, Dec offsets (deg) from source as a function of time
;   donnees_red.datapar.latoff  = donnees_red.datapar.latoff  +5./3600.
   donnees_red.datapar.baslong = donnees_red.datapar.baslong -3.5/3600./cos(donnees_red.delta_mid*!pi/180.)			;  angle on sphere in deg
   donnees_red.datapar.baslat = donnees_red.datapar.baslat + 5./3600.
;
   donnees_red.alpha_mid = donnees_red.datapar(id_ima_milieu).baslong
   donnees_red.delta_mid = donnees_red.datapar(id_ima_milieu).baslat   
endif

if donnees_red.scannum eq 4828 then begin
   donnees_red.datapar.baslong = donnees_red.datapar.baslong +0./3600./cos(donnees_red.delta_mid*!pi/180.)			;  angle on sphere in deg
   donnees_red.datapar.baslat = donnees_red.datapar.baslat + 9./3600.
   print, "scan number: ", donnees_red.scannum, " pointing corrections (RA, DEC): ", 0, 9
;
   donnees_red.alpha_mid = donnees_red.datapar(id_ima_milieu).baslong
   donnees_red.delta_mid = donnees_red.datapar(id_ima_milieu).baslat 
endif

if donnees_red.scannum eq 47536 then begin
   donnees_red.datapar.baslong = donnees_red.datapar.baslong + 5./3600./cos(donnees_red.delta_mid*!pi/180.)			;  angle on sphere in deg
   donnees_red.datapar.baslat = donnees_red.datapar.baslat + 3./3600.   
;
   donnees_red.alpha_mid = donnees_red.datapar(id_ima_milieu).baslong
   donnees_red.delta_mid = donnees_red.datapar(id_ima_milieu).baslat
endif

if donnees_red.scannum eq 48822 then begin
   donnees_red.datapar.baslong = donnees_red.datapar.baslong + 4./3600./cos(donnees_red.delta_mid*!pi/180.)			;  angle on sphere in deg
   donnees_red.datapar.baslat = donnees_red.datapar.baslat + 4./3600.   
;
   donnees_red.alpha_mid = donnees_red.datapar(id_ima_milieu).baslong
   donnees_red.delta_mid = donnees_red.datapar(id_ima_milieu).baslat
endif

if donnees_red.scannum eq 48825 then begin
   donnees_red.datapar.baslong = donnees_red.datapar.baslong + 10./3600./cos(donnees_red.delta_mid*!pi/180.)			;  angle on sphere in deg
   donnees_red.datapar.baslat = donnees_red.datapar.baslat + 6./3600.   
;
   donnees_red.alpha_mid = donnees_red.datapar(id_ima_milieu).baslong
   donnees_red.delta_mid = donnees_red.datapar(id_ima_milieu).baslat
endif

if donnees_red.scannum eq 49281 then begin
   donnees_red.datapar.baslong = donnees_red.datapar.baslong + 5./3600./cos(donnees_red.delta_mid*!pi/180.)			;  angle on sphere in deg
   donnees_red.datapar.baslat = donnees_red.datapar.baslat + 6./3600.   
;
   donnees_red.alpha_mid = donnees_red.datapar(id_ima_milieu).baslong
   donnees_red.delta_mid = donnees_red.datapar(id_ima_milieu).baslat
endif

endif

;;;;
;;;;  END POINTING CORRECTIONS
;;;;

donnees = donnees_save				; restoring copy of donnees after 1st baseline subtraction

;donnees = donnees_save				; restoring copy of donnees after frequency filtering and before 1st baseline subtraction


return
end
