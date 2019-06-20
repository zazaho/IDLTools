function clean_chaine, chaine, car1, car2

if n_params() ne 3 then begin
	print, 'Calling sequence :'
	print, '	chaine_propre = CLEAN_CHAINE(chaine, car1, car2)'
	return, -1
endif

pos = strpos(chaine, car1)
if pos ne -1 then strput, chaine, car2, pos

return, chaine
end

;===============================================================================

function header2string, commande, param_liste, header



if n_params() ne 3 then begin
	print, 'Calling sequence :'
	print, '	data_struct = HEADER2STRING(commande, param_liste, header)'
	return, -1
endif

chaine = commande

for i=0,n_elements(param_liste)-1 do begin
	value = fxpar(header, param_liste(i))
	
;	print, param_liste(i), value, size(value, /type)
	if size(value, /type) eq 7 then begin
;		value = clean_chaine(value, '/', ' ')
;		value = clean_chaine(value, '=', ' ')
;		value = clean_chaine(value, "'", " ")
;		value = clean_chaine(value, "'", " ")
		value = strtrim(value, 2)
		chaine = chaine + ", " + $
		           clean_chaine(param_liste(i), '-', '_') + ": '" + $
			   value + "'"
	endif else begin
		chaine = chaine + ', ' + $
		           clean_chaine(param_liste(i), '-', '_') + ': '  + $
			   string(value)
	endelse
endfor

return, chaine
end

;===============================================================================

;+
; NAME:
;	READFITS_APEX
;
; PURPOSE:
;
;	Put MBFits format into IDL structure file.	
;
; CALLING SEQUENCE:
;	
;	Datastr = READFITS_APEX(Scan_number, Subscan, Calibration_camera=Calibration_camera)
;
; INPUTS:
;
;	Scan_number:	Scan number.
;	Subscan:	Subscan number.
;	Calibration_camera:	Calibration file.	
;	
; KEYWORD PARAMETERS:
;
;	NOPOWERMAP:	Do not Calibrate V to pW.
;		
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name.
;		
; EXAMPLE:
;		Datastr = READFITS_APEX(47527, 10, Calibration_camera=Calibration_camera, /NOPOWERMAP)
;
; MODIFICATION HISTORY:
; 	
;-

function readfits_apex, scan_number, subscan, calibration_camera=calibration_camera, goodpix_ima=goodpix_ima, nopowermap=nopowermap 


COMMON obs1_configb

;!path = '/Users/artemis/Desktop/apex_idl/apexpro:'+!path



apexdata = work_dir+'apexdata/'

Calib_partemis = work_dir+'Calib_partemis/'

path = apexdata+'rawdata/'

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	datastr = READFITS_APEX(scan_number, subscan, calibration_camera=calibration_camera, goodpix_ima=goodpix_ima, nopowermap=nopowermap)'
	return, -1
endif


;obsname = 'APEX-' + strtrim(string(scan_number),1) + '-*' + '-' + project_name
obsname = 'APEX-' + strtrim(string(scan_number),1) + '-*' + '-' + '*'

subscan_num = strtrim(strcompress(subscan),2)


print, path + obsname + '/' + subscan_num + '/*.fits'


liste_arraydata = findfile(path + obsname + '/' + subscan_num + '/*ARRAYDATA-1.fits', count=nbre_1)

liste_datapar = findfile(path +obsname + '/' + subscan_num + '/*DATAPAR.fits', count=nbre_2)

liste_headertel = findfile(path + obsname + '/SCAN.fits', count=nombre)

if nbre_1 eq 0 or nbre_2 eq 0 then begin
       print, '% READFITS_APEX : Directory ' + path + obsname + ' not found'
       return, -1
endif



obslog = read_apexobslog(scan_number)

u=readfits(calibration_camera)
taille=size(u)

arraydata = mrdfits(liste_arraydata(0), 1, header_arraydata)
cube = reform(arraydata.data, taille(1), taille(2), n_elements(arraydata.data(*,0)))

datapar = mrdfits(liste_datapar(0), 1, header_datapar)


;hkcube = readfits(liste(2), header_chop)
;poschop = fltarr(n_elements(hkcube(2,*)))
poschop = intarr(n_elements(cube(0,0,*)))
;poschop(*) = hkcube(2,*)*20/65535-10
chopbeam = fix(poschop*0)
;lst = indgen(n_elements(hkcube(2,*)), /double)
lst = indgen(n_elements(cube(0,0,*)), /double)



datatel = mrdfits(liste_headertel(0), 1, header_tel)

; Modification du header pour intégrer les paramètres provenant de APEX avec la
; bonne dénomination dans la structure

id = where(strmid(header_tel, 0, 4)  eq  'LST ')
bidon = header_tel(id(0))
strput, bidon, 'LST_INI', 0
header_tel(id(0)) = bidon

;fxaddparr, header_tel, 'CHAMP_UTIL', fxpar(header_tel, 'CHAMP_ORIG')

fxaddpar, header_tel, 'CHOPAMP', fxpar(header_tel, 'WOBTHROW')				;  Amplitude (1/2 throw) of the wobbler in deg
fxaddpar, header_tel, 'LAMBDA',  fxpar(header_tel, 'BLONGOBJ')
fxaddpar, header_tel, 'BETA',    fxpar(header_tel, 'BLATOBJ')
fxaddpar, header_tel, 'SOURCE',  fxpar(header_tel, 'OBJECT')
fxaddpar, header_tel, 'OTFVLAM', fxpar(header_tel, 'SCANXVEL')
fxaddpar, header_tel, 'OBSMODE', fxpar(header_tel, 'SCANTYPE')
fxaddpar, header_tel, 'NIMGCHOP',  fxpar(header_tel, 'WOBCYCLE') ;  Attention, le contenu est recalculé plus tard
fxaddpar, header_tel, 'SUBSCAN',  subscan ;  Attention, le contenu est recalculé plus tard

fxaddpar, header_tel, 'SPECTIME',  fxpar(header_tel, 'SCANTIME')


toto=readfits(calibration_camera, header_cam)


;if (scan_number lt 47513)  then begin		; kosma jan 2007 + apex < 16 Nov 2007
;; toto=readfits('/Users/artemis/Documents/kosma_data/AcquiUpTo060207/070129_180708/070129_180708.fits', header_cam)
;; toto=readfits('/Users/artemis/Desktop/Calib_partemis/070129_180708.fits', header_cam)

;toto=readfits(Calib_partemis+'070129_180708.fits', header_cam)
;; 
;endif else begin				; apex a partir du 16 Nov 2007  (changement de polarisation de la matrice)
;;
 ;  if (scan_number lt 47913)  then begin	; apex < 18 Nov 2007
;;       toto=readfits('/Users/artemis/Desktop/Calib_partemis/071116_132116.fits', header_cam)
;         toto=readfits(Calib_partemis+'071116_132116.fits', header_cam)
;   endif else begin				; apex a partir du 18 Nov 2007
;;       toto=readfits('/Users/artemis/Desktop/Calib_partemis/071117_222315.fits', header_cam)
 ;      toto=readfits(Calib_partemis+'071117_222315.fits', header_cam)
  ; endelse
;endelse

fxaddpar, header_tel, 'OFFSET',  fxpar(header_cam, 'OFFSET')
fxaddpar, header_tel, 'VL',  fxpar(header_cam, 'VL')
fxaddpar, header_tel, 'VH',  fxpar(header_cam, 'VH')

;fxaddpar, header_tel, 'OTFSBET',  fxpar(header_tel, '')


parametres_cam_1 = [ $
'ORIGIN	 ', $ 
'FREQ	 ', $ 
'NBMOY   ', $ 
'NIMGCHOP', $ 
'HACHEUR ', $ 
'NBIMG   ', $ 
'STARTIME', $ 
'DEBUT   ', $ 
'FIN	 ', $ 
'DATE	 ', $ 
'HEURE   '  ] 

parametres_cam_2 = [ $
'OFFSET  ', $ 
'DIFF	 ', $ 
'VH	 ', $ 
'VL	 ', $ 
'VCH	 ', $ 
'VHBLIND ', $ 
'VINJ	 ', $ 
'VDECXL  ', $ 
'VDECXH  ', $ 
'VDL	 ', $ 
'VGL	 ', $ 
'VSS	 ', $ 
'VDD	 ', $ 
'VGG	 ', $ 
'VSMSL   ', $ 
'VSMSH   ', $ 
'CKRLL   ', $ 
'CKRLH   ', $ 
'VRL	 ', $ 
'VTRIX   ', $ 
'VTRIL   ', $ 
'VDLBU   ', $ 
'VSSBU   ', $ 
'VGLBU   ', $ 
'GNDBU   ', $ 
'VIN1	 ', $ 
'VIN2	 ', $ 
'CTEST   ', $ 
'CPOBI4HE', $ 
'CPOBI3HE', $ 
'CITBI4HE', $ 
'CITBI3HE', $ 
'CPO1MONO', $ 
'CPO2MONO', $ 
'CITMONO ', $ 
'CCN	 ', $ 
'POBI3HE ', $ 
'ITBI3HE ', $ 
'EVBI4HE ', $ 
'COBI4HE ', $ 
'POBI4HE ', $ 
'ITBI4HE ', $ 
'BU	 ', $ 
'EVMONO  ', $ 
'COMONO  ', $ 
'PO1MONO ', $ 
'PO2MONO ', $ 
'ITMONO  ', $ 
'DETECTOR', $ 
'EVBI3HE ', $ 
'SPARE1  ', $ 
'SPARE2  '  ]



parametres_tel_1 = [ $   
'EXTNAME ', $;  'SCAN-MBFITS'	  / name of this binary table extension
'TELESCOP', $;  'APEX-12m' 	  / Telescope Name
'SITELONG', $; 	-67.7592222222222 / [deg] Observatory longitude
'SITELAT ', $; 		-23.00575 / [deg] Observatory latitude
'SITEELEV', $; 		    5105. / [m] Observatory elevation
'DIAMETER', $; 		      12. / [m] Dish diameter
'PROJID  ', $;  'T-78.F-0006-2006'   / Project ID
'OBSID   ', $;  'RS/NN   ' 	  / Observer and operator initials
'SCANNUM ', $; 		     4025 / Scan number
'SUBSCAN ', $;                  9 / Subscan number (not part of the original header)
'TIMESYS ', $;  'TAI     ' 	  / Time system for MJD and DATE-OBS
'DATE-OBS', $;  '2007-03-20T15:01:03' / Scan start in TIMESYS system (Y2K format with
'MJD	 ', $; 	 54179.6257291667 / Scan date/time (Modified Julian Date) in
'LST_INI ', $; 	  80432.059377301 / Local apparent sidereal time (scan start)
'NOBS	 ', $; 			1 / Number of subscans in this scan
'NSUBS   ', $; 			1 / Number of subscans in this scan
'UTC2UT1 ', $; 		 -0.05192 / [s] UT1-UTC time translation
'TAI2UTC ', $; 		      33. / [s] UTC-TAI time translation
'ETUTC   ', $; 		    -999. / [s] Ephemeris Time - UTC time translation
'GPSTAI  ', $; 		      19. / [s] GPS time - TAI translation
'CTYPE1  ', $;  'RA---SFL' 	  / Basis system (longitude)
'CTYPE2  ', $;  'DEC--SFL' 	  / Basis system (latitude)
'WCSNAME ', $;  'absolute equatorial' / Human-readable description of frame
'RADESYS ', $;  'FK5     ' 	  / Additional system definition for ecliptic/equat
'EQUINOX ', $; 		    2000. / [Julian years] Equinox
'CRVAL1  ', $; 	 258.380402937614 / [deg] Native frame zero in basis system (long.)
'CRVAL2  ', $; 	 -22.283130251835 / [deg] Native frame zero in basis system (lat.)
'LONPOLE ', $; 		     180. / [deg] Native longitude of celestial pole: range
'LATPOLE ', $; 	  67.716869748165 / [deg] Basis latitude of native pole
'OBJECT  ', $;  'Jupiter ' 	  / Source name
'BLONGOBJ', $; 	  258.38040090495 / [deg] Source longitude in basis frame
'BLATOBJ ', $; 	-22.2831301671406 / [deg] Source latitude in basis frame
'LONGOBJ ', $;  -2.03266398557389E-06 / Source longitude in user native frame
'LATOBJ  ', $;  8.46943990495674E-08 / Source latitude in user native frame
'CALCODE ', $;  '        ' 	  / Calibrator Code
'MOVEFRAM', $; 			F / True if tracking a moving frame
'PERIDATE', $; 		    -999. / [Julian days] TP, Full Julian date of perihelio
'PERIDIST', $; 		    -999. / [AU] QR, perihelion distance
'LONGASC ', $; 		    -999. / [deg] OM, Longitude of ascending node
'OMEGA   ', $; 		    -999. / [deg] W, Angle from asc. node to perihelion
'INCLINAT', $; 		    -999. / [deg] IN, Inclination
'ECCENTR ', $; 		    -999. / EC, Eccentricity
'ORBEPOCH', $; 		    -999. / [Julian days] EPOCH, Epoch of orbital elements
'ORBEQNOX', $; 		    2000. / [years] Elements equinox: J2000.0 or B1950.0
'DISTANCE', $; 		    -999. / [AU] Geocentric Distance
'SCANTYPE', $;  'ONOFF   ' 	  / Scan astronomical type
'SCANMODE', $;  'RASTER  ' 	  / Mapping mode
'SCANGEOM', $;  'SINGLE  ' 	  / Scan geometry
'SCANDIR ', $;  'Unkn    ' 	  / (optional) scan direction
'SCANLINE', $; 			0 / (optional) number of lines in a scan
'SCANRPTS', $; 			0 / (optional) number of repeats of each scan line
'SCANLEN ', $; 		      -0. / (optional, OTF/RASTER) line length in deg
'SCANXVEL', $; 		      -0. / (optional, OTF) tracking rate along line.
'SCANTIME', $; 		      -0. / (optional, OTF) time for one line
'SCANXSPC', $; 		      -0. / (optional, RASTER) step along line between samp
'SCANYSPC', $; 		      -0. / (optional, OTF/RASTER) step between scan/raster
'SCANSKEW', $; 		      -0. / (optional, OTF/RASTER) offset in scan direction
'SCANPAR1', $; 		      -0. / (optional) spare scan parameter
'SCANPAR2', $; 		      -0. / (optional) another spare scan parameter
'CROCYCLE', $;  '        ' 	  / CAL/REF/ON loop string
'ZIGZAG  ', $; 			F / (optional, OTF/RASTER) Scan in zigzag?
'TRANDIST', $; 		    -999. / [m] (optional, HOLO) Holography transmitter dis
'TRANFREQ', $; 		    -999. / [Hz] (optional, HOLO) Holography transmitter fr
'TRANFOCU', $; 		    -999. / [deg] (optional, HOLO) Holography transmitter o
'WOBUSED ', $; 			F / Wobbler used?
'WOBTHROW', $; 		       0. / [deg] Wobbler throw
'WOBDIR  ', $;  'NONE    ' 	  / Wobbler throw direction
'WOBCYCLE', $; 		       0. / [s] Wobbler period
'WOBMODE ', $;  'NONE    ' 	  / Wobbler mode (SQUARE/TRIANGULAR)
'WOBPATT ', $;  'NONE    ' 	  / Wobbler pattern (e.g. NEG,POS,SYM)
'PHASE1  ', $;  'NONE    ' 	  / Phase 1 description
'NFEBE   ', $; 			1 / Number of FEBEs
'PDELTACA', $; 		       0. / [deg] Accumulated user ptg correction CA
'PDELTAIE', $; 		       0. / [deg] Accumulated user ptg correction IE
'FDELTACA', $; 		       0. / [deg] Accumulated ptg corr to CA due to focus
'FDELTAIE', $; 		       0. / [deg] Accumulated ptg corr to IE due to focus
'IA	 ', $; 		 0.418497 / [deg] Pointing Coefficient (-P1)
'IE	 ', $; 		0.4790366 / [deg] Pointing Coefficient (P7)
'HASA	 ', $; 	     -0.001058417 / [deg] Pointing Coefficient
'HACA	 ', $; 	    -0.0002590278 / [deg] Pointing Coefficient
'HESE	 ', $; 		       0. / [deg] Pointing Coefficient (was ZFLX)
'HECE	 ', $; 	     -0.006228222 / [deg] Pointing Coefficient (P8; was ECEC)
'HESA	 ', $; 	     2.183333E-05 / [deg] Pointing Coefficient
'HASA2   ', $; 	    -0.0008196944 / [deg] Pointing Coefficient
'HACA2   ', $; 	     -0.001612556 / [deg] Pointing Coefficient
'HESA2   ', $; 	    -5.102778E-05 / [deg] Pointing Coefficient
'HECA2   ', $; 	      -0.00082525 / [deg] Pointing Coefficient
'HACA3   ', $; 	     0.0005024445 / [deg] Pointing Coefficient
'HECA3   ', $; 	     0.0002202222 / [deg] Pointing Coefficient
'HESA3   ', $; 	     1.136111E-05 / [deg] Pointing Coefficient
'NPAE	 ', $; 		   -0.005 / [deg] Pointing Coefficient (-P3)
'CA	 ', $; 	      -0.03923142 / [deg] Pointing Coefficient (-P2)
'AN	 ', $; 	      0.006855028 / [deg] Pointing Coefficient (-P5)
'AW	 ', $; 	     -0.008293056 / [deg] Pointing Coefficient (-P4)
'TTYPE1  ', $;  'FEBE    ' 	  / Frontend-backend combination identification
'TFORM1  ', $;  '17A     ' 	  / format of field
'CHECKSUM', $;  'i6aAj6T4i6Y9i6Y9'   / HDU checksum updated 2007-03-20T15:01:11
'DATASUM ', $;  '1595939336'	  / data unit checksum updated 2007-03-20T15:01:11
; Ici,on trouve les parametres pour le traitement similaire Kosma
'CHOPAMP ', $;                     / equiv 'WOBTHROW'
'LAMBDA  ', $;                     / equiv 'BLONGOBJ'
'BETA    ', $;  		   / equiv 'BLATOBJ'
'SOURCE  ', $;                     / equiv 'OBJECT'
'OTFVLAM ', $;                     / equiv 'SCANXVEL'
'SPECTIME', $;                     / equiv 'SCANTIME'
'OTFSBET' , $; 
'NIMGCHOP', $;                     / equiv 'WOBCYCLE' a quelque chose pres
'OBSMODE',  $;                     / equiv 'SCANTYPE'
'OFFSET',   $;			L'offset detecteur provenant des HK
'VL',       $;         		V low  detecteur
'VH'        $;			V high detecteur
]

parametres_tel_2 = [ $
'NUMSPEC ', $ 
'NAMESPEC'  ]


taille = size(cube)

; Construction de la structure house keeping
;commande = header2string('', parametres_cam_2, header_cam)
;commande = strmid(commande, 1, strlen(commande))
;commande = strcompress('hk = {' + commande + ', hkcube:hkcube}')
;status = execute(commande)

; Construction de la structure contenant des parametres telescopes secondaires
commande = header2string('', parametres_tel_2, header_tel)
commande = strmid(commande, 1, strlen(commande))
commande = strcompress('tel = {' + commande + '}')
status = execute(commande)

;subscan_num = strtrim(strcompress(subscan),2)

apexdata = work_dir+'apexdata/'	
dirdat = apexdata + 'rawdata/'              
u=find_all_dir(dirdat)
u=u[where(strmatch(u,'*'+strtrim(string(scan_number),1)+'*',/FOLD_CASE) EQ 1)] 
proj=u[0]
proj=strsplit(proj,/EXTRACT,'-')
taille2=size(proj)
taille2=taille2(1)
proj_name=proj(taille2-4:taille2-1)
proj_name=strjoin(proj_name,'-')

filename = 'APEX-' + strtrim(string(scan_number),1) + '_' + subscan_num + '-' + proj_name

; construction de la structure de donnees
commande = 'filename : filename, cube : cube' 
commande = commande + ", azim : dblarr(" + strtrim(string(taille(3)), 2) + ")"
commande = commande + ", elev : dblarr(" + strtrim(string(taille(3)), 2) + ")"
commande = commande + ", rota : dblarr(" + strtrim(string(taille(3)), 2) + ")"
commande = commande + ', poschop : poschop'
commande = commande + ', chopbeam : chopbeam'
commande = commande + ', lst : lst'
commande = commande + ', lst_mid : double(0)'
commande = commande + ', alpha_mid : double(0)'
commande = commande + ', delta_mid : double(0)'
commande = commande + ', cdelt1 : 0.'
commande = commande + ', cdelt2 : 0.'
commande = commande + ', crota1 : 0.'
commande = commande + ', pas_az : 0.'
commande = commande + ', unit   : "V"'
commande = commande + ', datapar: datapar'
commande = commande + ', elevatio : double(0)'
commande = commande + ', scan : double(0)'
commande = commande + ', subscan_number : double(0)'
commande = commande + ', obslog : obslog'

;commande = header2string(commande, parametres_cam_1, header_cam)

commande = header2string(commande, parametres_tel_1, header_tel)


taille =size(cube)
nchannels=taille(1)*taille(2)

commande = commande + ', nchannels : ' + strtrim(nchannels)
commande = commande + ', freq : 40.'

commande = 'datastr = { ' + commande + $
;           ', hk:hk, $
;             tel:tel, $
            ',rmsmoy:fltarr(taille(1),taille(1)), goodpix_ima:bytarr(taille(1),taille(2)), '+ $
	   'cube_orig:cube}'
commande = strcompress(commande)

status = execute(commande(0))


datastr.elevatio = mean(datastr.datapar.elevatio)

; calcul du lst pour chacune des images
;lst_ini = float(strmid(datastr.OBS_LST, 0, 2))*3600. + $
;          float(strmid(datastr.OBS_LST, 3, 2))*60. + $
;          float(strmid(datastr.OBS_LST, 6, 2))

;coeff = 23 + 56/60. + 4./3600
;coeff = coeff/24.

;lst = lst * 25e-3/coeff + lst_ini
;lst = lst * dtloc2dlst(1./datastr.freq) + lst_ini
;lst = dtloc2dlst((arraydata.mjd - arraydata(0).mjd)*24*3600.) + datastr.lst_ini


datastr.lst = datastr.datapar.lst
datastr.freq =  40.		;  Frequence d'acquisition en Hz

taille = size(datastr.cube)
datastr.nchannels = taille(1)*taille(2)


datastr.cdelt1 = 5.85  ;   5.5     ;  4.5     ;  3.6     ;  3.65    ;  4.     ; taille du pixel P-Artemis 450 microns a APEX en arcsec
datastr.cdelt2 = 5.85  ;   5.5     ;  4.5     ;  3.6	 ;  3.65    ;  4.


;bidon = extract_ima_moy(datastr.cube, datastr.rmsmoy)

bidon = datastr.chopamp*3600.							; Expected amplitude (1/2 throw) of the wobbler in arcsec

datastr.nimgchop = (datastr.wobcycle/2.) * datastr.freq				; Nb d'images par 1/2 cycle du wobbler

if datastr.wobused eq 1 and datastr.nimgchop gt 0 then begin
		datastr.chopbeam = raw2chopbeam_apex(datastr.datapar.wobdisln*3600., bidon)
;		print, 'READFITS_APEX : WARNING - Wobbler position to be determined from MBFITS words'
endif
; plot, datastr.datapar.lst, datastr.datapar.longoff*3600.		; arcsec
; plot, datastr.datapar.lst, datastr.datapar.wobdisln*3600.		; arcsec

;datastr.chopamp = bidonsetenv FVTMP /temporary/directory/path

if datastr.lambda eq 0 and datastr.beta eq 0 then begin
	altaz2hadec, datastr.elevatio, datastr.azimuth, datastr.sitelat, ha, dec
	alpha = lst_ini*15./3600. - ha
	if alpha lt 0 then alpha = 360 + alpha
	datastr.lambda = alpha
	datastr.beta = dec
endif

;restore, '/Users/artemis/Desktop/Calib_partemis/goodpix_ima.xdr'
;restore, Calib_partemis + goodpix_ima

datastr.goodpix_ima = goodpix_ima			;  Utilise dans do_map_scan pour la reconstruction du subscan OTF 


if datastr.scanmode eq 'OTF' then begin
;	restore, '/Users/artemis/Documents/apexdata/obslogs/otf_obslog.xdr'
;	id = where(file_number eq datastr.SCANNUM, nid)
;	if nid eq 0 then begin
;		print, 'READFITS_APEX : Scan is not in xdr APEX log file'
;		return, -1	
;	endif
	datastr.scanxvel = float(datastr.obslog.map.xstep)/float(datastr.obslog.map.time) ;; in arcsec/sec
	datastr.otfvlam = double(datastr.scanxvel)
	datastr.scantime = float(datastr.obslog.map.xlen)/datastr.scanxvel  ; in sec
	datastr.scanlen  = float(datastr.obslog.map.xlen)/3600.              ; line length in degree
endif


; factor_otf = facteur d'echelle a appliquer sur la vitesse en Az pour l'obtenir en
;              arcsec/sec... depend de Kosma 
;factor_otf = 1./datastr.spectime
;datastr.otfvlam = datastr.otfvlam*factor_otf              ; vitesse en arcsec/sec
datastr.pas_az = double(datastr.otfvlam/datastr.freq)  		   ; pas le long de la direction de scanning en arcsec

dimens=size(datastr.cube)

if dimens[0] eq 3 then begin
							   	;  !! Pas necessairement Azimuth !!
;datastr.pas_az = -sgn(datastr.datapar(0).longoff)*datastr.pas_az     		;  positive or negative (zigzag !!)
datastr.pas_az = sgn(median(deriv(datastr.datapar.longoff)))*datastr.pas_az     ;  OK seulement si 0 le angle(scanning) le 90

if not keyword_set(nopowermap) then begin
	power_map, datastr.offset, datastr.vh-datastr.vl, $
	   datastr.vl, datastr.cube, cube_out
	datastr.cube = cube_out
	datastr.unit = 'pW'
endif

datastr.movefram = 0
source = datastr.object
if (source eq 'Mercury' or source eq 'Venus' or source eq 'Mars' or source eq 'Jupiter' or source eq 'Saturn' or source eq 'Uranus' or source eq 'Neptune') then begin
	datastr.movefram = 1
	print, "Observing Planet ", source
endif

endif

;stop

return, datastr

end
