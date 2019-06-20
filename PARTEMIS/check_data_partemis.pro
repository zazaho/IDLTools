;+
; NAME:
;	CHECK_DATA_PARTEMIS
;
; PURPOSE:
;
;	Check if data structure read from MB-FITS file is non-empty and correct. It returns 1 for correct structure, -1 for non correct structure	
;
; CALLING SEQUENCE:
;	
;	Check_data = CHECK_DATA_PARTEMIS(Datastr)
;
; INPUTS:
;
;	Datastr:	Data structure.
;	
;		
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name.
;		
; EXAMPLE:
;		Check_data = CHECK_DATA_PARTEMIS(Datastr)
;
; MODIFICATION HISTORY:
; 	
;-


function check_data_partemis, datastr


COMMON obs1_configb


if datatype(datastr) NE 'STC' then begin

print, 'ERREUR : DATASTR N EST PAS UNE STRUCTURE'

return, -1

endif



dimens=size(datastr.cube)


if dimens[0] NE 3 then begin

print, 'ERREUR : DATASTR.CUBE NE COMPORTE PAS 3 MAIS' + ' ' + strcompress(string(dimens[0]))+ ' '+'DIMENSIONS'

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.CUBE NE COMPORTE PAS 3 MAIS' + ' ' + strcompress(string(dimens[0]))+ ' '+'DIMENSIONS'

close, lun

free_lun, lun

return, -1

endif


if datastr.elevatio gt 90. OR  datastr.elevatio lt 0. then begin

print, 'ERREUR : DATASTR.ELEVATIO N EST PAS COMPRIS ENTRE 0 ET 90 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.elevatio))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.ELEVATIO N EST PAS COMPRIS ENTRE 0 ET 90 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.elevatio))

close, lun

free_lun, lun

return, -1

endif


if datastr.sitelat gt 90. OR  datastr.sitelat lt -90. then begin

print, 'ERREUR : DATASTR.SITELAT N EST PAS COMPRIS ENTRE -90 ET 90 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.sitelat))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.SITELAT N EST PAS COMPRIS ENTRE -90 ET 90 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.sitelat))

close, lun

free_lun, lun

return, -1

endif



if datastr.sitelong gt 360. OR  datastr.sitelong lt -360. then begin

print, 'ERREUR : DATASTR.SITELONG N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.sitelong))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, ' : DATASTR.SITELONG N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.sitelong))

close, lun

free_lun, lun


return, -1

endif



if datastr.blongobj gt 360. OR  datastr.blongobj lt -360. then begin

print, 'ERREUR : DATASTR.BLONGOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.blongobj))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.BLONGOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.blongobj))

close, lun

free_lun, lun

return, -1

endif



if datastr.blatobj gt 360. OR  datastr.blatobj lt -360. then begin

print, 'ERREUR : DATASTR.BLATOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.blatobj))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, ' : DATASTR.BLATOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.blatobj)) 

close, lun

free_lun, lun

return, -1

endif



if datastr.longobj gt 360. OR  datastr.longobj lt -360. then begin

print, 'ERREUR : DATASTR.LONGOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.blongobj))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, ' : DATASTR.BLONGOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.blongobj))

close, lun

free_lun, lun

return, -1

endif



if datastr.latobj gt 360. OR  datastr.latobj lt -360. then begin

print, 'ERREUR : DATASTR.LATOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.latobj))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, ': DATASTR.LATOBJ N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.latobj))

close, lun

free_lun, lun

return, -1

endif


if datastr.wobused NE 1 AND  datastr.wobused NE 0 then begin

print, 'ERREUR : DATASTR.WOBUSED EST DIFFERENT DE 0 OU 1 ET VAUT' + ' ' + strcompress(string(datastr.wobused))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.WOBUSED EST DIFFERENT DE 0 OU 1 ET VAUT' + ' ' + strcompress(string(datastr.wobused))

close, lun

free_lun, lun

return, -1

endif



if datastr.mjd gt 100000. OR  datastr.mjd lt 50000. then begin

print, 'ERREUR : DATASTR.MJD EST ABERRANT ET VAUT' + ' ' + strcompress(string(datastr.mjd))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.MJD EST ABERRANT ET VAUT' + ' ' + strcompress(string(datastr.mjd))

close, lun

free_lun, lun

return, -1

endif

ind = where(datastr.lst ne -999., cnt)
if cnt gt 0 then begin
   lst = (datastr.lst)(ind)

	if max(lst) gt 25.*3600. OR  min(lst) lt 0. then begin

	print, 'ERREUR : DATASTR.LST EST ABERRANT ET EST COMPRIS ENTRE' + ' ' + strcompress(string(min(lst))) + ' ' + strcompress(string(max(lst)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, 'DATASTR.LST EST ABERRANT ET EST COMPRIS ENTRE' + ' ' + strcompress(string(min(lst))) + ' ' + strcompress(string(max(lst)))
  
	close, lun

	free_lun, lun

	return, -1

	endif
endif


if max(datastr.azim) gt 360. OR  min(datastr.azim) lt -360. then begin

print, 'ERREUR : DATASTR.AZIM EST ABERRANT ET EST COMPRIS ENTRE' + ' ' + strcompress(string(min(datastr.azim))) + ' ' + strcompress(string(max(datastr.azim)))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, ' : DATASTR.AZIM EST ABERRANT ET EST COMPRIS ENTRE' + ' ' + strcompress(string(min(datastr.azim))) + ' ' + strcompress(string(max(datastr.azim)))
 
close, lun

free_lun, lun

return, -1

endif



if max(datastr.elev) gt 90. OR  min(datastr.elev) lt 0. then begin

print, 'ERREUR : DATASTR.ELEV EST ABERRANT ET EST COMPRIS ENTRE' + ' ' + strcompress(string(min(datastr.elev))) + ' ' + strcompress(string(max(datastr.elev)))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '   : DATASTR.ELEV EST ABERRANT ET EST COMPRIS ENTRE' + ' ' + strcompress(string(min(datastr.elev))) + ' ' + strcompress(string(max(datastr.elev)))

close, lun

free_lun, lun

return, -1

endif


if datastr.crval1 gt 360. OR  datastr.crval1 lt -360. then begin

print, 'ERREUR : DATASTR.CRVAL1 N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.crval1))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.CRVAL1 N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.crval1))
 

close, lun

free_lun, lun

return, -1

endif




if datastr.crval2 gt 360. OR  datastr.crval2 lt -360. then begin

print, 'ERREUR : DATASTR.CRVAL2 N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.crval2))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, '  : DATASTR.CRVAL2 N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET VAUT' + ' ' + strcompress(string(datastr.crval2))

close, lun

free_lun, lun

return, -1

endif


ind = where(datastr.datapar.elevatio ne -999., cnt)
if cnt gt 0 then begin
   elevatio = (datastr.datapar.elevatio)(ind)

	if max(elevatio) gt 90. OR  min(elevatio) lt 0. then begin

	print, 'ERREUR : DATASTR.DATAPAR.ELEVATIO N EST PAS COMPRIS ENTRE 0 ET 90 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(elevatio))) + ' ' + strcompress(string(max(elevatio)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, '  : DATASTR.DATAPAR.ELEVATIO N EST PAS COMPRIS ENTRE 0 ET 90 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(elevatio))) + ' ' + strcompress(string(max(elevatio)))

	close, lun

	free_lun, lun

	return, -1

	endif
endif


if max(datastr.datapar.mjd) gt 100000. OR  min(datastr.datapar.mjd) lt 50000. then begin

print, 'ERREUR : DATASTR.DATAPAR.MJD EST ABERRANT ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(datastr.datapar.mjd))) + ' ' + strcompress(string(max(datastr.datapar.mjd)))

openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

printf, lun, datastr.subscan, ' : DATASTR.DATAPAR.MJD EST ABERRANT ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(datastr.datapar.mjd))) + ' ' + strcompress(string(max(datastr.datapar.mjd)))


close, lun

free_lun, lun

return, -1

endif


ind = where(datastr.datapar.azimuth ne -999., cnt)
if cnt gt 0 then begin
   azimuth = (datastr.datapar.azimuth)(ind)
   
	if max(azimuth) gt 360. OR  min(azimuth) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.AZIMUTH N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(azimuth))) + ' ' + strcompress(string(max(azimuth)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, ' : DATASTR.DATAPAR.AZIMUTH N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(azimuth))) + ' ' + strcompress(string(max(azimuth)))
 
	close, lun

	free_lun, lun

	return, -1

	endif
endif	


ind = where(datastr.datapar.elevatio ne -999., cnt)
if cnt gt 0 then begin
   elevatio = (datastr.datapar.elevatio)(ind)
   
	if max(elevatio) gt 360. OR  min(elevatio) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.ELEVATIO N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(elevatio))) + ' ' + strcompress(string(max(elevatio)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, ': DATASTR.DATAPAR.ELEVATIO N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(elevatio))) + ' ' + strcompress(string(max(elevatio)))
 

	close, lun

	free_lun, lun

	return, -1

	endif

endif

ind = where(datastr.datapar.parangle ne -999., cnt)
if cnt gt 0 then begin
   parangle = (datastr.datapar.parangle)(ind)

	if max(parangle) gt 360. OR  min(parangle) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.PARANGLE N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(datastr.datapar.parangle))) + ' ' + strcompress(string(max(datastr.datapar.parangle)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, '  : DATASTR.DATAPAR.PARANGLE N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(datastr.datapar.parangle))) + ' ' + strcompress(string(max(datastr.datapar.parangle)))
  
	close, lun

	free_lun, lun

	return, -1

	endif
endif

ind = where(datastr.datapar.cbaslong ne -999., cnt)
if cnt gt 0 then begin
   cbaslong = (datastr.datapar.cbaslong)(ind)

	if max(cbaslong) gt 360. OR  min(cbaslong) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.CBASLONG N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(cbaslong))) + ' ' + strcompress(string(max(cbaslong)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, '  : DATASTR.DATAPAR.CBASLONG N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(cbaslong))) + ' ' + strcompress(string(max(cbaslong)))

	close, lun

	free_lun, lun

	return, -1

	endif
endif	

ind = where(datastr.datapar.cbaslat ne -999., cnt)
if cnt gt 0 then begin
   cbaslat = (datastr.datapar.cbaslat)(ind)

	if max(cbaslat) gt 360. OR  min(cbaslat) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.CBASLAT N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(cbaslat))) + ' ' + strcompress(string(max(cbaslat)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, '  : DATASTR.DATAPAR.CBASLAT N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(cbaslat))) + ' ' + strcompress(string(max(cbaslat)))
 
	close, lun

	free_lun, lun

	return, -1

	endif
endif

ind = where(datastr.datapar.baslong ne -999., cnt)
if cnt gt 0 then begin
   baslong = (datastr.datapar.baslong)(ind)

	if max(baslong) gt 360. OR  min(baslong) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.BASLONG N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(baslong))) + ' ' + strcompress(string(max(baslong)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, ' : DATASTR.DATAPAR.BASLONG N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(baslong))) + ' ' + strcompress(string(max(baslong)))
 
	close, lun

	free_lun, lun

	return, -1

	endif
endif	

ind = where(datastr.datapar.baslat ne -999., cnt)
if cnt gt 0 then begin
   baslat = (datastr.datapar.baslat)(ind)

	if max(baslat) gt 360. OR  min(baslat) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.BASLAT N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(baslat))) + ' ' + strcompress(string(max(baslat)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, '  : DATASTR.DATAPAR.BASLAT N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(baslat))) + ' ' + strcompress(string(max(baslat)))
 
	close, lun

	free_lun, lun

	return, -1

	endif
endif

ind = where(datastr.datapar.ra ne -999., cnt)
if cnt gt 0 then begin
   ra = (datastr.datapar.ra)(ind)

	if max(ra) gt 360. OR  min(ra) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.RA N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(ra))) + ' ' + strcompress(string(max(ra)))

	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, ' : DATASTR.DATAPAR.RA N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(ra))) + ' ' + strcompress(string(max(ra)))
 
	close, lun

	free_lun, lun

	return, -1

	endif
endif	

ind = where(datastr.datapar.dec ne -999., cnt)
if cnt gt 0 then begin
   dec = (datastr.datapar.dec)(ind)

	if max(dec) gt 360. OR  min(dec) lt -360. then begin

	print, 'ERREUR : DATASTR.DATAPAR.DEC N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(dec))) + ' ' + strcompress(string(max(dec)))
	
	openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(datastr.scannum)) + '_data.txt' , /APPEND, /GET_LUN

	printf, lun, datastr.subscan, '  : DATASTR.DATAPAR.DEC N EST PAS COMPRIS ENTRE -360 ET 360 DEGRES ET S INSCRIT DANS L INTERVALLE' + ' ' + strcompress(string(min(dec))) + ' ' + strcompress(string(max(dec)))
 
	close, lun

	free_lun, lun

	return, -1

	endif
endif

return, 1

end
