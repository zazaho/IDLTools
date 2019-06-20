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
;	READ_MBFITS
;
; PURPOSE:
;
;	Put MBFits format into IDL structure file.	
;
; CALLING SEQUENCE:
;
; 	READ_MBFITS, scan_number, subscan, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel	
;
; INPUTS:
;
;	Scan_number:	Scan number.
;	Subscan:	Subscan number.
;
; OUTPUTS:
;
;	arraydata
;	header_arraydata
;
;	datapar
;	header_datapar
;
;	monitor
;	header_monitor
;
;	datatel
;	header_tel	
;		
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name.
;		
; EXAMPLE:
;               @obs1_config
;		READ_MBFITS, 21450, 1, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel
;
; MODIFICATION HISTORY:
; 	
;-

pro read_mbfits, scan_number, subscan, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel, rows=rows


COMMON obs1_configb

;!path = '/Users/artemis/Desktop/apex_idl/apexpro:'+!path


apexdata = work_dir+'apexdata/'

;Calib_partemis = work_dir+'Calib_partemis/'

path = apexdata+'rawdata/'

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	READ_MBFITS, scan_number, subscan, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel'
	return
endif


;obsname = 'APEX-' + strtrim(string(scan_number),1) + '-*' + '-' + project_name
obsname = 'APEX-' + strtrim(string(scan_number),1) + '-*' + '-' + '*'

subscan_num = strtrim(strcompress(subscan),2)						; string


print, path + obsname + '/' + subscan_num + '/*.fits'


liste_arraydata = findfile(path + obsname + '/' + subscan_num + '/*ARRAYDATA-1.fits', count=nbre_1)

liste_datapar = findfile(path +obsname + '/' + subscan_num + '/*DATAPAR.fits', count=nbre_2)

liste_monitor = findfile(path +obsname + '/' + subscan_num + '/*MONITOR.fits', count=nbre_3)

liste_headertel = findfile(path + obsname + '/SCAN.fits', count=nombre)

if nbre_1 eq 0 or nbre_2 eq 0 or nbre_3 eq 0 then begin
       print, '% READ_MBFITS : Directory ' + path + obsname + ' not found'
       
       openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(scan_number)) + '_data.txt' , /APPEND, /GET_LUN  ; write into the error report

       printf, lun, strcompress(string(subscan)), '  : LES FICHIERS ARRAYDATA, DATAPAR OU MONITOR N ONT PAS ETE TROUVES  :' + path + obsname

       close, lun
       
       free_lun, lun
       
       return
endif


arraydata = mrdfits(liste_arraydata(0), 1, header_arraydata)

if datatype(arraydata) ne 'STC' then begin

       print, '% READFITS_APEX : PROBLEM DETECTED IN ARRAYDATA AT SUBSCAN' + ' ' + subscan
       print, 'SUBSCAN' + ' ' + subscan + ' '+'IGNORED'
       
       openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(scan_number)) + '_data.txt' , /APPEND, /GET_LUN  ; write into the error report

       printf, lun, strcompress(string(subscan)), '  : ECHEC DE LA LECTURE DE ARRAYDATA AVEC MRDFITS, ARRAYDATA N EST PAS UNE STRUCTURE'

       close, lun
       
       free_lun, lun
       
       return
endif

cube = reform(arraydata.data, 16, 16, n_elements(arraydata.data(*,0)))

datapar = mrdfits(liste_datapar(0), 1, header_datapar)


if datatype(datapar) ne 'STC' then begin
       print, '% READFITS_APEX : PROBLEM DETECTED IN DATAPAR AT SUBSCAN' + ' ' + subscan
       print, 'SUBSCAN' + ' ' + subscan + ' '+'IGNORED'
       
       openW, lun, work_dir + 'apexdata/' + strcompress('Error_report_on_' + string(scan_number)) + '_data.txt' , /APPEND, /GET_LUN ; write into the error report

       printf, lun, strcompress(string(subscan)), '  : ECHEC DE LA LECTURE DE DATAPAR AVEC MRDFITS, DATAPAR N EST PAS UNE STRUCTURE'

       close, lun
       
       free_lun, lun
       
       return
endif

poschop = intarr(n_elements(cube(0,0,*)))
chopbeam = fix(poschop*0)
lst = indgen(n_elements(cube(0,0,*)), /double)

if keyword_set(rows) then begin
   monitor = mrdfits(liste_monitor(0), 1, header_monitor,rows=rows)
   help, rows
endif else begin
   print, "keyword_set(rows) = ", keyword_set(rows)
   help, rows
   monitor = mrdfits(liste_monitor(0), 1, header_monitor)
endelse   

datatel = mrdfits(liste_headertel(0), 1, header_tel)


return

end
