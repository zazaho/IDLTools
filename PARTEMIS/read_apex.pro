;+
; NAME:
;	READ_APEX
;
; PURPOSE:
;
;	Restore or read MBFits data from APEX for a given subscan.
;
; CALLING SEQUENCE:
;	 
;	Datastr = READ_APEX(Scan_number, Subscan, Filetype, Goodpix_ima=Goodpix_ima, Calibration_camera=Calibration_camera)
;
; INPUTS:
;
;	Scan_number:	Scan number.
;	Subscan:	Subscan number.
;	Filetype:	MAP, CHMAP or DECORREL.
;	Goodpix_ima:	Good pixels array, 1 for good pixels, 0 for bad ones.
;	Calibration_camera:	Input calibration file.
;		
; KEYWORD PARAMETERS:
;
;	NOPOWERMAP:	Do not calibrate from V to pW
;		
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, calibration table and project name
;		
; EXAMPLE:
;
;		Donnees = READ_APEX(47527, 10, 'MAP', Goodpix_ima=Goodpix_ima, Calibration_camera=Calibration_camera, /NOPOWERMAP)
;
; MODIFICATION HISTORY:
; 	
;-

function read_apex, scan_number, subscan, filetype, calibration_camera=calibration_camera, goodpix_ima=goodpix_ima, nopowermap=nopowermap

COMMON obs1_configb

apexdata = work_dir+'apexdata/'

dirdef = apexdata + 'basic_xdr/'

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	READ_APEX, scan_number, subscan, filetype, calibration_camera=calibration_camera, goodpix_ima=goodpix_ima, nopowermap=nopowermap'
	return, -1
endif



;filename = dirdef + project_name + '_' + strtrim(string(scan_number),2) + '_' + subscan + '.xdr'
filename = dirdef + '*' + '_' + strtrim(string(scan_number),2) + '_' + subscan + '.xdr'




print, dirdef

file = findfile(filename, count=nfile)

datastr = -1

if nfile eq 1 then begin
	Print, 'READ_APEX : restoring ' + file(0)
	restore, file(0)
	filetype='xdr'
endif else begin
	Print, 'READ_APEX : reading fits ' + strtrim(string(scan_number), 2) + '  ' + subscan 
    if keyword_set(nopowermap) then begin
         
	datastr = readfits_apex(scan_number, subscan, calibration_camera=calibration_camera, goodpix_ima=goodpix_ima, nopowermap=nopowermap)
	 
	
	
    endif else begin
        
	datastr = readfits_apex(scan_number, subscan, calibration_camera=calibration_camera, goodpix_ima=goodpix_ima) 
	
    endelse
    
    check_data=check_data_partemis(datastr)
    
    	
        if datatype(datastr) EQ 'STC'  then begin

        dirdat = apexdata + 'rawdata/'              
        u=find_all_dir(dirdat)
        u=u[where(strmatch(u,'*'+strtrim(string(scan_number),1)+'*',/FOLD_CASE) EQ 1)] 
        proj=u[0]
        proj=strsplit(proj,/EXTRACT,'-')
        taille2=size(proj)
        taille2=taille2(1)
        proj_name=proj(taille2-4:taille2-1)
        proj_name=strjoin(proj_name,'-')
	
	save, filename=dirdef + proj_name + '_' + strtrim(string(scan_number),2) + '_' + subscan + '.xdr', datastr, /xdr
	filetype='fits'
	
	endif else begin
	
	print, 'Le subscan'+' '+ subscan + ' ' +'contient des erreurs (cf Error_report_on_' + strtrim(string(scan_number),2) + '_data.txt) et a ete ignore'
	
	endelse
endelse

return, datastr
end
