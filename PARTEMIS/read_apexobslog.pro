;+
; NAME:
;	READ_APEXOBSLOG
;
; PURPOSE:
;
;	Read additional informations stored in a HTML file about an observation.
;
; CALLING SEQUENCE:
;	
;	Obslog = READ_APEXOBSLOG(Scannumber)
;
; INPUTS:
;
;	Scannumber:	Observation ID number.
;		
;
; KEYWORD PARAMETERS:
;
;	ALL:	Read all the obslogs.
;		
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name.
;			
;
; EXAMPLE:
;
;		Obslog = READ_APEXOBSLOG(47527,/ALL)
;
; MODIFICATION HISTORY:
; 	
;-

function read_apexobslog, scannumber, all=all

COMMON obs1_configb

if n_params() eq 0 and not keyword_set(all) then begin
	print, 'Calling sequence :'
	print, '	log_struct = READ_APEXOBSLOG(scannumber [, /all])
	return, -1
endif	

;apexdata = '/Users/artemis/Documents/apexdata/'
;apexdata = '/Users/andre/kosma/artemis/apexdata/'
;apexdata='/mnt/local/home/vminier/artemis/partemis/apexdata/'

apexdata=work_dir+'apexdata/'

restore, apexdata+'obslogs/apex_obslog.xdr'

if keyword_set(all) then begin 
	outlog = apex_struct
endif else begin
	help, scannumber
	id = where(apexlog_struct.number eq strtrim(string(scannumber, 2)), n)
	
	if n ne 0 then begin
		outlog = apexlog_struct(id(0))
	endif
endelse

return, outlog

end
