;+
; NAME:
;	EKH_RESTORE
;
; PURPOSE:
;
;	Filter bolometer signal.
;
; CALLING SEQUENCE:
; 
;	Restruct = EKH_RESTORE(Diff)
;
; INPUTS:
;
;	Diff:	Input data structure
;
; EXAMPLE:
;
;		Restruct = EKH_RESTORE(Diff)
;
; MODIFICATION HISTORY:
;
;-


function ekh_restore, diff


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	Restruct = EKH_RESTORE(Diff)
	return, -1
endif	

taille = size(diff.cube)
nchan = taille(1)*taille(2)

;nrecord = n_elements(diff.phase)
nrecord = n_elements(diff.lst)
naz = nrecord

;ffun = filter_ekh_kosma(2*diff.chopamp, nrecord, diff.pas_az)
;rfun = rfilter_ekh_kosma(2*diff.chopamp, nrecord, diff.pas_az)

ffun = filter_ekh_kosma(2*diff.chopamp*3600., nrecord, diff.pas_az)
rfun = rfilter_ekh_kosma(2*diff.chopamp*3600., nrecord, diff.pas_az)


Signal_arr    = fltarr(nchan, nrecord)
Signal_arr(*) = diff.cube(*)
Resignal      = fltarr(nchan*nrecord)
rms_noise     = fltarr(nchan)


for ichan = 0, nchan-1 do begin

	Signal1 = Signal_arr(ichan,*)

	fsignal_1dim = conv_ekh1dim_kosma(Signal1,ffun,1.0, naz)
	rsignal_1dim = conv_ekh1dim_kosma(fsignal_1dim,rfun,-2.0, naz)

	Resignal(ichan*nrecord:(ichan+1)*nrecord-1) = rsignal_1dim

endfor

resignal = reform(resignal,nrecord,nchan)
cuberes = reform(transpose(resignal),taille(1),taille(2),nrecord)

;    print, "diff.paz = ", diff.pas_az, "  diff.phase(0) = ", diff.phase(0)
if sgn(diff.pas_az)*diff.phase(0) eq -1 then begin
    cuberes = -cuberes
    print, "phase = ", diff.phase(0), "  sgn(pas_az) = ", sgn(diff.pas_az)
    print, "EKH restore : Changing the sign of cuberes "
endif


resstruct = diff
resstruct.cube = cuberes

return, resstruct
end
