;+
; NAME:
;	SHIFT_AND_ADD
;
; PURPOSE:
;
;	Filter bolometer signal.
;
; CALLING SEQUENCE:
; 
;	Restruct = SHIFT_AND_ADD(Donnees)
;
; INPUTS:
;
;	Donnees:	Input data Structure
;		
; EXAMPLE:
;
;		Restruct = SHIFT_AND_ADD(Donnees)
;
; MODIFICATION HISTORY:
;
;-

function shift_and_add, diff

taille = size(diff.cube)
nchan = taille(1)*taille(2)

nrecord = n_elements(diff.lst)
naz = nrecord

ffun = filter_ekh_kosma(2*diff.chopamp*3600., nrecord, diff.pas_az)
;;rfun = rfilter_ekh_kosma(2*diff.chopamp*3600., nrecord, diff.pas_az)

throw = nint(abs(2*diff.chopamp*3600./diff.pas_az))		;  Wobbler throw in units of pix_az

Signal_arr    = fltarr(nchan, nrecord)
Signal_arr(*) = diff.cube(*)
Resignal      = fltarr(nchan*nrecord)
rms_noise     = fltarr(nchan)


for ichan = 0, nchan-1 do begin

	Signal1 = Signal_arr(ichan,*)
	
	fsignal_1dim = conv_ekh1dim_kosma(Signal1,ffun,1.0, naz)
;	fsignal_1dim(*) = Signal1(*)
;;;	rsignal_1dim = conv_ekh1dim_kosma(fsignal_1dim,rfun,-2.0, naz)

        fsignal_1dim_reb = rebin(fsignal_1dim,naz*2)
	rsignal_1dim = (shift(fsignal_1dim_reb,-throw)-shift(fsignal_1dim_reb,throw))/2.

	Resignal(ichan*nrecord:(ichan+1)*nrecord-1) = rebin(rsignal_1dim,naz)

endfor

resignal = reform(resignal,nrecord,nchan)
cuberes = reform(transpose(resignal),taille(1),taille(2),nrecord)

;    print, "diff.paz = ", diff.pas_az, "  diff.phase(0) = ", diff.phase(0)
if sgn(diff.pas_az)*diff.phase(0) eq -1 then begin
    cuberes = -cuberes
    print, "phase = ", diff.phase(0), "  sgn(pas_az) = ", sgn(diff.pas_az)
    print, "SHIFT and ADD : Changing the sign of cuberes "
endif


resstruct = diff
resstruct.cube = cuberes

return, resstruct
end
