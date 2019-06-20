;+
; NAME:
;	RAW2CHOPBEAM_APEX
;
; PURPOSE:
;
;	Convert wobbler displacement along wobbler coordinate system longitude
;	axis to a signal with 1 value for ON position, -1 for OFF positions and
;	10 value for transitions.
;	
; CALLING SEQUENCE:
;	
;	Chopbeam = RAW2CHOPBEAM_APEX(Value, Chopamp)
;
; INPUTS:
;
;	Value:		Chopper displacement along wobbler coordinate.
;	Chopamp:	Chopper amplitude.	
;
; EXAMPLE:
;
;		Datastr.chopbeam = RAW2CHOPBEAM_APEX(Datastr.datapar.wobdisln*3600., Datastr.chopamp*3600.)
;
; MODIFICATION HISTORY:
; 	
;-

function raw2chopbeam_apex, value, chopamp


if n_params() eq 0 then begin
	print, 'Calling sequence : '
	print, '	RAW2CHOPBEAM_APEX, value, chopamp'

	return, -1
endif

; chopbeam = raw2chopbeam_apex(donnees.datapar.wobdisln*3600., donnees.chopamp*3600.)

idneg = where(value lt 0)
idpos = where(value ge 0)

chopbeam = value

bidon = chopamp
if chopamp eq 0. then begin
	exp_chopamp_pos = median(chopbeam(idpos))
	exp_chopamp_neg = median(chopbeam(idneg))
	bidon = mean(abs([exp_chopamp_pos, exp_chopamp_neg]))
	print, '% RAW2CHOPBEAM : CHOPAMP automatically set to : ' ,bidon
endif
chopamp = bidon

idpos = where(chopbeam lt chopamp*1.05 and chopbeam gt chopamp*0.95, countpos)
idneg = where(chopbeam gt -chopamp*1.05 and chopbeam lt -chopamp*0.95, countneg)

bidon = fix(value)*0+10

if (countpos gt 0 and countneg gt 0) then begin  
	bidon(idpos) = 1
	bidon(idneg) = -1
	chopbeam = bidon
endif else begin
	print, "Warning: Wobbler never in position"	
	chopbeam = bidon
endelse


return, chopbeam
end
