;+
; NAME:
;	DLST2DTLOC
;
; PURPOSE:
;
;	Convert local sideral time to local time.
;
; CALLING SEQUENCE:
;
;	Dtloc = DLST2DTLOC(Dlst)
;
; INPUTS:
;
;	Dlst:	Local sideral time
;
; EXAMPLE:
;
;		Dtloc = DLST2DTLOC(Dlst)
;
; MODIFICATION HISTORY:
;
;-

function dlst2dtloc, dlst


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	Dtloc = DLST2DTLOC(Dlst)
	return, -1
endif	

coeff = 23 + 56/60. + 4./3600
coeff = coeff/24.

dtloc = dlst * coeff

return, dtloc
end
