;+
; NAME:
;	REDUCE_DONNEESTRUCT
;
; PURPOSE:
;
;	Reduce temporal dimensions for vectors and cube in a given structure.	
;
; CALLING SEQUENCE:
;
;	REDUCE_DONNEESTRUCT, Structin, Npoints, Structout
;
; INPUTS:
;
;	Structin:	Input structure.
;	Npoints:	Wished dimension.	
;
; OUTPUTS:
;
;	Structout:	Output structure.
;
; EXAMPLE:
;
;		REDUCE_DONNEESTRUCT, Structin, Npoints, Structout
;
; MODIFICATION HISTORY:
; 	
;-

pro reduce_donneestruct, structin, npoints, structout


; Cette procedure permet de reduire la dimension temporelle des champs du type vecteur et cube 


if n_params() eq 0 then begin
	print, 'Calling sequence:'
	print, '	REDUCE_DONNEESTRUCT, struct_name, npoints, structout'
	return
endif

help, structin, /str, out=structcomp

structcomp = strupcase(strtrim(structcomp, 2))

n_champ = n_elements(structcomp)-1


commande=''

for i=1, n_champ do begin
	fieldname = strmid(structcomp(i), 0, strpos(structcomp(i), ' '))
;	print, '('+fieldname+')'

	cmd = 'taille = size(structin.'+fieldname+')'
	status = execute(cmd)
	if taille(0) eq 0 then begin
		commande = commande + ', '+fieldname+':structin.'+fieldname
	endif else if taille(0) eq 1 then begin
		if taille(2) eq 8 then begin
			if fieldname eq 'OBSLOG' then begin
				commande = commande + ', '+fieldname+':structin.'+fieldname
			endif else begin
				commande = commande + ', '+fieldname+':structin.'+fieldname+'(0:npoints-1)'
			endelse
		endif else if taille(2) eq 7 then begin
			commande = commande + ', '+fieldname+':replicate("", npoints)'
		endif else begin
			commande = commande + ', '+fieldname+':structin.'+fieldname+'(0:npoints-1)*0'
		endelse
	endif else if taille(0) eq 2 then begin
		if fieldname eq 'GOODPIX_IMA' or fieldname eq 'RMSMOY' then begin
			commande = commande + ', '+fieldname+':structin.'+fieldname
		endif else begin
			commande = commande + ', '+fieldname+':structin.'+fieldname+'*0'
		endelse
	endif else if taille(0) eq 3 then begin
		if fieldname eq 'CUBE_ORIG' then begin
			commande = commande + ', '+fieldname+':structin.'+fieldname
		endif else begin
			commande = commande + ', '+fieldname+':structin.'+fieldname+'(*,*,0:npoints-1)*0'
		endelse
	endif
endfor

commande = strmid(commande, 1, strlen(commande))
commande = 'structout = {' + commande + '}'
;print, commande
status = execute(commande)

return
end	





