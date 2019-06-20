;+
; NAME:
;	STRUCT_EXPAND
;
; PURPOSE:
;
;	Add a new field in a given structure at a given position.	
;
; CALLING SEQUENCE:
;	
;	STRUCT_EXPAND, Struct_name, Param_name, Param_val, Structout
;
; INPUTS:
;
;	Struct_name:	Input structure.
;	Param_name:	Parameter name.
;	Param_val:	Parameter value.	
;
; OPTIONAL INPUTS:
;
;	Pos:	Position.
;		
; OUTPUTS:
;
;	Structout:	Output structure.
;	
; EXAMPLE:
;	
;		STRUCT_EXPAND, Struct_name, Param_name, Param_val, Structout, Pos=1
;
; MODIFICATION HISTORY:
; 	
;-

pro struct_expand, struct_val, param_name, param_val, structout, pos=pos


; Cette procedure permet d'ajouter un champ de tout type avec sa valeur dans une structure déja définie.
; En fait, la procedure crée une nouvelle structure contenant le nouveau champ, et recopie les valeurs de
; la structure initiale dans la nouvelle structure.


if n_params() eq 0 then begin
	print, 'Calling sequence:'
	print, '	STRUCT_EXPAND, struct_name, param_name, param_val, structout, pos=pos'
	return
endif


help, struct_val, /str, out=structcomp

structcomp = structcomp(1:*)
structcomp = strupcase(strtrim(structcomp, 2))
param_name = strupcase(param_name)

n_champ = n_elements(structcomp)
for i=0,n_champ-1 do begin
	structcomp(i) = strmid(structcomp(i), 0, strpos(structcomp(i), ' '))
endfor



if not keyword_set(pos) then pos = n_champ + 1

if pos gt n_champ then begin
	structcomp = [structcomp, param_name]
endif else if pos eq 1 then begin
	structcomp = [param_name, structcomp]
endif else begin
	structcomp = [structcomp(0:pos-1), param_name, structcomp(pos:*)]
endelse



commande = ''

for i=0, n_champ do begin
	if structcomp(i) eq param_name then begin
		commande = commande + ', ' + param_name + ':param_val'
	endif else begin
		commande = commande + ', ' + structcomp(i) +':struct_val.' + structcomp(i)
	endelse
endfor

commande = strmid(commande, 1, strlen(commande))
commande = 'structout = {' + commande + '}'
;print, commande
status = execute(commande)


return
end
