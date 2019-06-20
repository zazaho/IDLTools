function header2struct, header, parametres


chaine = ''
for i=0, n_elements(parametres)-1 do begin
	valeur = fxpar(header, parametres(i))
	chaine = chaine + ', '+ parametres(i)+ ' :' + string(valeur)



endfor
chaine=chaine+'}'

chaine=strmid(chaine, 2, strlen(chaine))
chaine= 'bidon={'+chaine

return, chaine

end

