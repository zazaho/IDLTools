function contlineint3,FILE=file

if not keyword_set(file) then begin
	file = 'lineint3_cont.aar'
endif

cont = read_faar(file)
return,cont

end
