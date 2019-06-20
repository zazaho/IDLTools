pro extract_beam, cube, chop_pos, cubepos, cubeneg, count = count

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	extract_beam, cube, chop_pos, cube_pos, cube_neg'
	return
endif


idneg = where(chop_pos eq -1,countneg)
if (countneg gt 0) then begin
	cubeneg = cube(*,*,idneg)
endif else begin
	cubeneg = -1
endelse

idpos = where(chop_pos eq 1,countpos)
if (countpos gt 0) then begin
	cubepos = cube(*,*,idpos)
endif else begin
	cubepos = -1
endelse

count = countpos + countneg

return
end



