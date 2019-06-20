pro collate_cubes,files,outfile
  
  for i=0,n_elements(files)-1 do begin
     cube = mrdfits(files[i],0,hdr)
     wave = mrdfits(files[i],1,hdr2)
     
     if n_elements(totalcube) eq 0 then begin
        totalcube = cube
        totalwave = wave
     endif else begin
        totalcube = [[[totalcube]],[[cube]]]
        totalwave = [totalwave,wave]
     endelse
  endfor

  idx=sort(totalwave)
  totalcube = totalcube[*,*,idx]
  totalwave = totalwave[idx]

  mwrfits,totalcube,outfile,hdr,/create
  mwrfits,totalwave,outfile,hdr2
end
