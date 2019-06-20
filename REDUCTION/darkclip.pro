; darkclip.pro
; Removes dark measurements outside a given range from spd.

function darkclip, in_spd, range=range

; Check for valid input
if not is_spd(in_spd) then error,'F','No valid SPD structure specified!'

spd = in_spd
dk = select(spd, test_status(spd,/dark))
;plotspd,dk,det=indgen(52)

if keyword_set(range) then begin
  minimum = range(0)
  maximum = range(1)
endif

for i = 0,51 do begin

plotspd,dk,det=i+1
if not(keyword_set(range)) then begin
  print,'Minimum value'
  read,minimum
  print,'maximum value'
  read,maximum
endif

;for i = 0,51 do begin
  dk2 = select(dk, dk.data.det(i).flux lt minimum or dk.data.det(i).flux gt maximum)
  if is_spd(dk2) then begin
    for j = 0,n_elements(dk2.data)-1 do begin
      idx1 = where(spd.data.itk eq dk2.data(j).itk)
      spd.data(idx1).det(i).flag=make_flag(flag=spd.data(idx1).det(i).flag,/nodata)
    endfor
    print,format='("Removed ", I2, " points from det ", I2)', n_elements(dk2.data), i
  endif else begin
    print,format='("Removed 0 points from det ", I2)', i
  endelse    
endfor

dk = select(spd, test_status(spd,/dark))
plotspd,dk,det=indgen(52)

return,spd

end 

