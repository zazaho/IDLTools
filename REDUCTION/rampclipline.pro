; This routine first plots the number of datapoints which are used to 
; determine the ramp for one line (AOT2/6)/AOT sub-band (AOT1). Hereafter 
; you give a minimum value for the number of "ramp determination points" 
; for each band, and it will throw away all the points which are based on 
; less ramp determination points.
; Recommended values for clipping: 15, 39 or 87.
; The input and output of this function is an spd.
;
; Example:
;
; spd1 = rampclip(spd0) 
;
; Arguments:
; spd0 = input spd
; spd1 = result spd

function rampclipline, spd0

; Check for valid input
if not is_spd(spd0) then error,'F','No valid SPD structure specified!'

spd = spd0
aar = extract_aar(spd)

lines = aar.data(sort(aar.data.line)).line
lines = lines(uniq(lines))

for j = 0,n_elements(lines)-1 do begin
  aar2 = select(aar, aar.data.line eq lines(j))

  plot,aar2.data.tint,psym=1
  print,format='("Minimum waarde voor lijn ", I2)', lines(j)
  read,minimum
  idx1 = where(aar2.data.tint lt minimum, ct1)
  print,format='("Goede data lijn ", I2, ": ", F7.3, " %")', $
        lines(j), 100.*(n_elements(aar2.data)-ct1)/n_elements(aar2.data)

  for i=0,ct1-1 do begin
    num1 = aar2.data(idx1(i)).det
    punt = where(spd.data.itk eq aar2.data(idx1(i)).itk) 
    spd.data(punt).det(num1-1).flag=make_flag(flag=spd.data(punt).det(num1-1).flag,/nodata)
  endfor
endfor

return,spd
end
