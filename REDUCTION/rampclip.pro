; This routine first plot the number of datapoints which are used
; to determine the ramp. Hereafter you give a minimum value for
; the number of "ramp determination points", and it will throw away
; all the points which are based on less ramp determination points.
; The input and output of this function is an spd.
;
; Example:
;
; spd1 = rampclip(spd0) 
;
; Arguments:
; spd0 = input spd
; spd1 = result spd



function rampclip, spd0, minimum=minimum

spd = spd0
aar = extract_aar(spd)
plot,(aar.data.itk-aar.data(0).itk)/24,aar.data.tint,psym=1
if not(keyword_set(minimum)) then begin
  print,'Minimum value'
  read,minimum
endif else begin
  print,format='("Minimum value: ", I2)',minimum
endelse
idx1 = where(aar.data.tint lt minimum, ct1)
print, 'Goede data',100.*(n_elements(aar.data)-ct1)/n_elements(aar.data),' %'

for i=0,ct1-1 do begin
 num1 = aar.data(idx1(i)).det
 punt = where(spd.data.itk eq aar.data(idx1(i)).itk) 
 spd.data(punt).det(num1-1).flag=make_flag(flag=spd.data(punt).det(num1-1).flag,/nodata)
endfor

return,spd

end 
