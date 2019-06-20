; function sel_region_inter
;
; Function to extract one wavelength regio from an aar.

function sel_region_inter, aar

; Check for valid input
if not is_aar(aar) then error,'F','No valid AAR structure specified!'
xrange = fltarr(2)
new = aar


jump1:

plotaar,new,psym=0
wait,0.5
veri,x1,/nostatus
wait,0.5
veri,x2,/nostatus

if (x1 gt x2) then begin
	print,'zooming'
	new = select(new,(new.data.wave gt x2) AND (new.data.wave lt x1))
	goto,jump1
endif

; Select the region
new = select(new,(new.data.wave gt x1) AND (new.data.wave lt x2))
return,new

end





