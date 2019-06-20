function addingspec,spec_in,spec=spec,image=image,noshow=noshow

; Program to make a spectrum out of a TIMMI2 spectral image.

; to add the positive and negative spectra
; spec_in = spectrum in

spec2 = spec_in

spec1 = fltarr(10+n_elements(spec_in(*,0)),10+n_elements(spec_in(0,*)))
spec1(5:(4+n_elements(spec_in(*,0))),5:(4+n_elements(spec_in(0,*)))) = spec_in

specsm = smooth(spec1,5)
tjop = fltarr(n_elements(spec_in(0,*)))
for ii=5,n_elements(spec_in(0,*))+4 do begin
  tjop(ii-5) = total(specsm(*,ii))
endfor 

;plot,tjop

tjop2 = tjop - median(tjop)
waar = where(tjop2 lt 0,ct)
if (ct gt 0) then tjop2(waar) = 0 

resultaat = gaussfit(indgen(n_elements(tjop2)),tjop2,A,NTERMS=3)

midden = fix(A(1)+0.5)

tjop3 = -tjop(0:midden) + median(tjop(0:midden))
waar3 = where(tjop3 lt 0,ct3)
if (ct3 gt 0) then tjop3(waar3) = 0 

;plot,tjop3

resultaat2 = gaussfit(indgen(n_elements(tjop3)),tjop3,B,NTERMS=3)

onder = fix(B(1)+0.5)

boven = 2*midden - onder

stap = ((midden-onder)/2)-1

if (n_elements(noshow) EQ 0) then begin
  print,'Midden=',midden
  print,'Onder =',onder
  print,'Boven =',boven
endif

if (onder - stap lt 0) then begin
  beneden = 0
endif else begin
  beneden = onder - stap
endelse  

if (boven + stap gt n_elements(spec2(0,*))) then begin
  bovenkant = 0
endif else begin
  bovenkant = n_elements(spec2(0,*)) - 1
endelse  

specpos = spec2(0:n_elements(spec_in(*,0))-1,midden-stap:midden+stap)
specneg1 = -spec2(0:n_elements(spec_in(*,0))-1,beneden:onder+stap)
specneg2 = -spec2(0:n_elements(spec_in(*,0))-1,boven-stap:bovenkant)


specend = specpos+specneg1+specneg2
result = total(specend,2)


if (n_elements(spec) EQ 0) then begin
  if (n_elements(noshow) EQ 0) then tvs,specend
  return,specend
endif else begin
  if (n_elements(noshow) EQ 0) then plot,result
  return,result
endelse

end
