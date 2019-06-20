pro irasconv_aar,aarstart

; Eerst IRAS gevoeligheden inlezen.

golf = fltarr(87)
resp = fltarr(87)
Iras12 = fltarr(2,18)
Iras25 = fltarr(2,32)
Iras60 = fltarr(2,21)
Iras100 = fltarr(2,16)
openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS12mu', /get_lun
readf, lun, Iras12
free_lun, lun

for i=0,17 do begin
  golf(i) = IRAS12(0,i)
  resp(i) = IRAS12(1,i)
endfor

openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS25mu', /get_lun
readf, lun, Iras25
free_lun, lun

for i=0,31 do begin
  golf(i+18) = IRAS25(0,i)
  resp(i+18) = IRAS25(1,i)
endfor

openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS60mu', /get_lun
readf, lun, Iras60
free_lun, lun

for i=0,20 do begin
  golf(i+50) = IRAS60(0,i)
  resp(i+50) = IRAS60(1,i)
endfor

openR, lun,'${HOME}/IA_TOOL/FRANKM/IRAS100mu', /get_lun
readf, lun, Iras100
free_lun, lun

for i=0,15 do begin
  golf(i+71) = IRAS100(0,i)
  resp(i+71) = IRAS100(1,i)
endfor

aar1 = select(aarstart,aarstart.data.wave gt 7.0  and aarstart.data.wave lt 140.0)
aar = sws_rebin(aar1,bin=0.1,/noplot)
;aar = sws_rebin(aarstart,res=100)
lambda = aar.data.wave
jansky = aar.data.flux
stappen = n_elements(aar.data.wave)
print,'stappen',stappen

totaal1 = 0.0 
delen1 = 0.0
delta1 = 0.0
totaal2 = 0.0 
delen2 = 0.0
delta2 = 0.0
totaal3 = 0.0 
delen3 = 0.0
delta3 = 0.0
totaal4 = 0.0 
delen4 = 0.0
delta4 = 0.0

keer1 = 0
keer2 = 0
keer3 = 0
keer4 = 0

for j = 0,stappen-2 do begin
  if (lambda(j) gt 7.0 and lambda(j) lt 15.5) then begin
    for k1 = 0,17 do begin
        if (golf(k1) gt lambda(j)) then begin
           wver = golf(k1) - golf(k1 - 1)
           rver = resp(k1) - resp(k1 - 1)
           wdif = lambda(j) - golf(k1 - 1)
           factor = ((wdif / wver) * rver) + resp(k1 - 1)
	   delen1 = delen1 + factor
           totaal1 = totaal1 + jansky(j) * factor
           keer1 = keer1 + 1
           goto, jump1
        endif
    endfor
  endif  

jump1:  if (lambda(j) gt 16.0 and lambda(j) lt 31.5) then begin
    for k1 = 18,49  do begin
        if (golf(k1) gt lambda(j)) then begin
           wver = golf(k1) - golf(k1 - 1)
           rver = resp(k1) - resp(k1 - 1)
           wdif = lambda(j) - golf(k1 - 1)
           factor = ((wdif / wver) * rver) + resp(k1 - 1)
           delen2 = delen2 + factor
           totaal2 = totaal2 + jansky(j) * factor
           keer2 = keer2 + 1
           goto, jump2
        endif
    endfor
  endif  

jump2:  if (lambda(j) gt 27.0 and lambda(j) lt 87.0) then begin
    for k1 = 50,70 do begin
        if (golf(k1) gt lambda(j)) then begin
           wver = golf(k1) - golf(k1 - 1)
           rver = resp(k1) - resp(k1 - 1)
           wdif = lambda(j) - golf(k1 - 1)
           factor = ((wdif / wver) * rver) + resp(k1 - 1)
           delen3 = delen3 + factor
           totaal3 = totaal3 + jansky(j) * factor
           keer3 = keer3 + 1
           goto, jump3
        endif
    endfor
  endif  

jump3:  if (lambda(j) gt 65.0 and lambda(j) lt 140.) then begin
    for k1 = 71,86 do begin
        if (golf(k1) gt lambda(j)) then begin
           wver = golf(k1) - golf(k1 - 1)
           rver = resp(k1) - resp(k1 - 1)
           wdif = lambda(j) - golf(k1 - 1)
           factor = ((wdif / wver) * rver) + resp(k1 - 1)
           delen4 = delen4 + factor
           totaal4 = totaal4 + jansky(j) * factor
           keer4 = keer4 + 1
           goto, jump4
        endif
    endfor
  endif  
jump4:
endfor

print,totaal1,totaal2,totaal3,totaal4
print,delen1,delen2,delen3,delen4
print,keer1,keer2,keer3,keer4

plot,lambda,jansky,psym=3,xra=[lambda(0),lambda(n_elements(lambda)-1)]
;oplot,[12,25,60,100],[totaal1/delen1, totaal2/delen2, totaal3/delen3, totaal4/delen4],psym=2,color=200

if (delen1 gt 0) then begin
   Print,'IRAS FLUX 12 micron:',totaal1 / delen1
   oplot,golf(0:17),(resp(0:17)/resp(0:17))*(totaal1/delen1),psym=-3,color=80
   oplot,golf(0:17),resp(0:17)*(totaal1/delen1),psym=-3,color=80
endif

if (delen2 gt 0) then begin
   Print,'IRAS FLUX 25 micron:',totaal2 / delen2
 oplot,golf(18:49),(resp(18:49)/resp(18:49))*(totaal2/delen2),psym=-3,color=100
   oplot,golf(18:49),resp(18:49)*(totaal2/delen2),psym=-3,color=100
endif

if (delen3 gt 0) then begin
   Print,'IRAS FLUX 60 micron:',totaal3 / delen3
 oplot,golf(50:70),(resp(50:70)/resp(50:70))*(totaal3/delen3),psym=-3,color=120
   oplot,golf(50:70),resp(50:70)*(totaal3/delen3),psym=-3,color=120
endif

if (delen4 gt 0) then begin
   Print,'IRAS FLUX 100 micron:',totaal4 / delen4
 oplot,golf(71:86),(resp(71:86)/resp(71:86))*(totaal4/delen4),psym=-3,color=140
   oplot,golf(71:86),resp(71:86)*(totaal4/delen4),psym=-3,color=140
endif

;openW, lun,'aartest' , /get_lun
;tabel = fltarr(2,(n_elements(lambda)))
;tabel(0,0:(n_elements(lambda)-1)) = lambda
;tabel(1,0:(n_elements(lambda)-1)) = jansky
;printf, lun, tabel
;free_lun, lun
;
end
