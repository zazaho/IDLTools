; getflux.pro
; determine line center and line fluxes

pro dubbel, aarf, aarr, xrange


contgx =fltarr(20)
contgy =fltarr(20)
x2range = fltarr(2)
x3range = fltarr(2)
x4range = fltarr(2)
realrange = xrange
;midwave = (xrange(0) + xrange(1))/2.0
resol = 500 
;resol = resolution(midwave,lambda_to_aot(midwave))/2.0
;oversam = 4
c=2.99792458E-12

aar1 =sh_select(aarf, aarf.data.wave ge xrange(0) and aarf.data.wave le xrange(1)$
             and (test_flag(aarf.data.flag,/nodata) eq 0) )
aar2 =sh_select(aarr, aarr.data.wave ge xrange(0) and aarr.data.wave le xrange(1)$
             and (test_flag(aarr.data.flag,/nodata) eq 0) )
mnmx = minmax(aar2.data.flux)
help = mnmx
aar2.data.flux = aar2.data.flux * c / (aar2.data.wave^2)
extreem = minmax(aar2.data.flux)
hulpje = extreem
aar2.data.flux = aar2.data.flux * (aar2.data.wave^2)/c

;aar2 = sws_rebin(aar1,res=resol,over=oversam)


;*************************************************************
;* Hier wordt bepaald welk figur het makkelijkst is om het   *
;* continuum te bepalen                                      *
;*************************************************************

jump0: aar1.data.flux = aar1.data.flux * c / (aar1.data.wave^2)
aar2.data.flux = aar2.data.flux * c / (aar2.data.wave^2)
plot,aar1.data.wave,aar1.data.flux,xrange=xrange,/xsty,psym=3,yra=extreem
print,'dit lukt wel...'
xrange = realrange
extreem = hulpje
oplot,aar2.data.wave,aar2.data.flux,psym=1,color=145


print,'F_lambda: In het plaatje is goed, erbuiten is fout.'
wait,0.5
cursor,x,y
if (x gt xrange(0) and x lt xrange(1)) then begin
  flambda = 1
  onth = extreem
  onthouden = extreem
  goto, jump1b
endif

aar1.data.flux = aar1.data.flux * (aar1.data.wave^2)/c
aar2.data.flux = aar2.data.flux * (aar2.data.wave^2)/c
plot,aar1.data.wave,aar1.data.flux,xrange=xrange,/xsty,psym=3,yra=mnmx
xrange = realrange
mnmx = help
oplot,aar2.data.wave,aar2.data.flux,psym=1,color=145


print,'F_nu: In het plaatje is goed, erbuiten is fout.'
wait,0.5
cursor,x,y
if (x gt xrange(0) and x lt xrange(1)) then begin
  flambda = 0
  onth = mnmx
  onthouden = mnmx
  goto, jump1b
endif
goto, jump0 


;*************************************************************
;* Now give the real underlying continuum                    *
;*************************************************************

jump1: plot,aar1.data.wave,aar1.data.flux,xrange=xrange,/xsty,psym=3,yra=onth
onth = onthouden
oplot,aar2.data.wave,aar2.data.flux,psym=1,color=145
jump1b: print, 'Give the real underlying continuum'
print, 'Now select points which indicate were the underlying continuum is...'
print, 'outside is enough points'

for n=0,19 do begin
  wait,0.5
  cursor,x,y
  contgx(n) = x
  contgy(n) = y
  if (x lt xrange(0) or x gt xrange(1)) then goto, jojo
  print,x,y
  xyouts,x,y,'X',alignment=0.4,color=100
endfor
jojo: print, 'OK!'
xrange = realrange
contx = fltarr(n)
conty = fltarr(n)
contx(0:(n-1)) = contgx(0:(n-1))
conty(0:(n-1)) = contgy(0:(n-1))


aar3 = aar2
fluxw2 = spline(contx,conty,aar2.data.wave)
oplot,aar2.data.wave,fluxw2,color=100
aar3.data.flux = aar2.data.flux - fluxw2
aar3.data.flux = smooth(aar3.data.flux,10)

print,'Een keer klikken voor resultaat'
wait,0.5
cursor,x,y
xrange = realrange
plot,aar3.data.wave,aar3.data.flux,xrange=xrange,/xsty
print,'In het plaatje is goed, erbuiten is fout.'
wait,0.5
cursor,x,y
if (x lt xrange(0) or x gt xrange(1)) then goto, jump1

if (flambda eq 0) then begin
  aar1.data.flux = aar1.data.flux * c/(aar1.data.wave^2)
  aar2.data.flux = aar2.data.flux * c/(aar2.data.wave^2) 
  aar3.data.flux = aar3.data.flux * c/(aar3.data.wave^2)
  conty = conty* c/(contx*contx)
  fluxw2 = fluxw2 * c/(aar2.data.wave^2)
  plot,aar3.data.wave,aar3.data.flux,xrange=xrange,/xsty
xrange = realrange
endif



;**************************************************************
;* Hier worden de stukken spectrum geselecteerd die opgemeten *
;* zouden moeten gaan worden.                                 *
;**************************************************************

jump2: print, 'Now select the part you want to use for the line flux with the mouse...'
oplot,xrange,[0.,0.]
wait,0.5
cursor,x,y
print,x,y
x2range(0) = x
xyouts,x,y,'X',alignment=0.4,color=100
wait,0.5
cursor,x,y
print,x,y
x2range(1) = x
xyouts,x,y,'X',alignment=0.4,color=100
print
print, 'Ok!'
print
print

aar4 = sh_select(aar3, aar3.data.wave ge x2range(0) and aar3.data.wave le x2range(1))
watje = minmax(aar4.data.flux)
if (watje(0) gt 0.5*watje(0)) then watje(0) = 0.


jump2b: print, 'Now select a part to determine the first gauss with the mouse...'
oplot,xrange,[0.,0.]
wait,0.5
cursor,x,y
print,x,y
x3range(0) = x
xyouts,x,y,'X',alignment=0.4
wait,0.5
cursor,x,y
print,x,y
x3range(1) = x
xyouts,x,y,'X',alignment=0.4
print
print, 'Ok!'
print
print

aar5 = sh_select(aar3, aar3.data.wave ge x3range(0) and aar3.data.wave le x3range(1))

gyfit = fltarr(n_elements(aar4.data.wave))
prutw = aar4.data.wave
prutf = aar4.data.flux

test3 = gaussfit(aar5.data.wave,(aar5.data.flux*1E+14),A3,nterms=3)

prutz = (-(prutw - A3(1))/A3(2))^2
;prutf = A3(0)*exp(- prutz /2.) + A3(3) + A3(4)*prutw + A3(5)*prutw^2.
prutf = A3(0)*exp(- prutz /2.)
oplot,prutw,prutf/1e+14
oplot,aar4.data.wave,(aar4.data.flux-prutf/1e+14),color=120

print,'Binnen het plaatje is goed erbuiten is fout.' 
wait,0.5
cursor,x,y
if (x lt xrange(0) or x gt xrange(1)) then begin
  plot,aar3.data.wave,aar3.data.flux,xrange=xrange,/xsty  
  xyouts,x2range(0),0,'X',alignment=0.4,color=100
  xyouts,x2range(1),0,'X',alignment=0.4,color=100
  goto, jump2b
endif


print, 'Now select a part to determine the second gauss with the mouse...'
wait,0.5
cursor,x,y
print,x,y
x4range(0) = x
xyouts,x,y,'X',alignment=0.4
wait,0.5
cursor,x,y
print,x,y
x4range(1) = x
xyouts,x,y,'X',alignment=0.4
print
print, 'Ok!'
print
print


prutje = aar4
prutje.data.flux = aar4.data.flux - prutf/1e+14

aar6 = sh_select(prutje, prutje.data.wave ge x4range(0) and prutje.data.wave le x4range(1))

test4 = gaussfit(aar6.data.wave,(aar6.data.flux*1E+14),A4,nterms=3)

lengte = x3range(1)- x3range(0)
nummer3 = fltarr(n_elements(aar4.data.wave))
nummer4 = fltarr(n_elements(aar4.data.wave))


for hij = 0,(n_elements(aar4.data.wave)-1) do begin
   x = aar4.data(hij).wave
   z3 = (x - A3(1))/A3(2)
   z4 = (x - A4(1))/A4(2)
;   nummer3(hij) = A3(0)*exp(-(z3^2)/2.) + A3(3) + A3(4)*x + A3(5)*x^2.
;   nummer4(hij) = A4(0)*exp(-(z4^2)/2.) + A4(3) + A4(4)*x + A4(5)*x^2.
   nummer3(hij) =  A3(0)*exp(-(z3^2)/2.)/1E+14
   nummer4(hij) =  A4(0)*exp(-(z4^2)/2.)/1E+14
   gyfit(hij) = nummer3(hij) + nummer4(hij)
endfor


;maxi3 = A3(0) + A3(3) + A3(4)*A3(1) + A3(5)*A3(1)^2 + $
;  A4(0)*exp(-(((A3(1) - A4(1))/A4(2))^2)/2) +A4(3) +A4(4)*A3(1)+A4(5)*A3(1)^2
maxi3 = (A3(0) + A4(0)*exp(-(((A3(1) - A4(1))/A4(2))^2)/2))/1E+14

;maxi4 = A4(0) + A4(3) + A4(4)*A4(1) + A4(5)*A4(1)^2 + $
;  A3(0)*exp(-(((A4(1) - A3(1))/A3(2))^2)/2) +A3(3) +A3(4)*A4(1)+A3(5)*A4(1)^2
maxi4 = (A4(0) + A3(0)*exp(-(((A4(1) - A3(1))/A3(2))^2)/2))/1E+14

print,'A3',A3
print,'A4',A4
FWHM3 = 2*A3(2)*sqrt(-2*alog(0.5))
FWHM4 = 2*A4(2)*sqrt(-2*alog(0.5))

oplot,aar4.data.wave,nummer4,color=60
oplot,aar4.data.wave,gyfit,color=140,linestyle=2



fluxw4 = spline(contx,conty,aar4.data.wave)
print,'Binnen het plaatje is goed erbuiten is fout.' 
wait,0.5
cursor,x,y
if (x lt xrange(0) or x gt xrange(1)) then begin
  plot,aar3.data.wave,aar3.data.flux,xrange=xrange,/xsty
  goto, jump2
endif


mean = 0d
lf = 0d

print,'Nu weer in gewone flux'
aar1.data.flux = aar1.data.flux * (aar1.data.wave^2)/c
aar2.data.flux = aar2.data.flux * (aar2.data.wave^2)/c
aar3.data.flux = aar3.data.flux * (aar3.data.wave^2)/c
aar4.data.flux = aar4.data.flux * (aar4.data.wave^2)/c
gyfit = gyfit * (aar4.data.wave^2)/c
conty = conty*contx*contx/c
fluxw2 = fluxw2 * (aar2.data.wave^2)/c
fluxw4 = fluxw4 * (aar4.data.wave^2)/c
maxi3 = maxi3 * (A3(1))^2/c
maxi4 = maxi4 * (A4(1))^2/c

waaris3 = where(aar2.data.wave gt A3(1)*0.999 and aar2.data.wave lt A3(1)*1.01) 
waaris4 = where(aar2.data.wave gt A4(1)*0.999 and aar2.data.wave lt A4(1)*1.01) 

!p.thick=1.0
print,mnmx,maxi3
plot,aar1.data.wave,aar1.data.flux,xrange=xrange,/xsty,psym=3,yrange=mnmx
mnmx = help
oplot,aar2.data.wave,aar2.data.flux,psym=4
xyouts,A3(1),(maxi3 + fluxw2(waaris3(0))+(mnmx(1) - mnmx(0))*0.15),'||',alignment=0.6
xyouts,A3(1),(maxi3 + fluxw2(waaris3(0))+(mnmx(1) - mnmx(0))*0.125),'||',alignment=0.6
xyouts,A3(1),(maxi3 + fluxw2(waaris3(0))+(mnmx(1) - mnmx(0))*0.1),'\/',alignment=0.5
xyouts,A4(1),(maxi4 + fluxw2(waaris4(0))+(mnmx(1) - mnmx(0))*0.15),'||',alignment=0.6
xyouts,A4(1),(maxi4 + fluxw2(waaris4(0))+(mnmx(1) - mnmx(0))*0.125),'||',alignment=0.6
xyouts,A4(1),(maxi4 + fluxw2(waaris4(0))+(mnmx(1) - mnmx(0))*0.1),'\/',alignment=0.5
plaatsx = xrange(0) + (xrange(1) - xrange(0))*0.05
plaatsy = mnmx(0) + (mnmx(1) - mnmx(0))*0.03
string = 'Lambda centraal ='+string(A3(1))+' en '+string(A4(1))
xyouts,plaatsx,plaatsy,string
!p.thick=6.0
oplot,aar4.data.wave,gyfit+fluxw4,color=70
oplot,aar2.data.wave,fluxw2,color=100
print,'Hardcopies (genormaliseerd en ongenormaliseerd)?'
print,'In het plaatje is ja, erbuiten is nee' 
wait,0.5
cursor,x,y
if (x gt xrange(0) and x lt xrange(1)) then begin
  print,'Ja'
  start_hardcopy
	!p.thick=1.0
        plot,aar3.data.wave,aar3.data.flux,xrange=xrange,/xsty
	!p.thick=6.0
        oplot,aar4.data.wave,gyfit
	!p.thick=1.0

	plot,aar1.data.wave,aar1.data.flux,xrange=xrange,/xsty,psym=3,yrange=mnmx
        mnmx = help
    	oplot,aar2.data.wave,aar2.data.flux,psym=4
	xyouts,A3(1),(maxi3 + fluxw2(waaris3(0))+(mnmx(1) - mnmx(0))*0.15),'||',alignment=0.6
	xyouts,A3(1),(maxi3 + fluxw2(waaris3(0))+(mnmx(1) - mnmx(0))*0.125),'||',alignment=0.6
	xyouts,A3(1),(maxi3 + fluxw2(waaris3(0))+(mnmx(1) - mnmx(0))*0.1),'\/',alignment=0.5
	xyouts,A4(1),(maxi4 + fluxw2(waaris4(0))+(mnmx(1) - mnmx(0))*0.15),'||',alignment=0.6
	xyouts,A4(1),(maxi4 + fluxw2(waaris4(0))+(mnmx(1) - mnmx(0))*0.125),'||',alignment=0.6
	xyouts,A4(1),(maxi4 + fluxw2(waaris4(0))+(mnmx(1) - mnmx(0))*0.1),'\/',alignment=0.5
	plaatsx = xrange(0) + (xrange(1) - xrange(0))*0.05
	plaatsy = mnmx(0) + (mnmx(1) - mnmx(0))*0.03
	string = 'Lambda centraal ='+string(A3(1))+' en '+string(A4(1))
	xyouts,plaatsx,plaatsy,string
	!p.thick=6.0
	oplot,aar4.data.wave,gyfit+fluxw4
	oplot,aar2.data.wave,fluxw2
  stop_hardcopy
endif else begin
  print,'nee'
endelse
!p.thick=1.0



print
print
print, 'All fit parameters:'
print, 'AOT band     			=              ',lambda_to_aot(A3(1))
print
print, 'FWHM top 1          		=',abs(FWHM3), ' micron.'
print, 'Centrale golflengte (top 1)     =',A3(1),' micron'
print, 'Sterkte in het midden (top 1)  	=',maxi3,' Jansky'

print, 'FWHM top 2          		=',abs(FWHM4), ' micron.'
print, 'Centrale golflengte (top 2)     =',A4(1),' micron'
print, 'Sterkte in het midden (top 2)  	=',maxi4,' Jansky'

	keercont = (fluxw2(waaris3(0)) + maxi3)/ fluxw2(waaris3(0))
print, 'I_p/I_c van top 1		=',keercont
	keercont = (fluxw2(waaris4(0)) + maxi4)/ fluxw2(waaris4(0))
print, 'I_p/I_c van top 2		=',keercont

print



aar4.data.flux = aar4.data.flux * c / (aar4.data.wave^2)
mean = total(aar4.data.flux)/n_elements(aar4.data)
lf = mean*(x2range(1)-x2range(0))
print, 'Line flux totale gerebinde data :', lf, 'W m^-2'



gyfit = gyfit * c/(aar4.data.wave^2)
mean = total(gyfit)/n_elements(gyfit)
lf = mean*(x2range(1)-x2range(0))
print, 'Line Flux dubbel gauss fit      :', lf, 'W m^-2'

mean = total(nummer3)/n_elements(nummer3)
lf = mean*(x2range(1)-x2range(0))
print, 'Line Flux gauss fit top 1       :', lf, 'W m^-2'

mean = total(nummer4)/n_elements(nummer4)
lf = mean*(x2range(1)-x2range(0))
print, 'Line Flux gauss fit top 2       :', lf, 'W m^-2'


fluxw1 = spline(contx,conty,aar1.data.wave)
aar1.data.flux = (aar1.data.flux - fluxw1)* c/(aar1.data.wave^2)
aarraw = sh_select(aar1, aar1.data.wave ge x2range(0) and aar1.data.wave le x2range(1))
mean = total(aarraw.data.flux)/n_elements(aarraw.data.flux)
lf = mean*(x2range(1)-x2range(0))
print, 'Line Flux raw data              :', lf, 'W m^-2'


end
