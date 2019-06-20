; dbfluxint.pro
; determine line center and line fluxes

pro dbfluxint, aar, xrange,n


contx =fltarr(n)
conty =fltarr(n)
x2range = fltarr(2)
midwave = (xrange(0) + xrange(1))/2.0
resol = resolution(midwave,lambda_to_aot(midwave))/2.0
oversam = 4

aar1 = select(aar, aar.data.wave ge xrange(0) and aar.data.wave le xrange(1)) 
mnmx = minmax(aar1.data.flux)
aar1.data.flux = smooth(aar1.data.flux,5)
troep = resol
aar2 = sws_rebin(aar1,res=troep,over=oversam)


; Now give the real underlying continuum
jump1: plotaar,aar1,xrange=xrange,/xsty,color=0,psym=1
plotaar,aar2,psym=2,/oplot,color=145
print, 'Give the real underlying continuum'
print, 'Now select',n,' points which indicate were the underlying continuum is...'

for ki=0,n-1 do begin
  wait,0.5
  cursor,x,y
  print,x,y
  contx(ki) = x
  conty(ki) = y
  xyouts,x,y,'X',alignment=0.4,color=100
endfor


print, 'Ok!'

aar3 = aar2
yfit = poly_fit(contx, conty, n-2)
fluxw2 = fltarr(n_elements(aar2.data))
fluxw2 = 0
for ik=0,n-2 do begin
    fluxw2  = fluxw2 + yfit(ik)*aar2.data.wave^(ik)
endfor 
oplot,aar2.data.wave,fluxw2,color=100
aar3.data.flux = aar2.data.flux / fluxw2

print,'Een keer klikken voor resultaat'
wait,0.5
cursor,x,y
plotaar,aar3,xrange=xrange,/xsty,color=0
print,'links van ',xrange(0),' is fout, rechts van ',xrange(0),' is goed' 
wait,0.5
cursor,x,y
if (x lt xrange(0)) then goto, jump1


jump2: print, 'Now select the part you want to use for the line flux with the mouse...'
oplot,xrange,[1.,1.]

wait,0.5
cursor,x,y
print,x,y
x2range(0) = x
xyouts,x,y,'X',alignment=0.4
wait,0.5
cursor,x,y
print,x,y
x2range(1) = x
xyouts,x,y,'X',alignment=0.4
print
print, 'Ok!'
print
print



aar4 = select(aar3, aar3.data.wave ge x2range(0) and aar3.data.wave le x2range(1))
gyfit = GAUSSFIT(aar4.data.wave,aar4.data.flux,A)

fluxw4 = fltarr(n_elements(aar4.data))
fluxw4 = 0
backgauss = 0 
for ik=0,n-2 do begin
    fluxw4  = fluxw4 + yfit(ik)*aar4.data.wave^(ik)
    backgauss = backgauss + yfit(ik)*A(1)^(ik)
endfor 

gaussmax = backgauss * (A(0) + A(3) + A(4)*A(1) + A(5)*A(1)^2)
maxi1 = max(gyfit,waar1)
maxim1 = maxi1 * fluxw4(waar1)
maxim2 = max((gyfit*fluxw4),waar2)
FWHM = 2*A(2)*sqrt(-2*alog(0.5))

resdif = A(1) / abs(aar4.data(waar1).wave - A(1)) 
plotaar,aar3,xrange=xrange,/xsty,color=0
oplot,aar4.data.wave,gyfit

print,'Binnen het plaatje is goed erbuiten is fout.' 
wait,0.5
cursor,x,y
if (x lt xrange(0) or x gt xrange(1)) then begin
  plotaar,aar3,xrange=xrange,/xsty,color=0
  goto, jump2
endif


  
  


  doorgaanup = 0.
  falseup = 0
  falsedown = 0
  doorgaandown = 0.
if (resdif lt resol/1.5) then begin
  print,'Gauss fit niet zo goed',resdif
  print,'golflengte volgens maximum methode:',aar4.data(waar1).wave 
  print,'golflengte volgens gauss   methode:',A(1)
  maxvalue = (maxi1 + 1.0)/2.0
  for k =1,(n_elements(gyfit)-1) do begin
     if (doorgaanup eq 0. and falseup eq 0) then $
	if (waar1+k lt n_elements(gyfit)-1) then begin $
  if (gyfit(waar1+k) lt maxvalue) then doorgaanup = aar4.data(waar1+k).wave
         endif else begin
           if (doorgaanup eq 0) then falseup = 1
        endelse
   if (doorgaandown eq 0. and falsedown eq 0) then $
	if (waar1-k gt -1) then begin $
  if (gyfit(waar1-k) lt maxvalue) then doorgaandown = aar4.data(waar1-k).wave
         endif else begin
           if (doorgaandown eq 0) then falsedown = 1
        endelse
  endfor
  if (falsedown eq 1) then begin 
     if (falseup eq 1) then begin
        print,'geen goede FWHM te bepalen!!'
     endif else begin
       print,'Niet geheel betrouwbare FWHM'
       FWHM = 2*abs(aar4.data(waar1).wave - doorgaanup)
     endelse
  endif else begin
     if (falseup eq 1) then begin
       print,'Niet geheel betrouwbare FWHM'
       FWHM = 2*abs(aar4.data(waar1).wave - doorgaandown)
     endif else begin
       FWHM = doorgaanup - doorgaandown
     endelse
   endelse
endif





mean = 0d
lf = 0d

print,'Nu weer in gewone flux'



!p.thick=1.0
plotaar,aar,xrange=xrange,/xsty,color=0,psym=3,yrange=mnmx
plotaar,aar2,xrange=xrange,color=0,/oplot,psym=4
xyouts,aar4.data(waar1).wave,(maxim1+(mnmx(1) - mnmx(0))*0.15),'||',alignment=0.6
xyouts,aar4.data(waar1).wave,(maxim1+(mnmx(1) - mnmx(0))*0.125),'||',alignment=0.6
xyouts,aar4.data(waar1).wave,(maxim1+(mnmx(1) - mnmx(0))*0.1),'\/',alignment=0.5
plaatsx = xrange(0) + (xrange(1) - xrange(0))*0.05
plaatsy = mnmx(0) + (mnmx(1) - mnmx(0))*0.03
string = 'Lambda centraal ='+string(aar4.data(waar1).wave)
xyouts,plaatsx,plaatsy,string
!p.thick=6.0
oplot,aar4.data.wave,gyfit*fluxw4,color=70
oplot,aar2.data.wave,fluxw2,color=100
print,'Hardcopies (genormaliseerd en ongenormaliseerd)?'
print,'In het plaatje is ja, erbuiten is nee' 
wait,0.5
cursor,x,y
if (x gt xrange(0) and x lt xrange(1)) then begin
  print,'Ja'
  start_hardcopy
        plotaar,aar3,xrange=xrange,/xsty,color=0
        oplot,aar4.data.wave,gyfit
	!p.thick=1.0
	plotaar,aar,xrange=xrange,/xsty,color=0,psym=3,yrange=mnmx
	plotaar,aar2,xrange=xrange,color=0,/oplot,psym=4
	xyouts,aar4.data(waar1).wave,(maxim1+(mnmx(1) - mnmx(0))*0.15),'||',alignment=0.6
	xyouts,aar4.data(waar1).wave,(maxim1+(mnmx(1) - mnmx(0))*0.125),'||',alignment=0.6
	xyouts,aar4.data(waar1).wave,(maxim1+(mnmx(1) - mnmx(0))*0.1),'\/',alignment=0.5
	plaatsx = xrange(0) + (xrange(1) - xrange(0))*0.05
	plaatsy = mnmx(0) + (mnmx(1) - mnmx(0))*0.03
	string = 'Lambda centraal ='+string(aar4.data(waar1).wave)
	xyouts,plaatsx,plaatsy,string
	!p.thick=6.0
	oplot,aar4.data.wave,gyfit*fluxw4,color=0
	oplot,aar2.data.wave,fluxw2,color=0
  stop_hardcopy
endif else begin
  print,'nee'
endelse
!p.thick=1.0

print
print
print, 'All fit parameters:'
print, 'AOT band     			=              ',lambda_to_aot(A(1))
print
if (falsedown eq 1 or falseup eq 1) then print,'Onbetrouwbare FWHM'
print, 'FWHM          			=',FWHM, ' micron.'
print, 'Centrale golflengte (max)      	=',aar4.data(waar1).wave, ' micron'
print, 'Sterkte van de lijn bij de top 	=',maxim1,' Jansky'
print, 'Centrale golflengte (gauss)	=',A(1), ' micron'
if (resdif lt resol/1.5) then begin
  print,'Gauss fit niet zo goed',resdif
  print,'golflengte volgens maximum methode:',aar4.data(waar1).wave 
  print,'golflengte volgens gauss   methode:',A(1)
  A(1) = aar4.data(waar1).wave
endif
print, 'Maximum volgens Gauss		=',gaussmax, ' Jansky'
print, 'Centrale golflengte (schijnbaar)=',aar4.data(waar2).wave, ' micron'
print, 'Schijnbare top van de lijn     	=',maxim2,' Jansky'
print
print, 'Hoogte van de Gaussian (zelfde res)   	=',maxi1,' keer continuum'
print, 'Hoogte van de Gaussian (volgens formule)=',(A(0) + A(3) + A(4)*A(1) + A(5)*A(1)^2),' keer continuum'
print, 'Breedte van de Gaussfunctie    	        =',A(2) 
print, 'Constante van de lineare fit   		=',A(3)
print, 'Helling van de lineare fit		=',A(4)
print, 'Kwadratische van de lineare fit		=',A(5)
print

aar4.data.flux = (aar4.data.flux*fluxw4 - fluxw4)
mean = total(aar4.data.flux)/n_elements(aar4.data)
lf = mean*(xrange(1)-xrange(0))
print, 'Line Flux Integral data     :', lf, ' Jy micron'
lf = 2.99792458E-12*lf/(A(1)*A(1))
print, 'Line Flux data    :', lf, 'W m^-2'


gyfit = (gyfit * fluxw4) - fluxw4

mean = total(gyfit)/n_elements(gyfit)
lf = mean*(xrange(1)-xrange(0))
print, 'Line Flux Integral fit      :', lf, ' Jy micron'
lf = 2.99792458E-12*lf/(A(1)*A(1))
print, 'Line Flux fit     :', lf, 'W m^-2'


fluxw1 = fltarr(n_elements(aar1.data))
fluxw1 = 0
for ik=0,n-2 do begin
    fluxw1  = fluxw1 + yfit(ik)*aar1.data.wave^(ik)
endfor 

aar1.data.flux = aar1.data.flux - fluxw1
aarraw = select(aar1, aar1.data.wave ge x2range(0) and aar1.data.wave le x2range(1))
mean = total(aarraw.data.flux)/n_elements(aarraw.data.flux)
lf = mean*(xrange(1)-xrange(0))
print, 'Line Flux integral raw data :', lf, ' Jy micron'
lf = 2.99792458E-12*lf/(A(1)*A(1))
print, 'Line Flux raw data:', lf, 'W m^-2'


end
