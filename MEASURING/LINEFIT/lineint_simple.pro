; lineint3.pro
; determine line center and line fluxes first determine cont.

function lineint_simple,wavelngth,flux,XRANGE = xrange, OLDDATA=olddata, CONT=cont

dataline = {gmaxx:0d0,gmaxy:0d0,gwidth:0d0,gflux:0d0,gew:0d0,gewl:0d0, $
	    maxx:0d0, maxy:0d0, flux:0d0,             ew:0d0, ewl:0d0, $
	    meancont:0d0,cont:0d0,starcont:0d0, remark:'' }

if not keyword_set(olddata) then begin
	newdata = replicate(dataline,1)
endif else begin
	newdata = olddata
end

contx =fltarr(2)
conty =fltarr(2)
x2range = fltarr(2)
y2range = fltarr(2)
if keyword_set(xrange) then begin
	indx  = where(wavelngth ge xrange(0) and wavelngth le xrange(1))
 	wavelngth = wavelngth(indx)
	flux = flux(indx)
end

if not keyword_set(cont) then begin

; plot the data end let user indicate the continuum

xc =fltarr(40)
yc =fltarr(40)
!err = 0
i = 0

plot,wavelngth,flux,psym=0,ystyle=16

Cursor,a,b,2,/down
xc(i) = a
yc(i) = b
while (!err ne 4) AND (i lt 39)  DO begin
	!err = 0
	print,xc(i),yc(i)
	i = i + 1
	cursor,a,b,2,/down
	xc(i) = a
	yc(i) = b
EndWhile

xcont = wavelngth
ycont = spline(xc(0:i-1),yc(0:i-1),xcont)

;plotaar,cont,/oplot,psym=0
;write_fitstable,cont,'lineint3_cont.aar'
end

jump2: 
plot,wavelngth,flux,psym=0,ystyle=16
oplot,wavelngth,ycont,psym=0

jump1:
print, 'Now select the part you want to use for the line flux with the mouse...'
wait,0.5
cursor,x,y
print,x,y
x2range(0) = x
y2range(0) = y
wait,0.5
cursor,x,y
print,x,y
x2range(1) = x
y2range(1) = y
if x2range(0) ge x2range(1) then begin
	print,'you chose first the longs- and next the short wavelength limit;'
	print,'Zooming: '
	plot,wavelngth,flux,xr=[x2range(1),x2range(0)],psym=0,ystyle=16
	oplot,wavelngth,ycont,psym=0
	goto, jump1
endif

lineindx = where(wavelngth ge x2range(0) and wavelngth le x2range(1))
linewavelngth = wavelngth(lineindx)
lineflux = flux(lineindx)

contx(0) = x2range(0)
conty(0) = y2range(0)
contx(1) = x2range(1)
conty(1) = y2range(1)

;get dimensions and wavelengths from line-aar
contwavelngth = linewavelngth
contflux = lineflux

;make interpolation for continuum
fluxstep = (conty(1)-conty(0))/n_elements(contwavelngth)
for ik=1,n_elements(contwavelngth)-2 do begin
    contflux(ik) = conty(0) + ik*fluxstep
endfor

;substract underlying cont.
lineflux = lineflux - contflux
plot,linewavelngth,lineflux,psym=0,ystyle=16

;get dimensions and wavelengths from line-aar 
fitwavelngth = linewavelngth
fitflux = lineflux

fitflux = GAUSSFIT(linewavelngth,lineflux,A,nterms=3)
oplot,fitwavelngth,fitflux,psym=1

print,'---> gauss-fit: Y = h * exp( ( x - lc ) /b ) ' 
print,'---> centrale golflengte (lc): ',A(1),' micron
print,'---> hoogte (h): ',A(0),' Jansky'
print,'---> breedte (b): ',A(2),' micron'
;oppervlak onder gauss = breedte * hoogte * sqrt(2*pi)
print,'---> gaussische lijn flux: ',A(2)*A(0)*2.5066283,' JanskyMicron'
print
foo = lineflux

maxflux = max(foo,maxindex)

maxwavelngth = linewavelngth(maxindex)
binsize = ( contx(1) - contx(0) ) / ( n_elements(foo) - 1 )
totalflux = total(foo)*binsize

print,'---> data punten:' 
print,'---> golflengte maximum : ',maxwavelngth(0),' micron'
print,'---> hoogte : ',maxflux,' Jansky'
print,'---> lijn flux: ',totalflux,' JanskyMicron'
K_atm = km(maxwavelngth(0),print='no')


foo = moment(contflux)
print
print,'---> gemiddelde continuum flux: ',foo(0),' Jansky'
print,'---> Flux bij centrale golflengte: ',contflux(maxindex),' Jansky'
print,'---> flux over continuum: ',maxflux/contflux(maxindex)
print,'---> Equivalente breedte gauss: ',A(2)*A(0)*2.5066283/foo(0)*1e4,' Angstrom'
print,'---> Equivalente breedte data punten: ',totalflux/foo(0)*1e4,' Angstrom'
print,'---> EW/l gauss: ',A(2)*A(0)*2.5066283/foo(0)/A(1)
print,'---> EW/l data: ',totalflux/foo(0)/maxwavelngth(0)
print,'---> Cont/Ster-cont : ',foo(0)/K_atm

print,'Click left of the plot to narrow the line,in the plot to restart.'
print,'Below zero Jansky to store this measurement'

wait,0.5
cursor,x,y
if (x lt x2range(0) or x gt x2range(1)) then begin
   goto, jump1
endif

if y lt 0 then begin
tempdata = replicate(dataline,n_elements(newdata)+1)
for i = 1,n_elements(newdata)-1 do begin
	tempdata(i).gmaxx = newdata(i).gmaxx
	tempdata(i).gmaxy = newdata(i).gmaxy
	tempdata(i).gwidth = newdata(i).gwidth
	tempdata(i).gflux = newdata(i).gflux
	tempdata(i).gew = newdata(i).gew
	tempdata(i).gewl = newdata(i).gewl
	tempdata(i).maxx = newdata(i).maxx
	tempdata(i).maxy = newdata(i).maxy
	tempdata(i).flux = newdata(i).flux
	tempdata(i).ew = newdata(i).ew
	tempdata(i).ewl = newdata(i).ewl
	tempdata(i).meancont = newdata(i).meancont
	tempdata(i).cont = newdata(i).cont
	tempdata(i).starcont = newdata(i).starcont
	tempdata(i).remark = newdata(i).remark
endfor
datapointer = n_elements(newdata)
newdata=tempdata
newdata(datapointer).gmaxx = A(1) 
newdata(datapointer).gmaxy = A(0)
newdata(datapointer).gwidth = A(2)
newdata(datapointer).gflux = A(2)*A(0)*2.5066283*2.99d14/(A(1)*A(1))
newdata(datapointer).maxx = maxwavelngth(0)
newdata(datapointer).maxy = maxflux(0)
newdata(datapointer).flux = totalflux*2.99d14/(A(1)*A(1))
newdata(datapointer).starcont = K_atm
newdata(datapointer).meancont = foo(0)
newdata(datapointer).cont = contflux(maxindex)
newdata(datapointer).gew = A(2)*A(0)*2.5066283/foo(0)*1e4  
newdata(datapointer).ew = totalflux/foo(0)*1e4
newdata(datapointer).gewl = A(2)*A(0)*2.5066283/K_atm/A(1)
newdata(datapointer).ewl = totalflux/K_atm/maxwavelngth(0)
A = STRING(0)
print,'remark for this line:'
READ,A
newdata(datapointer).remark = A
save,newdata,file='lines_lineint3_tmp.dat'
endif else begin
	goto,jump2
end

print,'click left of the line to do another line,in the plot to stop.' 
wait,0.5
cursor,x,y
if (x lt x2range(0) or x gt x2range(1)) then begin
   goto, jump2
endif

return,newdata

end








