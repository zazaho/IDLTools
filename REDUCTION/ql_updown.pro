pro ql_updown,a,rsol

a = cut_aot_bands(a)
up = getscan(a,/up)
down = getscan(a,/down)
fup = sws_flatfield(up,sig=2.)
fdown = sws_flatfield(down,sig=2)
sup = sigclip(fup,sig=2.)
sdown = sigclip(fdown,sig=2.)
rup = sws_rebin(sup,res=rsol,met='mean')
rdown = sws_rebin(sdown,res=rsol,met='mean')
dmin = min(rdown.data.flux)
dmax = max(rdown.data.flux)
umax = max(rup.data.flux)
umin = min(rup.data.flux)

down.data.flux = down.data.flux + 10000
plotaar,down,/flag,yr=[min([dmin,umin]),max([dmax,umax])]
plotaar,rdown,psym=0,/oplot
plotaar,rup,psym=0,/oplot
veri
print,'Druk op de linker muisknop voor flags van de upscan(boven)'
print,'rechts stopt'
Cursor,x,y,2,/down
if !err ne 4 then begin
	up.data.flux = up.data.flux + 10000
	plotaar,up,/flag,yr=[min([dmin,umin]),max([dmax,umax])]
        plotaar,rup,psym=0,/oplot
	plotaar,rdown,psym=0,/oplot
	veri
Endif

end


