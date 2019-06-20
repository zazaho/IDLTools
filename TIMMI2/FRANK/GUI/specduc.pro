; Tool to reduce TIMMI2 spectra.
; Made by Frank Molster, no guarantees provided.


;==================================================================
                             Pro Fit_event, event
;==================================================================

common tjip, lijst

Widget_Control, event.id, Get_uvalue=what
 case what.type of
  
  'quit'   :begin
            widget_control, event.top, /destroy
	    end


  'draw'   :begin
	      nieuwspec = plotfigure()	 
	      lijst.waves(0:(n_elements(nieuwspec(0,*))-1)) = nieuwspec(0,*)
	      lijst.fluxs(0:(n_elements(nieuwspec(0,*))-1)) = nieuwspec(1,*)
            end

  'reken'    :	begin
		 nieuwspec = plotfigure()	 
		 lijst.waves(0:n_elements(nieuwspec(0,*))-1) = nieuwspec(0,*)
		 lijst.fluxs(0:n_elements(nieuwspec(0,*))-1) = nieuwspec(1,*)
		end

  'test':	begin
		 Widget_control, event.id, get_uvalue=uv
                 store=uv.store
		 idx = event.index
		 case event.id of
		 store.srcid:	begin
				 lijst.srcnr = idx
		widget_control,store.airmsid,set_value=store.airms(idx)*100
				end
		 store.calid:	begin
				 lijst.calnr = idx
		widget_control,store.airmcid,set_value=store.airmc(idx)*100
				end
		 store.bandid:	begin
				 lijst.bandnr = idx
				end
		 endcase
		 nieuwspec = plotfigure()	 
		 lijst.waves(0:(n_elements(nieuwspec(0,*))-1)) = nieuwspec(0,*)
		 lijst.fluxs(0:(n_elements(nieuwspec(0,*))-1)) = nieuwspec(1,*)
		end

  'write'   :	begin
                 tabel = fltarr(2,n_elements(lijst.waves))
		 tabel(0,*) = lijst.waves
		 tabel(1,*) = lijst.fluxs
		 naamster = lijst.namss(lijst.srcnr) 
		 naamcal = lijst.namsc(lijst.calnr)
		 naampje= naamster+'_'+naamcal+'.tab'
		 openW, lun,naampje, /get_lun
		 printf, lun, tabel
		 free_lun, lun
		end

  else :	print,'Niets dus'

endcase

end

;==============================================================
; Plot new figure

function plotfigure

common tjip,lijst

  Widget_Control, lijst.shiftid, get_value=lshift
  Widget_Control, lijst.airmsid, get_value=airms
  Widget_Control, lijst.airmcid, get_value=airmc

  ymax = max([max(lijst.src(lijst.srcnr,*)),max(lijst.cal(lijst.calnr,*))])

;  wavecal = leescalwave(lijst.band(lijst.bandnr))
  wavecal = lijst.waveom(lijst.bandnr,*)
		   
  lijst.wavec(0:n_elements(wavecal)-1) = wavecal
  if (n_elements(lijst.wavec) lt n_elements(wavecal)) then begin
    lijst.wavec(n_elements(wavecal):n_elements(lijst.wavec)-1) = 0
  endif

  newspec=shiftair(lijst.wavec, $
  lijst.cal(lijst.calnr,*), $
  lijst.src(lijst.srcnr,*), $
  float(lshift)/100., float(airms)/100., float(airmc)/100., $
  lijst.imags(lijst.srcnr), $
  lijst.imagc(lijst.calnr))
  plot,newspec(0,*),newspec(1,*),/xsty,xra=[lijst.plotra(lijst.bandnr,0),lijst.plotra(lijst.bandnr,1)]

return,newspec

end


;==============================================================

function leescalwave,band

case band of
	'N':	begin
  		wave=lezen('./wavelength_tabel_N.tab')
		wavelength = fltarr(n_elements(wave(1,*)))
		wavelength(*) = wave(1,*)
		end
	'Q':	begin
  		wave=lezen('./wavelength_tabel_Q.tab')
		wavelength = fltarr(n_elements(wave(1,*)))
		wavelength(*) = wave(1,*)
		end
endcase

return,wavelength
end

;==============================================================

function shiftair,wcali,cal,src,xshift,airm1,airm2,nrims,nrimc

common tjip,lijst

flux = lijst.fluxc(lijst.calnr)

waar = where(wcali gt 0,ct)
wcal = float(wcali(waar))

wmax = min([n_elements(wcal),n_elements(cal),n_elements(src)]-1)

;wavelength determination assuming source spectrum is right

specsrc = fltarr(2,n_elements(src))
specsrc(0,0:wmax) = wcal(0:wmax)
speccal = fltarr(2,n_elements(cal))
speccal(0,*) = interpol(wcal,indgen(n_elements(wcal)),float(indgen(n_elements(cal)))+xshift)

; making of the calibration file assuming a lambda^-2 spectrum

minwcal = wcal(0)
maxwcal = wcal(n_elements(wcal)-1)

wgrid  = wcal(0) + float(indgen(3000))*(maxwcal-minwcal)/2999.
oriflux = wgrid^(-2)*(1./((1./wgrid(0))-(1./wgrid(2999))))
orical = (wgrid(2999)-wgrid(0))*flux*oriflux

; make grid nice to zero at the ends

extracal = 0d0*fltarr(n_elements(cal)+6)
extraspec = fltarr(n_elements(speccal(0,*))+6)
extraspec(3:n_elements(speccal(0,*))+2) = float(speccal(0,*))
extracal(3:n_elements(cal)+2) = float(cal)
extraspec(0) = 0
extraspec(1) = 0.95*minwcal
extraspec(2) = 0.99*minwcal
extraspec(n_elements(speccal(0,*))+3) = 1.01*maxwcal
extraspec(n_elements(speccal(0,*))+4) = 1.05*maxwcal
extraspec(n_elements(speccal(0,*))+5) = 10000.

; calculate sensitivity

fluxgrid = interpol(extracal,extraspec,float(wgrid))
waar = where(fluxgrid lt 0.02*max(fluxgrid),ct)
if (ct gt 0) then fluxgrid(waar) = 0.02*max(fluxgrid)
sensgrid = float(orical)/float(fluxgrid)

revsens = sensgrid^(airm1/airm2)

cali = interpol(revsens,wgrid,specsrc(0,*))
waar = where(cali lt 0.001*max(revsens), ct)
if (ct gt 0) then cali(waar) = 0.001*max(revsens)
waar = where(src lt 0.0, ct)
if (ct gt 0) then src(waar) = 0.00
specsrc(1,*) = src*cali*nrimc/nrims

return,specsrc

end

;==============================================================

function reduceren,filenaam

plaatje = readfits(filenaam)
spec = addingspec(plaatje,/spec,/noshow)

return,spec
end

;==============================================================

FUNCTION specduc, source, calibration

common tjip, lijst

default,source,'input_source'
default,calibration,'input_calibration'

close,1
close,2

openr, 1, source
openr, 2, calibration

a = fix(0)
readf,1,a

names = strarr(a)
namss = strarr(a)
imags = intarr(a)
airms = fltarr(a)
woord = string('')
sternaam = string('')
getal = fix(0)
dsrcspec = fltarr(a,350) 
wmax=0
for ii=0,a-1 do begin
  readf,1,sternaam
  namss(ii)=sternaam
  readf,1,woord
  names(ii) = woord
  redsourcespec = reduceren(woord)
  dsrcspec(ii,0:(n_elements(redsourcespec)-1)) = redsourcespec
  if (n_elements(redsourcespec) gt wmax) then wmax=n_elements(redsourcespec)
  readf,1,getal1
  imags(ii) = getal1
  readf,1,zomaar
  airms(ii) = zomaar
endfor
close,1
srcspec = fltarr(a,wmax)
srcspec(*,*) = dsrcspec(*,0:wmax-1)

b = fix(0)
readf,2,b

namec = strarr(b)
namsc = strarr(b)
imagc = intarr(b)
airmc = fltarr(b)
fluxc = fltarr(b)
calspec = fltarr(b,350) 
for jj=0,b-1 do begin
  readf,2,sternaam
  namsc(jj)=sternaam
  readf,2,woord
  namec(jj)=woord
  calibrationspec = reduceren(woord)
  calspec(jj,0:(n_elements(calibrationspec)-1)) = calibrationspec
  readf,2,getal
  imagc(jj)=getal
  readf,2,zomaar1
  airmc(jj)=zomaar1
  readf,2,zomaar2
  fluxc(jj)=zomaar2
endfor
close,2


base = widget_base(title='Telluric', column=2)
base1 = widget_base(base,column=1)
tjop1 = widget_slider(base1,min=-5d2,max=5d2,title='pixels * 100',xsize=480,value=0)
tjop2 = widget_slider(base1,min=100,max=300,title='airmass*100 observed star',xsize=480,value=airms(0)*100)
tjop3 = widget_slider(base1,min=100,max=300,title='airmass*100 standard star',xsize=480,value=airmc(0)*100)
tjop4 = widget_droplist(base1,/dynamic_resize,title='source star',value=namss)
tjop5 = widget_droplist(base1,/dynamic_resize,title='calibration star',value=namsc)
tjop6 = widget_droplist(base1,/dynamic_resize,title='N or Q band',value=['N','Q'])

waves = fltarr(wmax)
fluxs = fltarr(wmax)
wavec = float(indgen(1000)*0)
lshift=0
test=0
plotra=fltarr(2,2)
plotra(0,*)=[8.5,11.7]
plotra(1,*)=[15.,23.5]

waveom = fltarr(2,320)
waveom(0,*) = leescalwave('N')
waveom(1,*) = leescalwave('Q')

store ={band:['N','Q'],	$
	src:srcspec,	$
	cal:calspec, 	$
	names:names,	$ 
	namec:namec,	$
	namss:namss,	$ 
	namsc:namsc,	$
	airms:airms,	$
	airmc:airmc,	$
	imags:imags,	$
	imagc:imagc,	$
	first:fltarr(3),$ 
	shiftid:tjop1,	$
	airmsid:tjop2,	$
	airmcid:tjop3,  $
	srcid:tjop4,	$
	calid:tjop5,	$
	bandid:tjop6,	$
	srcnr:0,	$
	calnr:0,	$
	bandnr:0,	$
        drawid:0,	$
	lshift:lshift,	$
	waveom:waveom,	$
	fluxc:fluxc,	$
	wavec:wavec,	$
	fluxs:fluxs,	$
	waves:waves,	$
	plotra:plotra}

lijst = store

base2 = widget_base(base,column=1)
tjopa = widget_draw(base2,xsize=500,ysize=500,/expose_events, $
	  /retain,/button_events, uvalue={type:'draw'})
lijst.drawid = tjopa

Widget_Control, tjopa, Get_uvalue=win
widget_control,tjop1,set_uvalue={type:'reken', store:store}
widget_control,tjop2,set_uvalue={type:'reken', store:store}
widget_control,tjop3,set_uvalue={type:'reken', store:store}
widget_control,tjop4,set_uvalue={type:'test', store:store}
widget_control,tjop5,set_uvalue={type:'test', store:store}
widget_control,tjop6,set_uvalue={type:'test', store:store}

dummy1 = widget_button(base1,value='Save spectrum',uvalue={type:'write'})
dummy2 = widget_button(base1,value='kill',uvalue={type:'quit'})


widget_control, base, /realize

;Widget_Control, tjopa, set_uvalue={type:'draw', store:store}


xmanager, 'Fit', base

;event = widget_event(base)

result=fltarr(2,wmax)
result(0,*) = lijst.waves
result(1,*) = lijst.fluxs
return, result

end
;--------------------------
