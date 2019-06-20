; sh_line
; determine line center and line fluxes first determine cont.

function sh_line,in,XRANGE=xrange,OLDDATA=olddata,CONT=cont, $
                 flambda=flambda,nocomment=nocomment

;(SH Jan  5 1999)
  dataline = {gmaxx:0d0,gmaxy:0d0,gwidth:0d0,gflux:0d0,gew:0d0,gewl:0d0, $
              maxx:0d0, maxy:0d0, flux:0d0,             ew:0d0, ewl:0d0, $
              meancont:0d0,cont:0d0,starcont:0d0, remark:'' }
  
  aar = in
  if n_elements(olddata) ne 0 then begin
    newdata = replicate(dataline,n_elements(olddata))
    for i = 0,n_elements(olddata)-1 do begin
      newdata(i).gmaxx = olddata(i).gmaxx
      newdata(i).gmaxy = olddata(i).gmaxy
      newdata(i).gwidth = olddata(i).gwidth
      newdata(i).gflux = olddata(i).gflux
      newdata(i).gew = olddata(i).gew
      newdata(i).gewl = olddata(i).gewl
      newdata(i).maxx = olddata(i).maxx
      newdata(i).maxy = olddata(i).maxy
      newdata(i).flux = olddata(i).flux
      newdata(i).ew = olddata(i).ew
      newdata(i).ewl = olddata(i).ewl
      newdata(i).meancont = olddata(i).meancont
      newdata(i).cont = olddata(i).cont
      newdata(i).starcont = olddata(i).starcont
      newdata(i).remark = olddata(i).remark
    endfor
  endif

  contx =fltarr(2)
  conty =fltarr(2)
  x2range = fltarr(2)
  y2range = fltarr(2)
  if keyword_set(xrange) then begin
    aar = sh_select( aar,(aar.data.wave ge xrange(0)) and $
                      (aar.data.wave le xrange(1)) ) 
  endif
  
  if n_elements(cont) ne 0 then oldcont=cont
  
  usefl = 0
  if keyword_set(flambda) then begin
    usefl = 1
    aar = sh_calcaar(aar,/flambda)
    if keyword_set(cont) then begin
      cont = sh_calcaar(cont,/fl)
    endif
  endif
  
  if (n_elements(cont) eq 0) then begin

; plot the data end let user indicate the continuum

    xc =fltarr(20)
    yc =fltarr(20)
    !err = 0
    i = 0

    pl,aar
    
    print,'Please indicate the continuum with the left button, stop=right button'
    Cursor,a,b,2,/down
    xc(i) = a
    yc(i) = b
    while (!err ne 4) AND (i lt 19)  DO begin
      !err = 0
      print,xc(i),yc(i)
      i = i + 1
      cursor,a,b,2,/down
      xc(i) = a
      yc(i) = b
    EndWhile

    xcont = aar.data.wave
    ycont = spline(xc(0:i-1),yc(0:i-1),xcont)

    cont = aar
    cont.data.flux = ycont
;plotaar,cont,/oplot,psym=0
;;    write_fitstable,cont,'lineint3_cont.aar'
  endif

jump2: 
  pl,aar,ps=0
  pl,cont,/oplot,ps=0

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
    pl,aar,xr=[x2range(1),x2range(0)]
    pl,cont,/oplot
    goto, jump1
  endif

  aarline=sh_select(aar,aar.data.wave ge x2range(0) and aar.data.wave le x2range(1))

  contx(0) = x2range(0)
  conty(0) = y2range(0)
  contx(1) = x2range(1)
  conty(1) = y2range(1)

;get dimensions and wavelengths from line-aar
  aarcont = aarline

;make interpolation for continuum
  fluxstep = (conty(1)-conty(0))/n_elements(aarcont.data)
  for ik=1,n_elements(aarcont.data)-2 do begin
    aarcont.data(ik).flux = conty(0) + ik*fluxstep
  endfor

;substract underlying cont.
  aarline.data.flux = aarline.data.flux - aarcont.data.flux
  pl,aarline,ps=0

;get dimensions and wavelengths from line-aar 
  aarfit = aarline
  flx=aarline.data.flux
  wv =aarline.data.wave

  if (usefl eq 0) then begin
    aarfit.data.flux = GAUSSFIT( wv, flx, A,nterms=3)
  endif else begin
    aarfit.data.flux = 1e-14*GAUSSFIT(wv,flx*1e14,A,nterms=3)
    A(0) = 1e-14*A(0)
  endelse
  
  pl,aarfit,/oplot,psym=1,thi=3

; now calculate the line properties  
  maxflux = max(flx,maxindex)
  maxwave = wv(maxindex)
  
;(SH Jan  5 1999) gejat van getlineflux.pro
  npoints=n_elements(flx)
  intval=fltarr(npoints)
  intval(1:npoints-2)=0.5*(wv(2:npoints-1)-wv(0:npoints-3))
  intval(0)=0.25*(wv(1)-wv(0))
  intval(npoints-1)=0.25*(wv(npoints-1)-wv(npoints-2))
  totalflux=total(flx*intval)

;  binsize = ( contx(1) - contx(0) ) / ( n_elements(foo) - 1 )
;  totalflux = total(foo)*binsize
  K_atm = km(maxwave(0),print='no')
;(SH Jan  5 1999)
  if (usefl eq 0) then begin
    mcont = mean(aarcont.data.flux)
  endif else begin
    mcont = 1e-14*mean(aarcont.data.flux*1e14)
  endelse
;  mcont = moment(aarcont.data.flux)
;  mcont = mcont(0)

  print,'---> gauss-fit: Y = h * exp( -( x - lc )^2 / b ) ' 
  print,'---> centrale golflengte (lc): ',A(1),' micron
  print,'---> hoogte (h): ',A(0),' Jansky'
  print,'---> breedte (b): ',2.35482*A(2),' micron'
;oppervlak onder gauss = breedte * hoogte * sqrt(2*pi)
  if (usefl eq 0) then begin
    print,'---> gaussische lijn flux: ', $
      A(2)*A(0)*2.5066283*2.99e-12/A(1)^2,' W/m2'
  endif else begin
    print,'---> gaussische lijn flux: ', $
      A(2)*A(0)*2.5066283,' W/m2'
  endelse
  
;FWHM = A(2)*2*sqrt{2*ln(2)}  = A(2)*2.35482   
  print,'---> FWHM gauss: ',A(2)*2.35482
  print,'---> Resolving Power (=l/Dl): ',A(1)/(A(2)*2.35482) 
  print,'---> data punten:' 
  print,'---> golflengte maximum : ',maxwave(0),' micron'
  print,'---> hoogte : ',maxflux,' Jansky'
  if (usefl eq 0) then begin
    print,'---> lijn flux: ',totalflux*3e-12/A(1)^2,' W/m2'
  endif else begin
    print,'---> lijn flux: ',totalflux,' W/m2'
  endelse
  print
  print,'---> gemiddelde continuum flux: ',mcont,' Jansky'
  print,'---> Flux bij centrale golflengte: ',aarcont.data(maxindex).flux,' Jansky'
  print,'---> flux over continuum: ',maxflux/aarcont.data(maxindex).flux
  print,'---> Equivalente breedte gauss: ',A(2)*A(0)*2.5066283/mcont*1e4,' Angstrom'
  print,'---> Equivalente breedte data punten: ',totalflux/mcont*1e4,' Angstrom'
  print,'---> EW/l gauss: ',A(2)*A(0)*2.5066283/mcont/A(1)
  print,'---> EW/l data: ',totalflux/mcont/maxwave(0)
  print,'---> Cont/Ster-cont : ',mcont/K_atm

  print,'Click below zero Jansky to store this measurement'

  wait,0.5
  cursor,x,y
  
;(SH Jan  5 1999)
  if y lt 0 then begin
    if n_elements(newdata) eq 0 then begin
      newdata = dataline
    endif else begin
      newdata = [newdata,dataline]
    endelse
    datapointer = n_elements(newdata)-1
    newdata(datapointer).gmaxx = A(1) 
    newdata(datapointer).gmaxy = A(0)
    newdata(datapointer).gwidth = A(2)
    if (usefl eq 0) then begin
      newdata(datapointer).gflux = A(2)*A(0)*2.5066283*2.99d-12/(A(1)*A(1))  
    endif else begin
      newdata(datapointer).gflux = A(2)*A(0)*2.5066283
    endelse
    newdata(datapointer).maxx = maxwave(0)
    newdata(datapointer).maxy = maxflux(0)
    if (usefl eq 0) then begin
      newdata(datapointer).flux = totalflux*2.99d-12/(A(1)*A(1))
    endif else begin
      newdata(datapointer).flux = totalflux
    endelse
    newdata(datapointer).starcont = K_atm
    newdata(datapointer).meancont = mcont
    newdata(datapointer).cont = aarcont.data(maxindex).flux
    newdata(datapointer).gew = A(2)*A(0)*2.5066283/mcont*1e4  
    newdata(datapointer).ew = totalflux/mcont*1e4
    newdata(datapointer).gewl = A(2)*A(0)*2.5066283/K_atm/A(1)
    newdata(datapointer).ewl = totalflux/K_atm/maxwave(0)
;(SH Jan  6 1999)
    A = STRING(0)
    if not keyword_set(nocomment) then begin
       print,'remark for this line:'
       READ,A
;    A = dialog(/STRING,value='','enter a remark')
       newdata(datapointer).remark = A
    endif
    save,newdata,file='lines_lineint3_tmp.dat'
  endif
    
  print,'click in the plot to measure another line, below to stop.' 
  wait,0.5
  cursor,x,y
  if (y gt 0) then begin
    goto, jump2
  endif

  if (usefl ne 0)  then cont = sh_calcaar(cont,fl=-1)
  if (n_elements(oldcont) ne 0) then cont = oldcont
    
  if n_elements(newdata) ne 0 then return,newdata

end
