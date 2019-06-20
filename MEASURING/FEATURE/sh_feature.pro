; sh_feature
; determine feature properties
; no fitting just use the mouse to indicate fwhm and peak etc.

; Should do the following:
; fit polynomial continuum to selected parts
; ask user
; subtract this continuum
; use mouse to indicate begin-end of roi
; determine integrated flux
; ask peakpos+heigth
; plot hm
; ask left right hm positons
; 
; should save:
; poly order +params max 3 a+bx+cxx
; flux range
; flux
; peak height plus pos
; left right hm
; fwhm
; flux assym = left-peak/peak-right
; line assym = lefthwhm/righthwhm

function sh_feature,in,XRANGE=xrange,OLDDATA=olddata,CONT=cont, $
                    ZOOM=zoom

;(SH Jan  5 1999)
  dataline = {ft,corder:0,cparam0:0d0,cparam1:0d0,cparam2:0d0, $
              xmin:0d0,xmax:0d0,flux:0d0,xpeak:0d0,ypeak:0d0, $
              hleft:0d0,hright:0d0,fwhm:0d0,fleft:0d0,fright:0d0}
  a = in
  
  if ( n_elements(cont) gt 0 ) then begin
    c = cont
    cset = 1
    corder = -1
    fit = [0d0,0d0,0d0]
  endif else begin
    cset = 0
    c = in
    c.data.flux = 0d0
  endelse
  
;(SH Feb  4 1999)
;Use factor of 1e14 to avoid float problems
  
  a = sh_calcaar(a,/fl,fact=1e14)
  c = sh_calcaar(c,/fl,fact=1e14)
  
  if n_elements(olddata) ne 0 then begin
    newdata = olddata
;    newdata = replicate(dataline,n_elements(olddata))
;    for i = 0,n_elements(olddata)-1 do begin
;      newdata(i).gmaxx = olddata(i).gmaxx
;      newdata(i).gmaxy = olddata(i).gmaxy
;      newdata(i).gwidth = olddata(i).gwidth
;      newdata(i).gflux = olddata(i).gflux
;      newdata(i).gew = olddata(i).gew
;      newdata(i).gewl = olddata(i).gewl
;      newdata(i).maxx = olddata(i).maxx
;      newdata(i).maxy = olddata(i).maxy
;      newdata(i).flux = olddata(i).flux
;      newdata(i).ew = olddata(i).ew
;      newdata(i).ewl = olddata(i).ewl
;      newdata(i).meancont = olddata(i).meancont
;      newdata(i).cont = olddata(i).cont
;      newdata(i).starcont = olddata(i).starcont
;      newdata(i).remark = olddata(i).remark
;    endfor
  endif
  
  ; Take only the part of interest
  if keyword_set(xrange) then begin
    a = sh_select( a,(a.data.wave ge xrange(0)) and $
                      (a.data.wave le xrange(1)) ) 
    c = sh_select( c,(c.data.wave ge xrange(0)) and $
                      (c.data.wave le xrange(1)) ) 
  endif
  
  ain = a
  cin = c
  
start:
  if keyword_set(zoom) then begin
    pl,ytit='Flux density[W/m/m/micron]*1e14',ain
zoom:    
    print, 'Select the waverange, long to short to zoom.'
    veri,x1,/nostatus
    wait,0.2
    veri,x2,/nostatus
    wait,0.2
    if x1 gt x2 then begin
      print,'Zooming: '
      pl,ytit='Flux density[W/m/m/micron]*1e14',ain,xr=[x2,x1]
      goto,zoom
    endif
    a = sh_select(ain,(ain.data.wave ge x1) and $
               (ain.data.wave le x2))
    c = sh_select(cin,(cin.data.wave ge x1) and $
               (cin.data.wave le x2))
  endif

  if not (cset) then begin
continuum:
  
    xc =fltarr(100)
    yc =fltarr(100)
    i = 0
    corder = 0
    !err = 0
    
; plot the data end let user indicate the continuum
    pl,a,ytit='Flux density[W/m/m/micron]*1e14'
    print,'Please indicate the continuum with the left button,', $
      'stop=right button'
    while (!err ne 4) AND (i lt 99)  DO begin
      cursor,x,y,2,/down
      if (!err ne 4) then begin
        xc(i) = x
        yc(i) = y
        print,xc(i),yc(i)
        oplot,ps=4,[xc(i)],[yc(i)],col=160
        i = i + 1
      endif
    EndWhile
    
polynome:
    Print,'Which order of polynome shall I fit (0/1/2)?'
    Read,corder
    
    fit = poly_fit(xc(0:i-1),yc(0:i-1),corder)
    case corder of
      0: fit = [fit(0),0d0,0d0]
      1: fit = [fit(0),fit(1),0d0]
      2: fit = [fit(0),fit(1),fit(2)]
      else: begin
        fit=[0,0,0,0]
        corder = -1
        print,'warning no continuum selected'
      end
    endcase
    wv = c.data.wave
    c.data.flux = fit(0)+fit(1)*wv+fit(2)*wv^2
    pl,c,/o
    answer = 'a'
    print,'What next: accept,change order, restart (a/o/r)?'
    read,answer
    case 1 of
      (answer eq 'r') OR (answer eq 'R'): goto,continuum
      (answer eq 'o') OR (answer eq 'O'): goto,polynome
      else :
    endcase

;   write_fitstable,cont,'lineint3_cont.aar'
  endif
; return,0
 
; We have a continuum: subtract it and plot
 
 f = sh_calcaar(a,subt=c)

  pl,ytit='Flux density[W/m/m/micron]*1e14',f
  
feature:
  print, 'Now select the waverange for the feature. Long to short to zoom'
  veri,xmin,/nostatus
  wait,0.2
  print,xmin
  
  veri,xmax,/nostatus
  wait,0.2
  print,xmax

  if xmin gt xmax then begin
    print,'you chose first the longs- and next the short wavelength limit'
    print,'Zooming: '
    pl,ytit='Flux density[W/m/m/micron]*1e14',f,xr=[xmax,xmin]
    goto, feature
  endif

  ff = sh_select(f,f.data.wave ge xmin and f.data.wave le xmax)
;get dimensions and wavelengths from ff
  flx=ff.data.flux
  wv =ff.data.wave
  npoints=n_elements(flx)
  intval=fltarr(npoints)
    
  polyfill,wv,flx,color=50
  wait,3
  
  pl,ff,ytit='Flux density[W/m/m/micron]*1e14',ps=0
  print,'Indicate the peak position'
  cursor,xpeak,ypeak,2,/down
  print,xpeak,ypeak
  oplot,ps=4,[xpeak],[ypeak],col=160
  oplot,[0.,200.],[ypeak/2.,ypeak/2.],col=160
  print,'Indicate the position of the left and right Half-Max'
  cursor,xhleft,yhleft,2,/down
  oplot,ps=4,[xhleft],[yhleft],col=100
  cursor,xhright,yhright,2,/down
  oplot,ps=4,[xhright],[yhright],col=100
  if (xhleft gt xhright) then begin
    foo = xhleft
    xhleft = xhright
    xhright = foo
  endif
  
; now calculate the line properties  
;(SH Jan  5 1999) gejat van getlineflux.pro
  intval(1:npoints-2)=0.5*(wv(2:npoints-1)-wv(0:npoints-3))
  intval(0)=0.25*(wv(1)-wv(0))
  intval(npoints-1)=0.25*(wv(npoints-1)-wv(npoints-2))
  totalflux=total(flx*intval)


  print,'Integrated Flux',totalflux*1e-14,' W/m/m'
  print,'peak:',ypeak,' W/m/m/mu at ',xpeak,' mu'
  print,'FWHM:',xhright-xhleft, ' mu, from ',xhleft,' to ',xhright,' mu'
  print,'Click below the abissa to store this measurement'
  
  wait,0.2
  cursor,x,y
  
;(SH Jan  5 1999)
  if y lt 0 then begin
; Get the left and right flux
    lf = sh_select(ff,ff.data.wave le xpeak)
    flx=lf.data.flux
    wv =lf.data.wave
    npoints=n_elements(flx)
    intval=fltarr(npoints)
    intval(1:npoints-2)=0.5*(wv(2:npoints-1)-wv(0:npoints-3))
    intval(0)=0.25*(wv(1)-wv(0))
    intval(npoints-1)=0.25*(wv(npoints-1)-wv(npoints-2))
    leftflux=total(flx*intval)
    
    rf = sh_select(ff,ff.data.wave ge xpeak)
    flx=rf.data.flux
    wv =rf.data.wave
    npoints=n_elements(flx)
    intval=fltarr(npoints)
    intval(1:npoints-2)=0.5*(wv(2:npoints-1)-wv(0:npoints-3))
    intval(0)=0.25*(wv(1)-wv(0))
    intval(npoints-1)=0.25*(wv(npoints-1)-wv(npoints-2))
    rightflux=total(flx*intval)

    if n_elements(newdata) eq 0 then begin
      newdata = dataline
    endif else begin
      newdata = [newdata,dataline]
    endelse
    datapointer = n_elements(newdata)-1
    newdata(datapointer).corder		= corder
    newdata(datapointer).cparam0	= fit(0)*1e-14
    newdata(datapointer).cparam1	= fit(1)*1e-14
    newdata(datapointer).cparam2	= fit(2)*1e14
    newdata(datapointer).xmin		= xmin
    newdata(datapointer).xmax		= xmax
    newdata(datapointer).flux		= totalflux*1e-14
    newdata(datapointer).xpeak		= xpeak
    newdata(datapointer).ypeak		= ypeak*1e-14
    newdata(datapointer).hleft		= xpeak-xhleft
    newdata(datapointer).hright		= xhright-xpeak
    newdata(datapointer).fwhm		= xhright - xhleft
    newdata(datapointer).fleft		= leftflux*1e-14
    newdata(datapointer).fright         = rightflux*1e-14
    save,newdata,file='sh_feature_tmp.dat'
  endif
    
  print,'click in the plot to measure another line, below to stop.' 
  wait,0.5
  cursor,x,y
  if (y gt 0) then begin
    goto, start
  endif

  c = sh_calcaar(c,fl=-1)
  if not (cset) then cont = c
    
  if n_elements(newdata) ne 0 then return,newdata

end
