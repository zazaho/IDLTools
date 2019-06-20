;+
;SH_LINE_SIMPLE: simple program to interactively measure line properties in a spectrum.
;SH_LINE_SIMPLE usage:
;   sh_line_simple,spectrum [,option=option]
;   spectrum     -  [2,*] array which hold wavelength (mum) and flux (jy)
;SH_LINE_SIMPLE options:
; XRANGE=xrange range to use
; CONT=cont array like spectrum that holds or on return will hold the continuum
; /HELP  if set show this help
; /QUIET - if set be more quiet in the console
; ------------------------------------------------------
; DEPENDS on pl.pro
;-
;(SH Jul  9 2009) modified to take a simple array transpose([wave[mum],flux[jy]])
;  as input
;(SH Jan  5 1999)
  
function sh_line_simple,in, $
                        XRANGE=xrange, $
                        CONT=cont, $
                        HELP=help, $
                        QUIET=quiet
  
  if n_elements(in) eq 0 then begin
     doc_library,'sh_line_simple'
  endif

  if keyword_set(help) then begin
     doc_library,'sh_line_simple'
  endif

  oldquiet = !quiet
  if keyword_set(quiet) then !quiet = 1
  
  c = 2.99792458d14
  dataline = {gmaxx:0d0,gmaxy:0d0,gwidth:0d0,gflux:0d0,flux:0d0,meancont:0d0}
  
  wave = reform(in[0,*])
  flux=  reform(in[1,*])
  
  ;; convert flux [Jy] to flambda [W/m2/mum]
  flux = flux * c / wave / wave * 1d-26

  if keyword_set(xrange) then begin
     idx = where((wave ge xrange[0]) and (wave le xrange[1]))
     wave = wave[idx]
     flux = flux[idx]
  endif
  
  spectrum = transpose([[wave],[flux]])

  if (n_elements(cont) eq 0) then begin

; plot the data end let user indicate the continuum
     xc =fltarr(20)
     yc =fltarr(20)
     !err = 0
     i = 0

     pl,spectrum,ps=0,xtit='wavelength [micron]',ytit='flux density [W/m2/micron]'
    
     print,'Please indicate the continuum with the left button, stop=right button'
     Cursor,a,b,2,/down
     xc[i] = a
     yc[i] = b
     while (!err ne 4) AND (i lt 19)  DO begin
        pl,a,b,ps=1,thick=4,/white,/opl
        !err = 0
        message,/info,string(xc[i],yc[i])
        i = i + 1
        cursor,a,b,2,/down
        xc[i] = a
        yc[i] = b
     EndWhile
     
     case i of
        0: begin
           ;no continuum
           flux_cont = 0d0*wave
        end
        1: begin
           ;constant continuum
           flux_cont = yc[0]*wave
        end
        2: begin
           ;linear continuum
           flux_cont = (wave-xc[0])*(yc[1]-yc[0])/(xc[1]-xc[0])+yc[0]
        end
        else: begin
           flux_cont = spline(xc[0:i-1],yc[0:i-1],wave)
        end
     endcase

     cont = transpose([[wave],[flux_cont]])

  endif
  
  ;; hold the selected range
  x2range = [0.0,0.0]
  
jump2: 
  pl,spectrum,ps=0,xtit='wavelength [micron]',ytit='flux density [W/m2/micron]'
  pl,cont,/oplot,ps=0

jump1:
  print, 'Now select the part you want to use for the line flux with the mouse...'
  print, 'Increasing wavelength selects, decreasing wavelength zooms'

  wait,0.5
  cursor,x,y
  message,/info,string(x,y)
  pl,x,y,ps=1,thick=4,/white,/opl
  x2range[0] = x

  wait,0.5
  cursor,x,y
  message,/info,string(x,y)
  pl,x,y,ps=1,thick=4,/white,/opl
  x2range[1] = x

  if (x2range[0] ge x2range[1]) then begin
     message,/info,'Decreasing order --> Zooming: '
     pl,spectrum,xr=[x2range[1],x2range[0]],xtit='wavelength [micron]',ytit='flux density [W/m2/micron]'
     pl,cont,/oplot
     goto, jump1
  endif
  
  idx_line = where((wave ge x2range[0]) and (wave le x2range[1]))

  ;; extract the region of interest
  wave_around_line        = reform(wave     [idx_line])
  flux_around_line        = reform(flux     [idx_line])
  flux_cont_around_line   = reform(flux_cont[idx_line])
  flux_line_around_line   = flux_around_line-flux_cont_around_line
  
  spectrum_around_line = transpose([[wave_around_line],[flux_around_line]])
  line_cont            = transpose([[wave_around_line],[flux_cont_around_line]])
  spectrum_line        = transpose([[wave_around_line],[flux_line_around_line]])

  ;; help gaussfit to avoid problems with small numbers
  tricky_scaling_factor=1d2/max(flux_line_around_line)

  fitted_flux_around_line = (GAUSSFIT( wave_around_line, flux_line_around_line*tricky_scaling_factor, A,nterms=3))/tricky_scaling_factor
  A[0] = A[0]/tricky_scaling_factor
  fitted_line = transpose([[wave_around_line],[fitted_flux_around_line]])
  
;(SH Jan  5 1999) taken from getlineflux.pro
  npoints=n_elements(wave_around_line)
  intval=fltarr(npoints)
  intval[1:npoints-2]=0.5*(wave_around_line[2:npoints-1]-wave_around_line[0:npoints-3])
  intval[0]=0.25*(wave_around_line[1]-wave_around_line[0])
  intval[npoints-1]=0.25*(wave_around_line[npoints-1]-wave_around_line[npoints-2])

  totalflux=total(flux_line_around_line*intval)

  mcont = mean(flux_cont_around_line)
  
  print,'---> gauss-fit: Y = h * exp( -( x - lc )^2 / b ) ' 
  print,'---> centrale wavelength (lc): ',A[1],' micron'
  print,'---> height (h): ',A[0],' W/m2/mum'
  print,'---> FWHM (b): ',2.35482*A[2],' micron'
  print,'---> gaussian line flux: ', A[2]*A[0]*2.5066283,' W/m2'
  print,'---> Integrated line flux: ',totalflux,' W/m2'
  print,'---> Mean under the line continuum: ',mcont,' W/m2'
  
  pl,spectrum_line,ps=0,xtit='wavelength [micron]',ytit='flux density [W/m2/micron]'
  pl,fitted_line,/oplot,psym=-1,thi=3
  print,'Click below zero Jansky to store this measurement'
  wait,0.5
  cursor,x,y
  
;(SH Jan  5 1999)
  if y lt 0 then begin
    if n_elements(measurement_data) eq 0 then begin
      measurement_data = dataline
    endif else begin
      measurement_data = [measurement_data,dataline]
    endelse
    datapointer = n_elements(measurement_data)-1
    measurement_data(datapointer).gmaxx = A[1] 
    measurement_data(datapointer).gmaxy = A[0]
    measurement_data(datapointer).gwidth = A[2]
    measurement_data(datapointer).gflux = A[2]*A[0]*2.5066283
    measurement_data(datapointer).flux = totalflux
    measurement_data(datapointer).meancont = mcont
  endif
    
  print,'click in the plot to measure another line, below to stop.' 
  wait,0.5
  cursor,x,y
  if (y gt 0) then begin
     goto, jump2
  endif
  
  !quiet = oldquiet

  if n_elements(measurement_data) ne 0 then begin
     return,measurement_data
  endif else begin
     return,-1
  endelse

end
