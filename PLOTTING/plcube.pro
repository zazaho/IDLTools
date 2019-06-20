;+
;PLCUBE: simple program to plot the spectrum of a spectral cube
;PLCUBE usage:
;   pl,cube [,wave]
;PLCUBE options:
;   x=x          -  show the summed spectrum of pixels with values x
;   y=y          -  show the summed spectrum of pixels with values y
;   box=b        -  show the summed spectrum of pixels values around
;                  x,y inside a box of side b
;   help         -  prints this message
;   nonorm       -  do not normalise the resulting spectrum by the
;                  number of valid (non nan) pixels
;-
pro plcube,cube_in,wave_in, $
           x=x_in, $
           y=y_in, $
           box=box, $
           help=help, $ $
           nonorm=nonorm, $
           _extra=_extra
  
  if keyword_set(help) then begin
     doc_library,'plcube'
     exit
  endif

  if n_elements(cube_in) eq 0 then begin
     message,/info,'A cube as input is needed'
     doc_library,'plcube'
     return
  endif

  cube = cube_in
  
  if n_elements(wave_in) ne 0 then begin
     wave = wave_in
  endif
  
  sizecube = size(cube)

  ;; allow a structure to be given
  if (sizecube[0] eq 1) and (sizecube[2] eq 8) then begin
     wave = cube.wave
     cube = cube.cube
     sizecube = size(cube)
  endif

  if sizecube[0] ne 3 then begin
     message,/info,'A cube as input is needed'
     doc_library,'plcube'
     return
  endif
  
  nx = long(sizecube[1])
  ny = long(sizecube[2])
  nw = long(sizecube[3])
  
  if n_elements(wave) eq 0 then begin
     w = dindgen(nw)
  endif else begin
     w = double(wave)
  endelse

  case 1 of
     (n_elements(x_in) eq 0) and (n_elements(y_in) eq 0): begin
        ;; if nothing is given take everything
        x = lindgen(nx)# (make_array(ny,value=1d0))
        y = lindgen(ny)##(make_array(nx,value=1d0))
     end
     (n_elements(x_in) eq 0) and (n_elements(y_in) ne 0): begin
        ;; take a full row at y = y[0]
        x = lindgen(nx)
        y = y_in[0]*make_array(nx,value=1d0)
     end
     (n_elements(x_in) ne 0) and (n_elements(y_in) eq 0): begin
        ;; take a full column at x = x[0]
        y = lindgen(ny)
        x = x_in[0]*make_array(nx,value=1d0)
     end
     (n_elements(y_in) gt 1) and (n_elements(x_in) eq 1): begin
        x = x_in + make_array(n_elements(y_in),value=0)
        y = y_in
     end
     (n_elements(y_in) eq 1) and (n_elements(x_in) gt 1): begin
        y = y_in + make_array(n_elements(x_in),value=0)
        x = x_in
     end
     else: begin
        ;; both x and y are given take the common number of elements
        minlen = min([n_elements(x_in),n_elements(y_in)])
        x = x_in[0:minlen-1]
        y = y_in[0:minlen-1]
     end
  endcase

  if (n_elements(box) ne 0) and (n_elements(x) eq 1) and (n_elements(y) eq 1) then begin
     x = x[0] + ((dindgen(box)-(box-1d0)/2d0)# (make_array(box,val=1)))
     y = y[0] + ((dindgen(box)-(box-1d0)/2d0)##(make_array(box,val=1)))
  endif
  
  x = reform(x,n_elements(x))
  y = reform(y,n_elements(y))
  x = long(round(x))
  y = long(round(y))
  
  ;; verify the values of x and y
  idx = where( (x ge 0L) and (x lt nx) and (y ge 0) and (y lt ny),cnt)
  if cnt eq 0 then begin
     message,/info,'the given x and y values are not inside the image'
     message,/info,'using the central pixel'
     x = long(round(nx/2d0))
     y = long(round(ny/2d0))
  endif

  ;; we have to reform the cube to make 1 selection criterion rather
  ;; than one on x and one on 1 which takes too much memory and time 
  
  cubeflat = reform(cube,nx*ny,nw)

  xxx = lindgen(nx)# (make_array(ny,value=1L))
  yyy = lindgen(ny)##(make_array(nx,value=1L))
  xxx = reform(xxx,n_elements(xxx))
  yyy = reform(yyy,n_elements(yyy))
  idx = -1
  cnt = 0
  for i=0,n_elements(x)-1 do begin
     idx1 = where((xxx eq x[i]) and (yyy eq y[i]),cnt1)
     if cnt1 ne 0 then begin
        idx = [idx,idx1]
        cnt = cnt+cnt1
     endif
  endfor

  if cnt ne 0 then begin
     idx = idx[1:*]

     spectrum = total(cubeflat[idx,*],1,/nan)
     
     if not keyword_set(nonorm) then begin
        normfac = total(finite(cubeflat[idx,*]),1,/nan)
        spectrum=spectrum/normfac
     endif
     
     pl,w,spectrum,_extra=_extra
  endif

end
