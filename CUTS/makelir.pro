;; routine to make LIR images from spitzer maps convolved  regridded
;; to the same pixel scheme

;; values are given in MJy/sr
FUNCTION makeLIR,incube,inwaves,fluxes=fluxes

  nx = n_elements(incube[*,0,0])
  ny = n_elements(incube[0,*,0])

  ;; add two points at 1 and 500 with values 0d0
  cube = [[[make_array(nx,ny,val=0d0)]],[[incube]],[[make_array(nx,ny,val=0d0)]]]
  waves = [1d0,inwaves,500d0]
  
  ;; loop over each pixel
  ;; i know it is a loop but it is much easier and efficient than
  ;; making cube cubes of everything
  lir = make_array(nx,ny,value=!values.d_nan)

  for x=0,nx-1 do begin
     for y=0,ny-1 do begin
        flux = reform(cube[x,y,*])
        idx = where(finite(flux),cnt)
        if cnt ne 0 then begin
           lir[x,y] = sh_integrate(transpose([[waves[idx]],[flux[idx]]]),/quiet,/noplot,xr=[4.,2d3])
        endif
     endfor
  endfor

  return,lir
END
