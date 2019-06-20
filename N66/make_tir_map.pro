function make_tir_map,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,wave=wave

  np = n_params()
  if np eq 0 then begin
     message,/info,'no maps given'
     return,!values.d_nan
  endif
  
  if n_elements(wave) ne np then begin
     message,/info,'number of wavelengths does not match the number of given maps'
     return,!values.d_nan
  endif

  tir = p1*0d0
  nx = n_elements(p0[*,0])
  ny = n_elements(p0[0,*])
  
  command = 'cube=[[[p0]]'+strjoin(',[[p'+n2s(indgen(np-1)+1)+']]')+']'
  foo = execute(command)

  for xx=0,nx-1 do begin
     for yy=0,ny-1 do begin
        flux = reform(cube[xx,yy,*])
        idx = where(finite(flux),cnt)
        if cnt gt 3 then begin
           ww = wave[idx]
           ff = flux[idx]
           tir[xx,yy] = sh_integrate(transpose([[ww],[ff]]),_extra=_extra,/quiet,/noplot)
        endif
     endfor
  endfor

  return,tir

end

        
