pro fancy_map,map,weightmap
  
  loadct,39
  
  nx=n_elements(map[*,0])
  ny=n_elements(map[0,*])

  min = min(map,max=max,/nan)
  smap = (map-min)/(max-min)*256.
  smap = finitise(smap)

  min = min(weightmap,max=max,/nan)
  sweightmap = (weightmap-min)/(max-min)
  sweightmap = finitise(sweightmap)*5d0

  factor = floor(900./max([nx,ny]))

  window,xsize=nx*factor,ysize=ny*factor
  
  deg = dindgen(11)*2d0*!dpi/10d0

  xc = cos(deg)
  yc = sin(deg)

  usersym,xc,yc,/fill

  for xx=0,nx-1 do begin
     for yy=0,ny-1 do begin
        plots,xx*factor*[1,1],yy*factor*[1,1],color=smap[xx,yy],symsize=sweightmap[xx,yy],/device,psym=8
     endfor
  endfor

end

     
