; sh_velscale
; this procedure makes plots of aar data in velocity scale

pro sh_velscale,aar,CENTER=center,VRANGE=vrange,OPLOT=oplot

  c=3e5
  default,vrange,[-2000,2000]

jump1:

  if not keyword_set(center) then begin
    plotaar,aar,psym=0
    print,'select which line, right button to zoom'
    veri,center,/nostatus,button=b
    if b eq 4 then begin
      lrange = [center*(vrange[0]/c+1), $
                center*(vrange[1]/c+1)]
      plotaar,aar,psym=0,xr=lrange
      veri,center,/nostatus
    endif
  endif
  print,center

  foo = aar
  foo.data.wave = (foo.data.wave/center -1d0)*c
  foo = select(foo,foo.data.wave ge vrange[0] AND $
               foo.data.wave le vrange[1])
  minflux = min(foo.data.flux)
  maxflux = max(foo.data.flux)
  frange = [minflux-(maxflux-minflux)*.1,maxflux+(maxflux-minflux)*.1]
  if not keyword_set(oplot) then begin
    plot,foo.data.wave,foo.data.flux,xr=vrange,yr=frange,psym=0
  endif else begin
    oplot,foo.data.wave,foo.data.flux
  end

  veri,x
  center = 0

  print,'left button for another line right button to stop'
  !err = 0
  Cursor,x,y,2,/down
  if !err ne 4 then goto, jump1
end
