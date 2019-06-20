pro sh_show_variance,n,band=band

ar = n

if keyword_set(band) then ar = get_aotband(ar,band)

bands = ['1a','1b','1d','1e',$
  '2a','2b','2c',$
  '3a','3c','3d','3e',$
  '4']

for j = 0,11 do begin
  if sh_has_aotband(ar,bands[j]) then begin
    art = get_aotband(ar,bands[j])
    det = band2det(bands[j])
    mmean = fltarr(12)
    mstdev = fltarr(12)
    print,bands[j]
    print,'det mean variance skewness'
    for i=0,11 do begin
      ind = where(art.data.det eq det[i],count)  
      if count gt 0 then begin
        m = moment(art.data[ind].flux)
        print,det[i],m[0],m[1],m[2]
        mmean[i] = m[0]
        mstdev[i] = m[1]
      endif
    endfor
    mstdev = mstdev*max(mmean)/max(mstdev)
    plot,det,mstdev,ps=10,title='mean/relative variance per detector'$
      ,xr=[det[0],det(11)+1]
    oplot,det,mmean,ps=1
    oplot,det,mstdev/mmean*max(mmean)/max(mstdev/mmean),ps=2
    dummy = ''
    read,dummy,prompt='enter to continue'
  endif
endfor
end
