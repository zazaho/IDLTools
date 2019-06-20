function sh_getresp,band,det=det,ref=ref

  dets = band2det(band)

  if keyword_set(det) then begin
    i = where(dets eq det,count) 
    if (count gt 0) then begin
      det = det - (det/12)*12
    endif else begin
      print,'The band you specified is not observed with this detector'
      print,'using default detector 6'
      det = 6
    endelse 
  endif else begin
    det = 6
  endelse

; the resp.data structure is an array base 0 so produce -1 offset
  det=det-1

  resp = readcal('25_'+band)

  l = resp.data.(det*3)
  r = resp.data.(det*3+1)
  i = where (l ne 0)

  l = reverse(l(i))
  r = reverse(r(i))

  if (keyword_set(ref) and is_aar(ref)) then begin
    r = spline(l,r,ref.data.wave)
    l = ref.data.wave
  endif

  out = define_aar(length=n_elements(l))
  out.data.wave = l
  out.data.flux = r

  return,out
end
