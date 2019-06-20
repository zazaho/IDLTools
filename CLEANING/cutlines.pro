; remove lines from data by median filtering
; (SH Mar  2 2000) now using width parameter in median
function cutlines,in,level=level,base=base,res=res
  
  if is_aar(in) then begin
    f = in.data.flux
    w = in.data.wave
    out = in
  endif else begin
    print,'Please supply an aar as input'
    return,in
  endelse
  
  default,level,1d1
  default,base,4d1
  
  if keyword_set(res) then begin
    dores = 1
    lbin = 1d0-5d-1/res
    rbin =  1d0+5d-1/res
  endif else begin
    dores = 0
  endelse
  
  if (dores) then begin
    ; we want fixed resolution sampling so
    nelementsf = LONG(n_elements(f))
    medf = f
    
    for i=0L,nelementsf-1 do begin
      idx = where( (w gt w[i]*lbin) and (w lt w[i]*rbin) )
      medf[i] = median(f[idx])
    endfor
  endif else begin
    medf = median(f,base,/even)
  endelse
  
  out.data.flux = f - (f-medf)*(abs(f-medf) GT level)
  return,out
end
