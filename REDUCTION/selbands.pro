FUNCTION selbands, aar, bands, REMOVE=remove, COUNT=tcnt
; main: select given bands data
; aar: AAR
; bands: aot_bands to select
; /remove: remove specified bands from data
; COUNT: number of selected data. returned value. 97/04/24
; 97/05/01 use indband() instead of having own one

flg=intarr(n_elements(aar.data))
tcnt=long(0)

for i = 0, n_elements(bands) - 1 do begin
   index=indband(aar, bands(i), count=count)
   tcnt = tcnt + count
   if( count gt 0 ) then flg(index) = 1
endfor

if (keyword_set(remove)) then return, select(aar, flg eq 0) $
else                          return, select(aar, flg eq 1)

end
