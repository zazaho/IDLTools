FUNCTION indband, aar, band, COUNT=count
; return indexes of one selected band
; separated from selbands.pro 97/03/17
; 97/05/01 add COUNT to return number of selected data
; 97/05/25 band = '0' for all data (used for merged data)

det=intarr(2)
iband = strmid(band, 0, 1)
if (iband eq 1) then det=[ 1, 12]
if (iband eq 2) then det=[13, 24]
if (iband eq 3) then det=[25, 36]
if (iband eq 4) then det=[37, 48]
if (iband eq 5) then det=[49, 50]
if (iband eq 6) then det=[51, 52]

if (iband eq 0) then begin
   idx = where(aar.data.wave gt 0.0, count)
endif else if (iband eq 5 OR iband eq 6) then begin
   idx = where( test_flag(aar.data.flag, /nodata) eq 0 AND $
                aar.data.det ge det(0) AND aar.data.det le det(1), $
                count )
endif else begin
   ador = aot_to_apband(band)
   idx = where( test_status(aar.data.status, aper=ador(0)) AND $
                test_flag(aar.data.flag, order=ador(2)) AND $
                test_flag(aar.data.flag, /nodata) eq 0 AND $
                aar.data.det ge det(0) AND aar.data.det le det(1), $
                count )
endelse
   if (count gt 0) then return, idx $
   else                 return, -1
end
