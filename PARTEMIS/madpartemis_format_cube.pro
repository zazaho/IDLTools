;+
; NAME: MADPARTEMIS_FORMAT_CUBE
;
;
; PURPOSE: convert (x,y,t) data array into (good detector,t)
;
;
; CALLING SEQUENCE: cube2d = madpartemis_format_cube(cube3d)
;
;
; MODIFICATION HISTORY:
;
;
;-
function madpartemis_format_cube, cube, goodpix_ima, rsamp
 
 
 ;restore, '/mnt/local/home/gwillman/apexpro_pipeline_v2_iap/Calib_partemis/goodpix_ima_mars_28nov07.xdr'

  dbad=reform(goodpix_ima,256)
 ;dbad=reform(goodpix_ima_50010,256)
 ;dbad=dblarr(256)+1.
 igood=where(dbad EQ 1, count)
 igood=double(igood)
 ixdetector=igood mod 16.d0
 iydetector=igood / (16.d0)
 size = size(cube, /dimensions)
 output = dblarr(size[2], count)
 
 
 for i=0l, count-1 do begin
    output[*, i] = reform(cube[ixdetector[i], iydetector[i], *])
 endfor
 
 return, output

end
