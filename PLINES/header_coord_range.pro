;; determine the box in ra,dec that fits around a image based on the
;; header info
function header_coord_range,header
  if n_elements(header) gt 1 then begin
     nx = sxpar(header,'NAXIS1')
     ny = sxpar(header,'NAXIS2')
     ;; the coordinates of the outer edges of the corner pixels
     xyad,header,[-0.5,nx-1+0.5,nx-1+0.5,-0.5],[-0.5,-0.5,ny-1+0.5,ny-1+0.5],ra,dec
     ra_min=min(ra,max=ra_max)
     dec_min=min(dec,max=dec_max)
  endif else begin
     ;; bogus values
     ra_min=400d0
     ra_max=-400d0
     dec_min=900d0
     dec_max=-900d0
  endelse
  return,[ra_min,dec_min,ra_max,dec_max]
end
