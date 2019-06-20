;; translate coords to pixel offset from center
;; input values ra_c,dec_c, of the center
;; scale pixel scale in arcsec/pixel
;; pts point pairs in [ra,dec]
function coord2pix,radec_c,scale,radec
  dx = -1d0*(radec[0] - radec_c[0])*36d2/scale*cos(!pi/180d0*radec[1])
  dy = (radec[1] - radec_c[1])*36d2/scale
  return,[dx,dy]
END
