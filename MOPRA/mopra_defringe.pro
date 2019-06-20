;; silly fudge to remove fringes from mopra data
;; needs an aar structure of band 3c
;; in this case 'band3c.fits' read by
;; band3c = sh_read_faar('band3c.fits') will do

function mopra_defringe,y,band3c, $
                        nfringes=nfringes, $
                        noplot=noplot, $
                        _extra=_extra
  
  default,nfringes,4
  if n_elements(noplot) eq 0 then noplot=1
  
  ;; select the right number of points to put the y values in
  sub3c = select(band3c,indgen(n_elements(band3c.data.wave)) lt n_elements(y))

  ;; replace the y values and add 1d2 
  sub3c.data.flux = y+1d2

  defringed = fringes(sub3c,band='3c',nfring=nfringes,noplot=noplot,_extra=_extra)

  return,defringed.data.flux-1d2

end
