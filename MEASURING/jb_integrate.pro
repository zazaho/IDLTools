;; Simple function to integrate lambda against F_nu
;; Let wel: micron against Jansky
FUNCTION jb_integrate,lam,f,distance=distance

  IF NOT keyword_set(distance) THEN distance = 1
  ;;First go to 1d14*F_lambda
  ;;The 1d14 is for avoiding flaot errors
  f_lam = (f/lam/lam*3d14/1d26)*1d14

  npoints=n_elements(f_lam)
  intval=dblarr(npoints)
    
; now calculate the integrated fluxes  
;(SH Jan  5 1999) gejat van getlineflux.pro
;This is to triangulate the fluxes. For irregular grids

  intval[0]=0.25*(lam[1]-lam[0])
  intval[1:npoints-2]=0.5*(lam[2:npoints-1]-lam[0:npoints-3])
  intval[npoints-1]=0.25*(lam[npoints-1]-lam[npoints-2])
  totalflux=total(f_lam*intval)*1d-14

  parsec = 3.085678d16 ;;m
  l_sun = 3.826d26  ;; Watt
  print,'Integrated Flux',string(totalflux),' W/m/m'
  print,'Luminosity at a distance of: '+string(distance)+' pc equals: ' $
        +string(totalflux*4*!PI*(distance*parsec)^2d0/l_sun)
  return,totalflux
END
