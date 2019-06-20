FUNCTION get_profile,img_in,object=obj,center=cnt,pixelsize=pxs, $
                     flux=flx,background=bkg,fwhm=fwhm

  ;; do we want to normalise on a give flux?
  norm = keyword_set(flx)

  default,cnt,[49.5,49.5]
  default,pxs,0.0955
  default,flx,32.0
  default,bkg,[6,9]
  
  default,exc,0
  default,np,50
  default,extent,4 ;; in arcsec
  
  img=img_in

  ;; position of the annuli in pixels
  radi = dindgen(np)/(np-1d)*(extent/pxs)+1d0
  
  ;; position of the background in pixels
  bkg = bkg/pxs
  
  ;; Do we want to first convolve the image with a gaussian beam?
  IF keyword_set(fwhm) THEN BEGIN
      ;; convert to 
      fwh = fwhm/pxs ;; in pixels
      img = filter_image(img,fwhm=fwh)
  ENDIF 

  IF keyword_set(norm) THEN BEGIN
;  ;; define the object
;      x = floor(img*0+lindgen(n_elements(img))) MOD n_elements(img[*,0])
;      y = floor(img*0+lindgen(n_elements(img))) / n_elements(img[*,0])
;      obj = ( ((x-cnt[0])^2d0+(y-cnt[1])^2d0) LE (extent/pxs)^2d0 )
;      img = img/total(img*obj)*flx

      ;; now measure the flux within the radii
      aper,img,cnt[0],cnt[1], flux_in_radi, errap, sky, skyerr,1,radi,bkg,[-1d40,1d40],/flux,exact=exc,setsky=0d0
      ;; normalise to the give total flux
      flux_in_radi = flux_in_radi/flux_in_radi[np-1]*flx
  ENDIF ELSE BEGIN
      ;; now measure the flux within the radii
      aper,img*1d-11,cnt[0],cnt[1], flux_in_radi, errap, sky, skyerr,1,radi,[0,100],[-1d40,1d40],/flux,exact=exc,setsky=0d0
      flux_in_radi = flux_in_radi*1d11
  ENDELSE 

  ;; calculate the flux in the annuli
  flux_in_annulus = flux_in_radi-shift(flux_in_radi,1)
  flux_in_annulus[0] = flux_in_radi[0]

  ;; calculate the surface of each annulus in arcsec^2
  opp_annulus = !pi*(radi^2d0-(shift(radi,1))^2d0)
  opp_annulus[0] = !pi*radi[0]^2d0
  
  ;; the brightness is of course flux/surface
  bright_in_annulus = flux_in_annulus/opp_annulus

  ;; Calculate the distance in arcseconds for each point
  dist = radi*pxs

  ;; Convert to /steradian if we had normalised on the flux in Jy
  IF keyword_set(norm) THEN BEGIN
      bright_in_annulus = bright_in_annulus*4.25d10/(pxs^2d0)
  ENDIF

  data = make_array(2,n_elements(dist),value=0d0)
  data[0,*] = dist
  data[1,*] = bright_in_annulus

;  ;;Write this to a file
;  openw,lun,'hd56126_11.9_1D.dat',/get_lun
;  printf,lun,data
;  close,lun
;  free_lun,lun
  return,data
end
