;; conv_cube,'hii8_hii243_ll_LL3_cube.fits','../KERNELS/irac_c1_to_mips_160_100K.fits','CONV'
pro conv_cube_sed,fitsfile,kernel,outdir

  cube = mrdfits(fitsfile,0,hdr)
  
  ;; reference values
  crval3  = sxpar(hdr,'CRVAL3' )
  ;; wavelength step in micron
  cdelt3  = sxpar(hdr,'CDELT3')
  ;; reference pixel
  crpix3  = sxpar(hdr,'CRPIX3' )
  ;; number of wavelengths
  nwave = n_elements(cube[0,0,*])

  wave = crval3+(dindgen(nwave)-crpix3)*cdelt3

  basename=strmid(fitsfile,strpos(fitsfile,'/',/reverse_search)+1)
  
  for i=0,n_elements(wave)-1 do begin
     rnd = n2s(floor(randomu(seed)*1d7))
     tmpfile = '/tmp/conv_cube'+rnd
     mwrfits,reform(cube[*,*,i]),tmpfile+'.fits',hdr
     CONV_IMAGE,tmpfile+'.fits',kernel,tmpfile+'_conved.fits',/no_derotate
     conved = mrdfits(tmpfile+'_conved.fits',0,hdr_conved)
     if n_elements(cube_conved) eq 0 then begin
        cube_conved = make_array( $
                      n_elements(conved[*,0]), $
                      n_elements(conved[0,*]), $
                      n_elements(wave) $
                                )
     endif
     cube_conved[*,*,i] = conved
     spawn,'rm -f '+tmpfile+'.fits '+tmpfile+'_conved.fits'
  endfor
  mwrfits,cube_conved,outdir+'/'+basename,hdr_conved,/create
  mwrfits,wave,outdir+'/'+basename,hdr2
end
