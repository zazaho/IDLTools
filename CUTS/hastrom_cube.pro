pro hastrom_cube,fitsfile,refheader,outdir

  cube = mrdfits(fitsfile,0,hdr)
  wave = mrdfits(fitsfile,1,hdr2)

  sxaddpar,hdr,'NAXIS',2
  sxaddpar,hdr,'NAXIS3',1

  basename=strmid(fitsfile,strpos(fitsfile,'/',/reverse_search)+1)
  
  for i=0,n_elements(wave)-1 do begin
     hastrom,reform(cube[*,*,i]),hdr,proj,hdr_proj,refheader,missing=!values.d_nan
     if n_elements(cube_proj) eq 0 then begin
        cube_proj = make_array( $
                    n_elements(proj[*,0]), $
                    n_elements(proj[0,*]), $
                    n_elements(wave) $
                              )
     endif
     cube_proj[*,*,i] = proj
  endfor
  sxaddpar,hdr_proj,'NAXIS',3
  sxaddpar,hdr_proj,'NAXIS3',n_elements(wave)
  mwrfits,cube_proj,outdir+'/'+basename,hdr_proj,/create
  mwrfits,wave,outdir+'/'+basename,hdr2
end
