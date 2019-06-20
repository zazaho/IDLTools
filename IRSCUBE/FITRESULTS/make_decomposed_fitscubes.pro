;; should rework the default filenames to decent values
pro make_decomposed_fitscubes,xdr=xdr,fitsfile=fitsfiles,prefix=prefix

  default,xdr,'*.xdr'
  default,fitsfile,'*.fits'
  default,prefix,'../decomposed_cube_'

  restore,xdr
  originalcube=readfitscube(fitsfile)

  tot = fit.fnu_tot
  pah = total(fit.fnu_band,3)
  rest = tot - pah
  wave = fit.w

  totstruct ={cube:tot ,header:originalcube.header,wave:wave,hwave:originalcube.hwave,mask:mask eq 0}
  pahstruct ={cube:pah ,header:originalcube.header,wave:wave,hwave:originalcube.hwave,mask:mask eq 0}
  reststruct={cube:rest,header:originalcube.header,wave:wave,hwave:originalcube.hwave,mask:mask eq 0}
  
  writefitscube,totstruct ,filename=prefix+'total_fit.fits',/usemask
  writefitscube,pahstruct ,filename=prefix+'pah_fit.fits',/usemask
  writefitscube,reststruct,filename=prefix+'rest_fit.fits',/usemask

end
