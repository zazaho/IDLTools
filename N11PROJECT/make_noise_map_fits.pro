pro make_noise_map_fits,filename,outfile=outfile

  default,outfile,'noisemap.fits'

  img=readfits(filename,hdr)
  
  nx = n_elements(img[*,0])
  ny = n_elements(img[0,*])
  noiselevel=sxpar(hdr,'SIGMABKG')
  
  noisemap=randomn(seed,nx,ny)*noiselevel

  writefits,outfile,noisemap,hdr

end
