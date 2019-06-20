pro align_spectra,refspectrum,spectrum,name,masterlun

  ;;refspectrum and spectrum are 320 x 30 pixel spectra that will need
  ;;to be aligned
  lspec1=refspectrum[116:146,*]
  lspec2=spectrum[116:146,*]
  center=15  ;;'central' pixel, [15,15]
  size=10     ;;do the fit in [5:25,5:25], if size is small fit is not robust
  edgesize=2 ;;with 2 pixels of edge

;  lspec1=refspectrum[12:42,*]
;  lspec2=spectrum[12:42,*]
;  center=15  ;;'central' pixel, [15,15]
;  size=11     ;;do the fit in [5:25,5:25], if size is small fit is not robust
;  edgesize=2 ;;with 2 pixels of edge

  shifts = timmi2_2D_fitshift(lspec1,lspec2,center=center,size=size,edgesize=edgesize)

  print,'shifts to align with reference spectrum: ',shifts[0],shifts[1]
  spectrum = timmi2_2D_shift(spectrum,shifts[0],shifts[1])

  printf,masterlun,shifts[0],shifts[1]

  default,outputfilename,'sa2_'+strcompress(string(name),/remove_all)+'.fits'
  MWRFITS,spectrum,outputfilename,/Create

END
