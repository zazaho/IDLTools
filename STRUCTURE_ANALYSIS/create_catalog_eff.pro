PRO create_catalog_eff,file,$
                   NSTARS=NSTARS,$
                   CORERADIUS=CORERADIUS,$
                   GAMMA=GAMMA
  
  npixels = 6500L
  d_SMC = 60.0     ;; kpc
  pixelsize = 0.05 ;; arcsec

  AU_in_pc = !dpi/(180d0*3600d0)

  default,nstars,5150L
  default,coreradius,4.0 ;;arcsec
  default,gamma,2.0

  ;; parse the input file
  dir = file_dirname(file)
  catalogfile_prefix = file_basename(file,'.data')

  index_map = lindgen(npixels,npixels)
  x = index_map mod npixels
  y = index_map  /  npixels
  r = sqrt((x-npixels/2d0)^2+(y-npixels/2d0)^2)*pixelsize

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this is specific for the EFF function
  alpha= coreradius/sqrt(2.^(2/gamma)-1d0)
  pmap = (1.0+(r/alpha)^2)^(-1d0*gamma/2d0)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; normalise to unity
  pmap = pmap/total(pmap)
  foo = populate_probability_map(pmap,nstars=nstars,catalog=catalog)
  
  openw,lun,file,/get_lun
  printf,lun,catalog
  close,lun
  free_lun,lun

  openw,lun,dir+'/'+catalogfile_prefix+'.info',/get_lun
  
  printf,lun,catalogfile_prefix
  printf,lun,f2s(pixelsize,2)+' # pixel scale in arcsec'
  printf,lun,f2s(npixels^2,3)+' # total useful (observed) pixels'
  printf,lun,f2s(d_SMC*1d3*AU_in_pc,2)+' # arcsec to parsec correspondance'
  printf,lun,format='(4I,A)',0,npixels,0,npixels,' # total original X, Y, pixel coordinates limits'
  printf,lun,format='(4I,A)',0,npixels,0,npixels,' # selected region X, Y pixel coordinates limits'
  close,lun
  free_lun,lun

END
