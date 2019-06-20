PRO mcmg_create_catalog,$
   file,$
   NSTARS=NSTARS,$
   FRACTION=FRACTION,$
   SEPARATION=SEPARATION,$
   C1_CORERADIUS=C1_CORERADIUS,$
   C1_GAMMA=C1_GAMMA,$
   C2_CORERADIUS=C2_CORERADIUS,$
   C2_GAMMA=C2_GAMMA
  
  ;; read global variables
  @grid_global_definitions.idl
  
  ;; calculate the center in pixels of each cluster
  c1_x0 = npixels/2d0-separation/2d0/pixelsize
  c1_y0 = npixels/2d0

  c2_x0 = npixels/2d0+separation/2d0/pixelsize
  c2_y0 = npixels/2d0

  c1_alpha= c1_coreradius/sqrt(2.^(2/c1_gamma)-1d0)
  c2_alpha= c2_coreradius/sqrt(2.^(2/c2_gamma)-1d0)

  index_map = lindgen(npixels,npixels)
  x = index_map mod npixels
  y = index_map  /  npixels

  c1_r = sqrt((x-c1_x0)^2+(y-c1_y0)^2)*pixelsize
  c2_r = sqrt((x-c2_x0)^2+(y-c2_y0)^2)*pixelsize

  pmap = $
     fraction     * (1.0+(c1_r/c1_alpha)^2)^(-1d0*c1_gamma/2d0) + $
     (1-fraction) * (1.0+(c2_r/c2_alpha)^2)^(-1d0*c2_gamma/2d0)

  ;; normalise to unity
  pmap = pmap/total(pmap)
  
  foo = populate_probability_map(pmap,nstars=nstars,catalog=catalog)
  
  openw,lun,file,/get_lun
  printf,lun,catalog
  close,lun
  free_lun,lun

end

PRO mcmg_write_acf_info_file,dir

  ;; read global variables
  @grid_global_definitions.idl
  openw,lun,dir+'/'+catalogfile_prefix+'.info',/get_lun
  
  AU_in_pc = !dpi/(180d0*3600d0)

  printf,lun,catalogfile_prefix
  printf,lun,f2s(pixelsize,2)+' # pixel scale in arcsec'
  printf,lun,f2s(npixels^2,3)+' # total useful (observed) pixels'
  printf,lun,f2s(d_SMC*1d3*AU_in_pc,2)+' # arcsec to parsec correspondance'
  printf,lun,format='(4I,A)',0,npixels,0,npixels,' # total original X, Y, pixel coordinates limits'
  printf,lun,format='(4I,A)',0,npixels,0,npixels,' # selected region X, Y pixel coordinates limits'
  close,lun
  free_lun,lun

END

;; routine to make a multicluster catalog simulation and run an
;; autocorrelation analysis on the generated catalog

;; parameters that can be varied:
;;  nstars: total number of stars [default 5150]
;;  fraction: fraction of total stars in cluster 1
;;  separation: projected separation in arcsec
;;  coreradius
;;  c1_coreradius
;;  c2_coreradius
;;  gamma
;;  c1_gamma
;;  c1_gamma

PRO multicluster_make_grid, $
   nstars=nstars, $
   fraction=fraction, $
   separation=separation, $
   coreradius=coreradius, $
   c1_coreradius=c1_coreradius, $
   c2_coreradius=c2_coreradius, $
   gamma=gamma, $
   c1_gamma=c1_gamma, $
   c2_gamma=c2_gamma, $
   Rootdir=Rootdir, $
   execute=execute, $
   help=help
  
  IF keyword_set(help) THEN BEGIN
     print,' PRO multicluster_make_grid, [options]'
     print,'*** Parameter Options (Specify multiple values as an array) ***'
     print,'Nstars        = Total number of stars in the catalog [5150]'
     print,'Fraction      = Fraction of total stars in Cluster 1 [0.67]'
     print,'Separation    = Projected distance between the cluster centers [10"]'
     print,'CoreRadius    = CoreRadius of the clusters [4"]'
     print,'C1_CoreRadius = CoreRadius of the cluster 1 [CoreRadius]'
     print,'C2_CoreRadius = CoreRadius of the cluster 2 [CoreRadius]'
     print,'gamma         = gamma value of the clusters [2]'
     print,'C1_gamma      = gamma value of clusters 1 [gamma]'
     print,'C2_gamma      = gamma value of clusters 2 [gamma]'
     print,'*** Other options: ***'
     print,'Rootdir       = name of the directory to put the grid in'
     print,'/Execute        If set, calculate the ACF after generating the star catalog'
     print,'/help           If set, show this help'
     return
  ENDIF 
  
  ;; read global variables
  @grid_global_definitions.idl
  calculate_batch_filename = 'multicluster_grid_acf_batch.idl'
  
  ;; parameters that can vary
  default,nstars,5150L
  default,fraction,0.67 ;; c1_nstars = 2/3*nstars, c2_nstars = 1/3*nstars
  default,separation,10. ;; arcsec
  default,coreradius,4. ;; arcsec
  default,c1_coreradius,coreradius
  default,c2_coreradius,coreradius
  default,gamma,2.0
  default,c1_gamma,gamma
  default,c2_gamma,gamma
  default,execute,0

  cd,'./',current=pwd
  default,rootdir,pwd+'/MULTICLUSTER_GRID'

  ;; move into the root directory
  file_mkdir,rootdir
  pushd,rootdir
  
  openw,lun_batch_calculate,calculate_batch_filename,/get_lun,/append
  
  if file_test(read_batch_filename) then begin
     reading_offset=file_lines(read_batch_filename)
     openw,lun_batch_reading,read_batch_filename,/get_lun,/append
  endif else begin
     reading_offset=0
     openw,lun_batch_reading,read_batch_filename,/get_lun
  endelse

;; Make the dirnames once here
  dir1  = make_array(n_elements(NSTARS),value='')
  dir2  = make_array(n_elements(FRACTION),value='')
  dir3  = make_array(n_elements(SEPARATION),value='')
  dir4  = make_array(n_elements(C1_CORERADIUS),value='')
  dir5  = make_array(n_elements(C1_GAMMA),value='')
  dir6  = make_array(n_elements(C2_CORERADIUS),value='')
  dir7  = make_array(n_elements(C2_GAMMA),value='')
  
  FOR j1 = 0, n_elements(dir1)-1 DO dir1 [j1 ] = 'NSTARS'+par_val_seperator+f2s(NSTARS[j1],0)
  FOR j2 = 0, n_elements(dir2)-1 DO dir2 [j2 ] = 'FRACTION'+par_val_seperator+f2s(FRACTION[j2],2)
  FOR j3 = 0, n_elements(dir3)-1 DO dir3 [j3 ] = 'SEPARATION'+par_val_seperator+f2s(SEPARATION[j3],1)
  FOR j4 = 0, n_elements(dir4)-1 DO dir4 [j4 ] = 'C1_CORERADIUS'+par_val_seperator+f2s(C1_CORERADIUS[j4],2)
  FOR j5 = 0, n_elements(dir5)-1 DO dir5 [j5 ] = 'C1_GAMMA'+par_val_seperator+f2s(C1_GAMMA[j5],2)
  FOR j6 = 0, n_elements(dir6)-1 DO dir6 [j6 ] = 'C2_CORERADIUS'+par_val_seperator+f2s(C2_CORERADIUS[j6],2)
  FOR j7 = 0, n_elements(dir7)-1 DO dir7 [j7 ] = 'C2_GAMMA'+par_val_seperator+f2s(C2_GAMMA[j7],2)
  
  FOR j1 = 0, n_elements(dir1)-1 DO BEGIN
     FOR j2 = 0, n_elements(dir2)-1 DO BEGIN
        FOR j3 = 0, n_elements(dir3)-1 DO BEGIN
           FOR j4 = 0, n_elements(dir4)-1 DO BEGIN
              FOR j5 = 0, n_elements(dir5)-1 DO BEGIN
                 FOR j6 = 0, n_elements(dir6)-1 DO BEGIN
                    FOR j7  = 0, n_elements(dir7)-1 DO BEGIN
                       
                       dir = dir1[j1]+'/'+dir2[j2]+'/'+dir3[j3]+'/'+dir4[j4]+'/'+dir5[j5]+'/'+dir6[j6]+'/'+dir7[j7]
                       file_mkdir,dir
                       
                       IF file_test(dir+'/'+catalogfile_prefix+'.data*') THEN BEGIN
                          message,/info,'Catalog already exists not redoing the work'
                       ENDIF ELSE BEGIN
                          mcmg_create_catalog,dir+'/'+catalogfile_prefix+'.data', $
                                                                NSTARS=NSTARS[j1],$
                                                                FRACTION=FRACTION[j2],$
                                                                SEPARATION=SEPARATION[j3],$
                                                                C1_CORERADIUS=C1_CORERADIUS[j4],$
                                                                C1_GAMMA=C1_GAMMA[j5],$
                                                                C2_CORERADIUS=C2_CORERADIUS[j6],$
                                                                C2_GAMMA=C2_GAMMA[j7]
                          
                          
                          mcmg_write_acf_info_file,dir
                          
                          ;; write the command for the autocorrelations in the batchfile
                          printf,lun_batch_calculate,calculate_command+',"'+dir+'"'
                          flush,lun_batch_calculate

                          ;; and add a line to the reading file
                          printf,lun_batch_reading,'acf'+i2s(reading_offset,4)+'='+read_command+'("'+dir+'")'
                          flush,lun_batch_reading
                          reading_offset = reading_offset+1
                          
                       ENDELSE

                       if execute ne 0 then begin
                          foo = execute(calculate_command+',dir')
                       endif

                    ENDFOR 
                 ENDFOR
              ENDFOR
           ENDFOR
        ENDFOR
     ENDFOR
  ENDFOR
  
  close,lun_batch_reading
  free_lun,lun_batch_reading
  
  close,lun_batch_calculate
  free_lun,lun_batch_calculate
  
  popd

END  
