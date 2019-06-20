;; stolen in part from build_fractal_cluster.pro
function chmg_create_fractal_catalog, $
   nstars=nstars, $
   fractaldimension=fractaldimension, $
   infoheader=infoheader, $
   _extra=_extra

  ;; yields floating values of x,y,z of the fractal
  stararray = generatefractal3d( $
              NSTARS=NSTARS, $
              FINALNSTARS=NSTARS, $
              FractalDimension=FractalDimension, $
              PerturbationScale=0.1, $
              infoheader=infoheader, $
              _extra=_extra $
                               )

  ;; now we need to scale this to 0-1,0-1,0-1
  ;; we take a 10% margin
  minmin=min(stararray,max=maxmax)

  stararray = 0.05 + 0.9*(stararray - minmin)/(maxmax-minmin)
  
  return,stararray[0:1,*]
end

;; chmg h=hierachy since f was already taken by clusterfield
PRO chmg_create_catalog,$
   file,$
   NSTARS=NSTARS,$
   FRACTION=FRACTION,$
   CORERADIUS=CORERADIUS,$
   GAMMA=GAMMA, $
   FRACTALDIMENSION=FRACTALDIMENSION, $
   _extra=_extra
  
  ;; read global variables
  @grid_global_definitions.idl
  
  ;; calculate the center in pixels of each cluster
  x0 = npixels/2d0
  y0 = npixels/2d0

  alpha= coreradius/sqrt(2.^(2/gamma)-1d0)

  index_map = lindgen(npixels,npixels)
  x = index_map mod npixels
  y = index_map  /  npixels
  
  r = sqrt((x-x0)^2+(y-y0)^2)*pixelsize

  pmap_cluster = (1.0+(r/alpha)^2)^(-1d0*gamma/2d0)
  pmap_cluster = pmap_cluster/total(pmap_cluster)
  foo = populate_probability_map(pmap_cluster,nstars=fraction*nstars,catalog=cluster_catalog)

  fractal_catalog= npixels*chmg_create_fractal_catalog( $
                   nstars=(1-fraction)*nstars, $
                   fractaldimension=fractaldimension, $
                   infoheader=infoheader, $
                   _extra=_extra $
                                                      )

  catalog=[[cluster_catalog],[fractal_catalog]]
  
  openw,lun,file,/get_lun
  printf,lun,infoheader
  printf,lun,catalog
  close,lun
  free_lun,lun

end

PRO chmg_write_acf_info_file,dir

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

;; routine to make a clusterfractal catalog simulation and run an
;; autocorrelation analysis on the generated catalog

;; parameters that can be varied:
;;  nstars: total number of stars [default 5150]
;;  fraction: fraction of total stars in cluster
;;  separation: projected separation in arcsec
;;  coreradius
;;  coreradius
;;  c2_coreradius
;;  gamma
;;  gamma
;;  gamma

PRO clusterfractal_make_grid, $
   nstars=nstars, $
   fraction=fraction, $
   coreradius=coreradius, $
   gamma=gamma, $
   fractaldimension=fractaldimension, $
   Rootdir=Rootdir, $
   execute=execute, $
   help=help, $
   _extra=_extra
  
  IF keyword_set(help) THEN BEGIN
     print,' PRO clusterfractal_make_grid, [options]'
     print,'*** Parameter Options (Specify multiple values as an array) ***'
     print,'Nstars        = Total number of stars in the catalog [5150]'
     print,'Fraction      = Fraction of total stars in Cluster [0.5]'
     print,'CoreRadius    = CoreRadius of the clusters [4"]'
     print,'gamma         = gamma value of the clusters [2]'
     print,'fractaldimension = 3D Fractal dimension of the field fractal [2.0]'
     print,'*** Other options: ***'
     print,'Rootdir       = name of the directory to put the grid in'
     print,'/Execute        If set, calculate the ACF after generating the star catalog'
     print,'/help           If set, show this help'
     return
  ENDIF 
  
  ;; read global variables
  @grid_global_definitions.idl
  calculate_batch_filename = 'clusterfractal_grid_acf_batch.idl'
  
  ;; parameters that can vary
  default,nstars,5150L
  default,fraction,0.5 ;; cluster_nstars = 0/2*nstars, fractal_nstars = 1/2*nstars
  default,coreradius,4. ;; arcsec
  default,gamma,2.0
  default,fractaldimension,2.0
  default,execute,0

  cd,'./',current=pwd
  default,rootdir,pwd+'/CLUSTERFRACTAL_GRID'

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
  dir3  = make_array(n_elements(CORERADIUS),value='')
  dir4  = make_array(n_elements(GAMMA),value='')
  dir5  = make_array(n_elements(FRACTALDIMENSION),value='')
  
  FOR j1 = 0, n_elements(dir1)-1 DO dir1 [j1 ] = 'NSTARS'+par_val_seperator+f2s(NSTARS[j1],0)
  FOR j2 = 0, n_elements(dir2)-1 DO dir2 [j2 ] = 'FRACTION'+par_val_seperator+f2s(FRACTION[j2],2)
  FOR j3 = 0, n_elements(dir3)-1 DO dir3 [j3 ] = 'CORERADIUS'+par_val_seperator+f2s(CORERADIUS[j3],2)
  FOR j4 = 0, n_elements(dir4)-1 DO dir4 [j4 ] = 'GAMMA'+par_val_seperator+f2s(GAMMA[j4],2)
  FOR j5 = 0, n_elements(dir5)-1 DO dir5 [j5 ] = 'FRACTALDIMENSION'+par_val_seperator+f2s(FRACTALDIMENSION[j5],2)
  
  FOR j1 = 0, n_elements(dir1)-1 DO BEGIN
     FOR j2 = 0, n_elements(dir2)-1 DO BEGIN
        FOR j3 = 0, n_elements(dir3)-1 DO BEGIN
           FOR j4 = 0, n_elements(dir4)-1 DO BEGIN
              FOR j5 = 0, n_elements(dir5)-1 DO BEGIN
                       
                 dir = dir1[j1]+'/'+dir2[j2]+'/'+dir3[j3]+'/'+dir4[j4]+'/'+dir5[j5]
                 file_mkdir,dir
                 
                 IF file_test(dir+'/'+catalogfile_prefix+'.data*') THEN BEGIN
                    message,/info,'Catalog already exists not redoing the work'
                 ENDIF ELSE BEGIN
                    chmg_create_catalog,dir+'/'+catalogfile_prefix+'.data', $
                                        NSTARS=NSTARS[j1],$
                                        FRACTION=FRACTION[j2],$
                                        CORERADIUS=CORERADIUS[j3],$
                                        GAMMA=GAMMA[j4], $
                                        FRACTALDIMENSION=FRACTALDIMENSION[j5], $
                                        _extra=_extra
                 
                    chmg_write_acf_info_file,dir
                    
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
  close,lun_batch_reading
  free_lun,lun_batch_reading
  
  close,lun_batch_calculate
  free_lun,lun_batch_calculate
  
  popd

END  
