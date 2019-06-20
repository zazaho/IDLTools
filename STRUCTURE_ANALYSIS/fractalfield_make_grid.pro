;; stolen in part from build_fractal_cluster.pro
function hfmg_create_fractal_catalog, $
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


;; hfmg h=hierachy since f was already taken by clusterfield
PRO hfmg_create_catalog,$
   file,$
   NSTARS=NSTARS,$
   FRACTION=FRACTION,$
   FRACTALDIMENSION=FRACTALDIMENSION, $
   _extra=_extra
  
  ;; read global variables
  @grid_global_definitions.idl

  if round(fraction*nstars) gt 0 then begin
     catalog= npixels*hfmg_create_fractal_catalog( $
                      nstars=round(fraction*nstars), $
                      fractaldimension=fractaldimension, $
                      infoheader=infoheader, $
                      _extra=_extra $
                                                         )
  endif
  
  if round((1-fraction)*nstars) gt 0 then begin
     pmap_field = make_array(npixels,npixels,val=1d0/npixels^2)
     foo = populate_probability_map(pmap_field,nstars=round((1-fraction)*nstars),catalog=field_catalog)
     if n_elements(catalog) ne 0 then catalog=[[field_catalog],[catalog]] else catalog = field_catalog
  endif
  
  openw,lun,file,/get_lun
  printf,lun,infoheader
  printf,lun,catalog
  close,lun
  free_lun,lun
  
end

PRO hfmg_write_acf_info_file,dir

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

PRO fractalfield_make_grid, $
   Rootdir=Rootdir, $
   execute=execute, $
   fractaldimension=fractaldimension, $
   fraction=fraction, $
   help=help, $
   nstars=nstars, $
   _extra=_extra
  
  IF keyword_set(help) THEN BEGIN
     print,' PRO fractalfield_make_grid, [options]'
     print,'*** Parameter Options (Specify multiple values as an array) ***'
     print,'Nstars        = Total number of stars in the catalog [5150]'
     print,'Fraction      = Fraction of total stars in fractal [0.5]'
     print,'fractaldimension = 3D Fractal dimension of the field fractal [2.0]'
     print,'*** Other options: ***'
     print,'Rootdir       = name of the directory to put the grid in'
     print,'/Execute        If set, calculate the ACF after generating the star catalog'
     print,'/help           If set, show this help'
     return
  ENDIF 
  
  ;; read global variables
  @grid_global_definitions.idl
  calculate_batch_filename = 'fractalfield_grid_acf_batch.idl'
  
  ;; parameters that can vary
  default,nstars,5150L
  default,fraction,0.5
  default,fractaldimension,2.0
  default,execute,0

  cd,'./',current=pwd
  default,rootdir,pwd+'/FRACTALFIELD_GRID'

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
  dir3  = make_array(n_elements(FRACTALDIMENSION),value='')
  
  FOR j1 = 0, n_elements(dir1)-1 DO dir1 [j1 ] = 'NSTARS'+par_val_seperator+f2s(NSTARS[j1],0)
  FOR j2 = 0, n_elements(dir2)-1 DO dir2 [j2 ] = 'FRACTION'+par_val_seperator+f2s(FRACTION[j2],2)
  FOR j3 = 0, n_elements(dir3)-1 DO dir3 [j3 ] = 'FRACTALDIMENSION'+par_val_seperator+f2s(FRACTALDIMENSION[j3],2)

  FOR j1 = 0, n_elements(dir1)-1 DO BEGIN
     FOR j2 = 0, n_elements(dir2)-1 DO BEGIN
        FOR j3 = 0, n_elements(dir3)-1 DO BEGIN

           dir = dir1[j1]+'/'+dir2[j2]+'/'+dir3[j3]
           file_mkdir,dir
           
           IF file_test(dir+'/'+catalogfile_prefix+'.data*') THEN BEGIN
              message,/info,'Catalog already exists not redoing the work'
           ENDIF ELSE BEGIN
              hfmg_create_catalog,dir+'/'+catalogfile_prefix+'.data', $
                                  NSTARS=NSTARS[j1],$
                                  FRACTION=FRACTION[j2],$
                                  FRACTALDIMENSION=FRACTALDIMENSION[j3], $
                                  _extra=_extra
              
              hfmg_write_acf_info_file,dir
              
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
  
  close,lun_batch_reading
  free_lun,lun_batch_reading
  
  close,lun_batch_calculate
  free_lun,lun_batch_calculate
  
  popd

END  
