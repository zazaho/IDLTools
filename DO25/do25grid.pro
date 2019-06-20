PRO do25grid_modust,file, $
                  L_star=L_star, $
                  R_in=R_in, $
                  R_out=R_out, $
                  rho=rho
  
  openw,lun,file,/get_lun

  T_sun = 5780d0
  T_star = 10000d0

  R_STAR = sqrt(L_star*((T_SUN/T_STAR)^(4.))) ;; in R_sun
  
  printf,lun,'"MODE"'
  printf,lun,'     3                              ! pmode'
  printf,lun,'"SOURCE:CENTRAL:BB"'
  printf,lun,' '+f2s(R_star,dec=1)+'  '+f2s(T_star,dec=1)+'  ! '+f2s(L_STAR,0)+' L_sun'
  printf,lun,'"SHELL:POWER LAW"'
  printf,lun,' '+f2s(R_in,dec=3)+'  '+f2s(R_out,dec=3)+'                ! rmin, rmax '
  printf,lun,' '+f2s(rho)+'   0.0   0.005   ! rho, power, fracdg'
  printf,lun,'"GRID SETUP"'
  printf,lun,' 15   35   115     1  1  1.0  2  8     ! ns,ni,nr,irmode,ipmode  np = ns+ni+nr'
  printf,lun,'0.2   2000.  0.01     5.  45.   1.0  0 ! wre_(min,max,step) wsp_(min,max,step)'
  printf,lun,' 100.  0.1                              ! tre, dtre_conv'
  printf,lun,'"CONVERGENCE"'
  printf,lun,'100     1.e-4    0  0  0           0 0  ! it_REmax,epsconv,mode_LI,mode_LC,mode_FeaSmooth,dummys'
  printf,lun,'"EOF"'
  close,lun
  free_lun,lun

END

PRO do25grid_dust,file,amin=amin,amax=amax
  
  openw,lun,file,/get_lun
  printf,lun,'* Input file for radiative transfer program'
  printf,lun,'* listed are the names of the dust types and'
  printf,lun,'* on/of switch, mass fraction, minimum grain size maximum'
  printf,lun,'* grain size, grain size interval in dex , the power of the grain'
  printf,lun,'* size distribution, the density'
  printf,lun,'* of the grain material and the far IR efficiency law:'
  printf,lun,'*'
  printf,lun,'* SELECT  ABUNDANCE  AMIN  AMAX  NA  MA  RHO  ALPHA'
  printf,lun,'*'
  printf,lun,'** family: olivine'
  printf,lun,'** latice structure: A'
  printf,lun,'** chem. comp.: mgfe'
  printf,lun,'** ID: 001.lnk'
  printf,lun,'*   '
  printf,lun,'"CROSS SECTIONS:MIE"'
  printf,lun,'/usr/local/iso/dbase2/olivine/A/mgfe_03.71_0000_001.lnk '
  printf,lun,'1  1.0 '+f2s(alog10(amin),2)+'  '+f2s(alog10(amax),2)+'    11    -3.50    03.71    -1.0    15000.    0'
  printf,lun,'EOF'
  close,lun
  free_lun,lun

END

PRO do25grid, $
   L_star=L_star, $
   R_in=R_in,R_out=R_out,rho=rho,amin=amin,amax=amax, $
   help=help,Rootdir=Rootdir,distance=distance,execute=execute, $
   offset=offset,clean=clean
  
  IF keyword_set(help) THEN BEGIN
      print,' PRO do25grid, [options]'
      print,'--- options:----'

      print,'L_star      = luminosity of the central star in L_sun'
      print,'R_in      = (array of) Inner radius in parsec'
      print,'R_out     = (array of) Outer radius in parsec'
      print,'rho       = (array of) densities in hydrogen/cm3'
      print,'amin      = (array of) miniumum grainsize in mum'
      print,'amax      = (array of) maximum grainsize in mum'
      
      print,'Rootdir    = name of the directory to put the grid in'
      print,'Distance   = The distance to assume when reading the files'
      print,'Offset     = First number to use when reading the files (modelXXX)'
      print,'Execute      If set the run the grid after generating it'
      print,'Clean        If set remove extra modust_output files after running it'
      print,'help         If set show this help'

      return
  ENDIF 

  default,distance,5.5d3 ;; pc of do25
  default,offset,0

  ;;;; Constants
  ;; cgs units
  c = 2.998d10
  G =  6.67259d-8

  R_sun = 6.96d10
  M_sun = 1.99d33
  T_sun = 5780.
  sec_year = 365.*24.*3600.
  parsec = 3.085677582d18                 ;; Parsec cm

  amu = 1.66053873d-24                    ;; Atomic mass unit gram
  
  ;; something about the star
  default,L_star,1d5 ;; L_sun
  
  default,T_star,30000d0
  
  R_star = sqrt(L_star*((T_sun/T_star)^(4.))^(0.5))
  R_star_cm = R_star*R_sun
  
  ;;default values for the parameters
  default,R_in      ,22d0  ; pc
  default,R_out     ,27d0  ; pc
  
  default,amin      ,[0.01]
  default,amax      ,[1.0]
  default,rho       ,1d0 ;; cgs
  
  rho_cgs = rho*amu

  R_in_R_star = R_in*parsec/R_star_cm
  R_out_R_star = R_out*parsec/R_star_cm

  ;; create a new subdir to store the tree
  IF NOT keyword_set(rootdir) THEN BEGIN
     suffix = 1
     WHILE file_test('GRID_'+i2s(suffix,2)) DO BEGIN
        suffix = suffix+1
     ENDWHILE
     rootdir = 'GRID_'+i2s(suffix,2)
  ENDIF 

  ;; move into this directory
  file_mkdir,rootdir
  pushd,rootdir
  
;; Now make some administration to store the grid parameters
  openw,lun,'grid_parameters.txt',/get_lun
  printf,lun,'Stellar parameters'
  printf,lun,'Radius      ',reform(R_star,n_elements(R_star)),' R_sun = ', $
         reform(R_star_cm,n_elements(R_star_cm)),' cm'
  printf,lun,'Luminosity  ',L_star,' L_sun'

  printf,lun,'-----------------------------------------'
  printf,lun,'Shell parameters'
  printf,lun,'Density ',rho,' hydrogen/cm3'
  printf,lun,'Inner radius',R_in,' parsec'
  printf,lun,'Outer radius',R_out,' parsec'
  printf,lun,'Minimum dust grain diameter',amin,' mum'
  printf,lun,'Maximum dust grain diameter',amax,' mum'

  close,lun
  free_lun,lun
  
  ;; Something about the command to execute in the directory to
  ;; calculate the model
  default,model_command,'modust; '

  IF keyword_set(clean) THEN BEGIN
      ;; remove extra output files and compress the dust{,1,2}.out files
      model_command=model_command+'clean_modust; '
  ENDIF 

  openw,lun_batch,'do_batch.csh',/get_lun
  openw,lun_reading,'readfiles.idl',/get_lun,/append
  counter = offset

;; Make the dirnames once here
  dir1  = make_array(n_elements(L_STAR),value='')
  dir2  = make_array(n_elements(R_in),value='')
  dir3  = make_array(n_elements(R_out),value='')
  dir4  = make_array(n_elements(rho),value='')
  dir5  = make_array(n_elements(amin),value='')
  dir6  = make_array(n_elements(amax),value='')

  FOR j1  = 0, n_elements(dir1)-1 DO dir1 [j1 ] = 'L_STAR:'+f2s(L_STAR[j1],0)
  FOR j2  = 0, n_elements(dir2)-1 DO dir2 [j2 ] = 'R_IN:'  +f2s(R_in  [j2],2)
  FOR j3  = 0, n_elements(dir3)-1 DO dir3 [j3 ] = 'R_OUT:' +f2s(R_out [j3],2)
  FOR j4  = 0, n_elements(dir4)-1 DO dir4 [j4 ] = 'RHO:'   +f2s(rho   [j4])
  FOR j5  = 0, n_elements(dir5)-1 DO dir5 [j5 ] = 'AMIN:'  +f2s(amin  [j5])
  FOR j6  = 0, n_elements(dir6)-1 DO dir6 [j6 ] = 'AMAX:'  +f2s(amax  [j6])
  

  FOR j1 = 0, n_elements(dir1)-1 DO BEGIN
     FOR j2 = 0, n_elements(dir2)-1 DO BEGIN
        FOR j3 = 0, n_elements(dir3)-1 DO BEGIN
           FOR j4 = 0, n_elements(dir4)-1 DO BEGIN
              FOR j5 = 0, n_elements(dir5)-1 DO BEGIN
                 FOR j6 = 0, n_elements(dir6)-1 DO BEGIN
                    dir = dir1[j1]+'/'+dir2[j2]+'/'+dir3[j3]+'/'+dir4[j4]+'/'+dir5[j5]+'/'+dir6[j6]
                    IF file_test(dir,/directory) EQ 0 THEN BEGIN
                       file_mkdir,dir
                    ENDIF
                    ;; exists if so do not run the model again
                    IF (file_test(dir+'/dust.out') OR file_test(dir+'/dust.out.gz')) THEN BEGIN
                       message,/info,'Model already exists not redoing the work'
                    ENDIF ELSE BEGIN
                       ;; This is where we know enough to write the modust.inp
                       do25grid_modust,dir+'/'+'modust.inp', $
                                       L_star=L_star       [j1], $
                                       R_in  =R_in_R_star  [j2], $
                                       R_out =R_out_R_star [j3], $
                                       rho=   rho_cgs      [j4]
                       
                       do25grid_dust,dir+'/'+'dust.dat', $
                                     amin=amin[j5], $
                                     amax=amax[j6]
                       
                       ;; also write a line in the general batchfile
                       printf,lun_batch,'pushd '+dir+'; '+model_command+'popd;'
                       counter = counter+1
                       printf,lun_reading,'model'+i2s(counter,3)+'=read_modust3(distance='+f2s(distance,0)+',"'+dir+'/")'
                    ENDELSE
                 ENDFOR 
              ENDFOR
           ENDFOR
        ENDFOR
     ENDFOR
  ENDFOR
  
  close,lun_batch
  free_lun,lun_batch
  
  close,lun_reading
  free_lun,lun_reading
  
  spawn,'chmod 755 do_batch.csh'
  
  IF keyword_set(execute) THEN BEGIN
     spawn,'csh -f -c ./do_batch.csh'
     ;; stupid test to see if we are in IA
     IF strpos(!prompt,'IA') NE -1 THEN BEGIN
        print,'Type the following in IA to read the results;'
        print,'pushd,"'+rootdir+'"'
        print,'@readfiles.idl'
        print,'popd'
     ENDIF 
  ENDIF
  
  ;; and move back to the starting directory
  popd
  
END  
