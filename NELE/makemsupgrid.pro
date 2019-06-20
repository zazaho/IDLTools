PRO makemsupgrid_modust,file, $
                  T_star=T_star, $
                  R_star=R_star, $
                  R_in=R_in, $
                  R_out=R_out, $
                  mdot=mdot
  
  openw,lun,file,/get_lun
  
  T_SUN = 5780.
  L_STAR = R_STAR^2d0 * (T_STAR/T_SUN)^(4d0)
  
  printf,lun,'"MODE"'
  printf,lun,'     3                              ! pmode'
  printf,lun,'"SOURCE:CENTRAL:MSUPER:'+n2s(round(T_STAR))+'"'
  printf,lun,' '+f2s(R_star,dec=1)+'            ! '+  f2s(L_STAR,0)+' L_sun'
  printf,lun,'"SHELL:MASS LOSS"'
  printf,lun,' '+f2s(R_in,dec=1)+'  '+f2s(R_out,dec=1)+'                ! rmin, rmax '
  printf,lun,' '+f2s(mdot)+'   10.0   10.0       0.01   ! mdot, vmin, vmax, fracdg'
  printf,lun,'"GRID SETUP"'
  printf,lun,'  15   35   115     2  1  1.0  2  8     ! ns,ni,nr,irmode,ipmode  np = ns+ni+nr'
  printf,lun,'0.20   2000.  0.040     2.  45.   0.3  0 ! wre_(min,max,step) wsp_(min,max,step)'
  printf,lun,' 100.  0.1                              ! tre, dtre_conv'
  printf,lun,'"1D-IMAGE"'
  printf,lun,'"CONVERGENCE"'
  printf,lun,' 70     1.e-3    0  0  0           0 0  ! it_REmax,epsconv,mode_LI,mode_LC,mode_FeaSmooth,dummys'
  printf,lun,'"EOF"'
  
  close,lun
  free_lun,lun
  
END

PRO makemsupgrid_dust,file, $
                      frac_melilite=frac_melilite, $
                      frac_olivine=frac_olivine, $
                      frac_mgfeo=frac_mgfeo, $
                      frac_alo=frac_alo, $
                      prefix=prefix

  default,prefix,'/home/nele/thesispcklas'
  
  
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
  printf,lun,'** family: alumino-silicates'
  printf,lun,'** latice structure: A'
  printf,lun,'** chem. comp.: ca2al2sio7'
  printf,lun,'** ID: 001.lnk'
  printf,lun,'*'
  printf,lun,'"CROSS SECTIONS:MIE"'
  printf,lun,prefix+'/MoDust_2.00/data/dbase2/alumino-silicates/A/ca2al2sio7_02.91_0000_001.lnk'
  printf,lun,'1     '+f2s(frac_melilite,2)+'    -2.00     0.0    7    -3.50    02.91    -1.0    0    0'
  printf,lun,'*'
  printf,lun,'** family: olivine'
  printf,lun,'** latice structure: A'
  printf,lun,'** chem. comp.: mg0.8fe1.2'
  printf,lun,'** ID: 001.lnk'
  printf,lun,'*'
  printf,lun,'"CROSS SECTIONS:MIE"'
  printf,lun,prefix+'/MoDust_2.00/data/dbase2/olivine/A/mg0.8fe1.2_03.75_0000_001.lnk'
  printf,lun,'1     '+f2s(frac_olivine,2)+'    -2.00     0.0    7    -3.50    03.75    -1.0    0    0'
  printf,lun,'*'
  printf,lun,'** family: oxides'
  printf,lun,'** latice structure: C'
  printf,lun,'** chem. comp.: mg0.1fe0.9'
  printf,lun,'** ID: 001.lnk'
  printf,lun,'*'
  printf,lun,'"CROSS SECTIONS:MIE"'
  printf,lun,prefix+'/MoDust_2.00/data/dbase2/oxides/C/mg0.1fe0.9_05.46_0000_001.lnk'
  printf,lun,'1     '+f2s(frac_mgfeo,2)+'    -2.00     0.0    7    -3.50    05.46    -1.0    0    0'
  printf,lun,'*'
  printf,lun,'** family: oxides'
  printf,lun,'** latice structure: C'
  printf,lun,'** chem. comp.: al2o3'
  printf,lun,'** ID: 001.lnk'
  printf,lun,'*'
  printf,lun,'"CROSS SECTIONS:MIE"'
  printf,lun,prefix+'/MoDust_2.00/data/dbase2/oxides/C/al2o3_04.00_0000_002.lnk'
  printf,lun,'1     '+f2s(frac_alo,2)+'    -2.00     0.0    7    -3.50    04.00    -1.0    0    0'
  printf,lun,'EOF'

  close,lun
  free_lun,lun

END

;; Dit is hoe we het aan kunnen roepen:
;; makemsupgrid, L_star=[.5d5,1d5],/execute

PRO makemsupgrid, $
   L_star=L_star, $
   T_star=T_star, $
   Mdot=Mdot, $
   R_in=R_in, $
   R_out=R_out, $
   frac_melilite=frac_melilite, $
   frac_olivine=frac_olivine, $
   frac_mgfeo=frac_mgfeo, $
   distance=distance, $
   prefix=prefix, $
   Rootdir=Rootdir, $
   execute=execute, $
   offset=offset, $
   clean=clean, $
   help=help
  
  IF keyword_set(help) THEN BEGIN
     print,' PRO makemsupgrid, [options]'
     print,'--- options:----'
     
     print,'L_star        = luminosity of the central star in L_sun'
     print,'T_star        = temperature of the central star in K'
     
     print,'Mdot          = (array of) Mass-loss rate in M_sun/yr'
     print,'R_in          = (array of) Inner radius in Rstar'
     print,'R_out         = (array of) Outer radius in Rstar'
     
     print,'frac_melilite = (array of) mass fraction of melilite'
     print,'frac_olivine  = (array of) mass fraction of amorphous olivine'
     print,'frac_mgfeo    = (array of) mass fraction of MgFe-Oxide'
     print,'The mass fraction of Al2O3 is whatever is (1-frac_melilite-frac_olivine+frac_mgfeo)'
     
     print,'Rootdir    = name of the directory to put the grid in'
     print,'Distance   = The distance to assume when reading the files'
     print,'Offset     = First number to use when reading the files (modelXXX)'
     print,'Execute      If set the run the grid after generating it'
     print,'Clean        If set remove extra modust_output files after running it'
     print,'help         If set show this help'
     
     return
  ENDIF 
  
  default,distance,1000.
  default,offset,0

  ;;;; Constants
  ;; cgs units
  c = 2.998d10
  G =  6.67259d-8

  R_sun = 6.96d10
  M_sun = 1.99d33
  T_sun = 5780.
  sec_year = 365.*24.*3600.
  
  ;; something about the star
  default,T_star,[3000.] ;; K
  default,L_star,[1d5]   ;; L_sun 
  R_star = [sqrt(L_star)*(T_sun/T_star)^(2d0)]
  R_star_cm = R_star*R_sun

  ;;default values for the parameters
  default,Mdot            ,[1e-6]
  default,R_in            ,[10]
  default,R_out           ,[1000]
  default,frac_melilite   ,[0.1]
  default,frac_mgfeo      ,[0.1]
  default,frac_olivine    ,[0.2]
  ;; This implies 0.6 of Al2O3

  mdot_cgs = mdot*M_sun/sec_year ;; g/s

  ;; create a new subdir to store the tree
  IF NOT keyword_set(rootdir) THEN BEGIN
     suffix = 1
     WHILE file_test('GRID_'+i2s(suffix,2)) DO BEGIN
        suffix = suffix+1
     ENDWHILE
     rootdir = 'GRID_'+i2s(suffix,2)
  ENDIF 
  
  ;; Something about the command to execute in the directory to
  ;; calculate the model
  default,model_command,'modust; '
  default,prefix,'/home/nele/thesispcklas'

  IF keyword_set(clean) THEN BEGIN
      ;; remove extra output files and compress the dust{,1,2}.out files
      model_command=model_command+'clean_modust; '
  ENDIF 

  ;; move into this directory
  file_mkdir,rootdir
  pushd,rootdir
  
;; Now make some administration to store the grid parameters
  openw,lun,'grid_parameters.txt',/get_lun
  printf,lun,'Stellar parameters'
  printf,lun,'Temperature ',T_star,' K'
  printf,lun,'Radius      ',reform(R_star,n_elements(R_star)),' R_sun = ', $
         reform(R_star_cm,n_elements(R_star_cm)),' cm'
  
  printf,lun,'-----------------------------------------'
  printf,lun,'Inner shell parameters'
  printf,lun,'Mass loss rate',Mdot,' M_sun/yr'
  printf,lun,'Inner radius',R_in,' R_star'
  printf,lun,'Outer radius',R_out,' R_star'
  printf,lun,'Mass fraction of melilite grains',frac_melilite
  printf,lun,'Mass fraction of olivine  grains',frac_olivine
  printf,lun,'Mass fraction of MgFeO    grains',frac_mgfeo
  printf,lun,'Mass fraction of Al2O3    grains rest'
  
  close,lun
  free_lun,lun
  
  openw,lun_batch,'do_batch.csh',/get_lun
  openw,lun_reading,'readfiles.idl',/get_lun,/append
  counter = offset

;; Make the dirnames ones here
  dir1  = make_array(n_elements(T_STAR),value='')
  dir2  = make_array(n_elements(L_STAR),value='')
  dir3  = make_array(n_elements(Mdot),value='')
  dir4  = make_array(n_elements(R_in),value='')
  dir5  = make_array(n_elements(R_out),value='')
  dir6  = make_array(n_elements(frac_melilite),value='')
  dir7  = make_array(n_elements(frac_olivine),value='')
  dir8  = make_array(n_elements(frac_mgfeo),value='')

  FOR j1  = 0, n_elements(dir1 )-1 DO dir1 [j1 ] = 'T_STAR:'+f2s(T_STAR[j1],0)
  FOR j2  = 0, n_elements(dir2 )-1 DO dir2 [j2 ] = 'L_STAR:'+f2s(L_STAR[j2],1)
  FOR j3  = 0, n_elements(dir3 )-1 DO dir3 [j3 ] = 'MDOT:'+f2s(Mdot[j3])
  FOR j4  = 0, n_elements(dir4 )-1 DO dir4 [j4 ] = 'R_IN:'+f2s(R_in[j4],2)
  FOR j5  = 0, n_elements(dir5 )-1 DO dir5 [j5 ] = 'R_OUT:'+f2s(R_out[j5],2)
  FOR j6  = 0, n_elements(dir6 )-1 DO dir6 [j6 ] = 'FRAC_MELILITE:'+f2s(frac_melilite[j6])
  FOR j7  = 0, n_elements(dir7 )-1 DO dir7 [j7 ] = 'FRAC_OLIVINE:'+f2s(frac_olivine[j7])
  FOR j8  = 0, n_elements(dir8 )-1 DO dir8 [j8 ] = 'FRAC_MGFEO:'+f2s(frac_mgfeo[j8])

  ;; first we loop over the stellar parameters T_star, L_star
  ;; T_STAR
  
  FOR j1 = 0, n_elements(T_star)-1 DO BEGIN
     
     ;; R_STAR
     FOR j2 = 0, n_elements(L_star)-1 DO BEGIN
        
        FOR j3 = 0, n_elements(Mdot)-1 DO BEGIN
           
           ;; R_in
           FOR j4 = 0, n_elements(R_in)-1 DO BEGIN
              
              ;; R_out
              FOR j5 = 0, n_elements(R_out)-1 DO BEGIN
                 
                 ;; frac_melilite
                 FOR j6 = 0, n_elements(frac_melilite)-1 DO BEGIN
                    
                    ;; frac_olivne
                    FOR j7  = 0, n_elements(frac_olivine)-1 DO BEGIN
                       
                       ;; frac_mgfeo
                       FOR j8  = 0, n_elements(frac_mgfeo)-1 DO BEGIN
                          
                          ;; First test if the fractions dont exceed 1
                          IF (frac_melilite[j6] + frac_olivine[j7] + frac_mgfeo[j8]) LE 1d0 THEN BEGIN
                             frac_alo = 1d0 - (frac_melilite[j6] + frac_olivine[j7] + frac_mgfeo[j8])
                             
                             dir = dir1[j1]+'/'+dir2[j2]+'/'+dir3[j3]+'/'+dir4[j4]+'/'+dir5[j5]+'/'+dir6[j6]+'/'+dir7[j7]+'/'+dir8[j8]

                             IF file_test(dir,/directory) EQ 0 THEN BEGIN
                                file_mkdir,dir
                             ENDIF
                          
                             ;; Now test to see if the output file already
                             ;; exists if so do not run the model again
                             IF (file_test(dir+'/dust.out') OR file_test(dir+'/dust.out.gz')) THEN BEGIN
                                message,/info,'Model already exists not redoing the work'
                             ENDIF ELSE BEGIN
                                ;; And write the dust.dat and modust.inp
                                ;; This is where we know enough to write the modust.inp
                                makemsupgrid_modust,dir+'/'+'modust.inp', $
                                                    T_star=T_star[j1], $
                                                    R_star=R_star_cm[j2]/R_sun, $
                                                    mdot=mdot[j3], $
                                                    R_in=R_in[j4], $
                                                    R_out=R_out[j5]
                             
                                makemsupgrid_dust,dir+'/'+'dust.dat', $
                                                  frac_melilite=frac_melilite[j6], $
                                                  frac_olivine=frac_olivine[j7], $
                                                  frac_mgfeo=frac_mgfeo[j8], $
                                                  frac_alo=frac_alo, $ $
                                                  prefix=prefix
                         
                                ;; also write a line in the general batchfile
                                printf,lun_batch,'pushd '+dir+'; '+model_command+'popd;'
                                counter = counter+1
                                printf,lun_reading,'model'+i2s(counter,3)+'=read_modust3(distance='+f2s(distance,0)+',"' $
                                       +dir+'/")'
                             ENDELSE
                          ENDIF
                       ENDFOR 
                    ENDFOR
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
      spawn,'sh -f -c ./do_batch.csh'
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
