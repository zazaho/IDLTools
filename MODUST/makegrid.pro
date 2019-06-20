;; function to create a tree of subdirectories to execute a grid of
;; modust models with a range of parameters. The model consists of an
;; expanding dust shell around a 2200~K BB star with L=7000 L_sun
;;
;; We use a standard expansion velocity of 15 km/s
;; options are:
;; time  = [T1,T2,...,TX] with TX in years
;; mdot  = [M1,M2,...,MX] with MX in 10^{-5} M_sun/year
;; deltaT = [DT1,...DTX] with DTX in year
;;
;; The dust composition parameters are
;; amin = [AL1,...,ALX] with ALX in micrometer
;; amax = [AH1,...,AHX] with AHX in micrometer
;; frac_MgS = [MgS1,...,MgSX] in percent

PRO makegrid, $
              time=time,mdot=mdot,deltaT=deltaT,amin=amin, $
              amax=amax,frac_MgS=frac_MgS, $
              L_star=L_star,T_star=T_star,exp_vel=exp_vel, $
              help=help, $
              deltaR=deltaR,Mshell=Mshell,Rootdir=Rootdir, $
              gas_to_dust=gas_to_dust,R_in=R_in

  IF keyword_set(help) THEN BEGIN
      print,' PRO makegrid, [options]'
      print,'--- options:----'
      print,'time     = (array of) time value(s) after leaving the star in year'
      print,'mdot     = (array of) mdot value(s) in 10^-5 M_sun/year'
      print,'deltaT   = (array of) burst length value(s) in year'
      print,'amin     = (array of) smallest a-C size(s) in micrometer'
      print,'amax     = (array of) largest a-C size(s) in micrometer'
      print,'frac_MgS = (array of) fractions of MgS in percent'
      print,'gas_dust = gas to dust mass ratio (default 220)'
      print,'---- other options: -----'
      print,'mshell   = (array of) integrated shell mass values in  10^-5 M_sun'
      print,'deltaR   = (array of) shell thickness value(s) cm'
      print,'R_in     = (array of) values of the inner edge in cm'
      print,'Rootdir  = name of the directory to put the grid in'
      print,'L_star   = luminosity of the central star in L_sun'
      print,'T_star   = temperature of the central star in K'
      return
  ENDIF 

  ;; cgs units
  c = 2.998d10
  G =  6.67259d-8

  R_sun = 6.96d10
  M_sun = 1.99d33
  T_sun = 5780.
  sec_year = 365.*24.*3600.
  
  ;; convert to cgs
  default,exp_vel,15.
  exp_vel = 1d5*exp_vel
  
  default,gas_to_dust,220.
  
  ;; something about the star
  default,L_star,7000.
  default,T_star,2200.
  R_star = (L_star*(T_sun/T_star)^(4.))^(0.5)
  R_star_cm = R_star*R_sun
  
  ;;default values for the parameters
  default,time,[100,200,400,800,1500,4000,10000] ;; year
  ;; are the R_im values specified then calculate the corresponding times
  IF keyword_set(R_in) THEN BEGIN
      time = (R_in - R_star_cm) /exp_vel/sec_year
  ENDIF 
  
  default,deltaT, 200 ;; year
  ;; are the thicknesses specified then calculate the corresponding durations
  IF keyword_set(deltaR) THEN BEGIN
      deltaT = deltaR/exp_vel/sec_year
  ENDIF 
  
  default,mdot,1 ;; 10^{-5} M_sun/year
  ;; are the shells masses spcified then calculate corresponding mdot
  ;; we calculate this value for the first time given in time
  IF keyword_set(mshell) THEN BEGIN
      mdot = mshell/deltaT[0]
  ENDIF
  mdot_cgs = mdot*1d-5*M_sun/sec_year ;; g/s

  default,amin,0.01 ;; micrometer
  default,amax,1.0  ;; micrometer
  default,frac_MgS,4 ;; percent

  ;; create a new subdir to store the tree
  IF NOT keyword_set(rootdir) THEN BEGIN
      suffix = 1
      WHILE file_test('GRID_'+i2s(suffix,2)) DO BEGIN
          suffix = suffix+1
      ENDWHILE
      rootdir = 'GRID_'+i2s(suffix,2)
  ENDIF 
  
  spawn,'mkdir '+rootdir
  pushd,rootdir
  
  openw,lun_params,'grid_parameters.txt',/get_lun
  printf,lun_params,'Stellar parameters'
  printf,lun_params,'Temperature ',T_star,' K'
  printf,lun_params,'Radius      ',R_star,' R_sun = ',R_star_cm,' cm'
  printf,lun_params,'Luminosity  ',L_star,' L_sun'

  printf,lun_params,'-----------------------------------------'
  printf,lun_params,'Shell parameters   '
  printf,lun_params,'Expansion velocity ',exp_vel*1d-5,' km/s'
  printf,lun_params,'Times after burst (year):'
  printf,lun_params,time
  printf,lun_params,'Inner edges (cm):'
  printf,lun_params,R_star_cm + exp_vel*sec_year*time
  printf,lun_params,'Inner edges (R_star):'
  printf,lun_params,(R_star_cm + exp_vel*sec_year*time)/R_star_cm
  printf,lun_params,'Burst duration (year):'
  printf,lun_params,deltaT
  printf,lun_params,'Shell thickness (cm):'
  printf,lun_params,deltaT*exp_vel*sec_year
  printf,lun_params,'Shell thickness (R_Star):'
  printf,lun_params,deltaT*exp_vel*sec_year/R_star_cm
  printf,lun_params,'M_dot (M_sun/year):'
  printf,lun_params,mdot*1d-5
  printf,lun_params,'Shell mass (M_sun):'
  printf,lun_params,deltaT#(mdot*1d-5)

  printf,lun_params,'-----------------------------------------'
  printf,lun_params,'Dust parameters'
  printf,lun_params,'gas-to-dust ratio:   ',gas_to_dust
  printf,lun_params,'Mass fraction of MgS:'
  printf,lun_params, frac_MgS/100.
  printf,lun_params,'Smallest a-C grainsize (um):'
  printf,lun_params,amin
  printf,lun_params,'Largest  a-C grainsize (um):'
  printf,lun_params,amax

  close,lun_params
  free_lun,lun_params
  
  openw,lun_batch,'do_batch.csh',/get_lun
  openw,lun_reading,'readfiles.idl',/get_lun
  counter = 0
  
  ;; first we loop over the shell parameters mdot,deltaT and time
  FOR j1 = 0, n_elements(mdot)-1 DO BEGIN
      dir_j1 = 'MDOT_'+f2s(mdot[j1],3)
      spawn,'mkdir '+dir_j1
      pushd,dir_j1
      FOR j2 = 0, n_elements(deltaT)-1 DO BEGIN
          dir_j2 = 'DELTAT_'+i2s(deltaT[j2],4)
          spawn,'mkdir '+dir_j2
          pushd,dir_j2
          FOR j3 = 0, n_elements(time)-1 DO BEGIN
              dir_j3 = 'TIME_'+i2s(time[j3],5)
              spawn,'mkdir '+dir_j3
              pushd,dir_j3
              
              ;; calc some shit
              R_min_cm = R_star_cm + exp_vel*sec_year*time[j3]
              R_max_cm = R_min_cm + exp_vel*sec_year*deltaT[j2]
              
              R_min = R_min_cm / R_star_cm
              R_max = R_max_cm / R_star_cm
              
              rho_in = mdot_cgs[j1]/gas_to_dust / ( 4.*!dpi* R_min_cm^2d0*exp_vel)
              
              openw,lun,'modust.inp',/get_lun
              
              printf,lun,'"MODE"'
              printf,lun,'     3                              ! pmode'
              printf,lun,'"SOURCE:CENTRAL:BB"'
              printf,lun,' '+f2s(R_star,dec=1)+'    '+f2s(T_star,dec=1)+'            ! 7000 L_sun'
              printf,lun,'"SHELL:POWER LAW"'
              printf,lun,' '+f2s(R_min,dec=1)+'  '+f2s(R_max,dec=1)+'                ! rmin, rmax 10467.8 = 1arcsec'
              printf,lun,' '+string(rho_in)+'   -2.0   1.0               ! rhomin, mrho, fracdg'
              printf,lun,'"GRID SETUP"'
              printf,lun,'  4   10   100     1  1 1.0 2  8   ! ns,ni,nr,irmode,ipmode  np = ns+ni+nr'
              printf,lun,'0.2   1000.  0.1  2.  45.  0.5     ! wre_(min,max,step) wsp_(min,max,step)'
              printf,lun,' 100.  0.1                          ! tre, dtre_conv'
              printf,lun,'"CONVERGENCE"'
              printf,lun,'10 0.001 1 0 0 0 0                  ! it_REmax,epsconv,mode_LI,mode_LC,mode_FeaSmooth,mode_dum,mod_dum'
              printf,lun,'"IMAGE"'
              printf,lun,'"EOF"'
              
              close,lun
              free_lun,lun
              
              ;; now we loop over the dust parameters frac_MgS, amin
              ;; and amax
              FOR j4 = 0, n_elements(frac_MgS)-1 DO BEGIN
                  dir_j4 = 'MGS_'+i2s(frac_MgS[j4],3)
                  spawn,'mkdir '+dir_j4
                  pushd,dir_j4
                  FOR j5 = 0, n_elements(amin)-1 DO BEGIN
                      dir_j5 = 'AMIN_'+f2s(amin[j5],dec=3)
                      spawn,'mkdir '+dir_j5
                      pushd,dir_j5
                      FOR j6 = 0, n_elements(amax)-1 DO BEGIN
                          dir_j6 = 'AMAX_'+f2s(amax[j6],dec=3)
                          spawn,'mkdir '+dir_j6
                          pushd,dir_j6
              
                          ;; calc some stuff
                          fMgS = frac_MgS[j4]*1d-2
                          fCarb = 1.0 - fMgS
                          log_amin = alog10(amin[j5])
                          log_amax = alog10(amax[j6])
                          
                          ;; put the specfic dust.dat file here
                          openw,lun,'dust.dat',/get_lun
                          
                          ;; write the file
                          printf,lun,'* Input file for radiative transfer program'
                          printf,lun,'* listed are the names of the dust types and'
                          printf,lun,'* on/of switch, mass fraction, minimum grain size maximum'
                          printf,lun,'* grain size, grain size interval in dex , the power of the grain'
                          printf,lun,'* size distribution, the density'
                          printf,lun,'* of the grain material and the far IR efficiency law:'
                          printf,lun,'*'
                          printf,lun,'* SELECT  ABUNDANCE  AMIN  AMAX  NA  MA  RHO  ALPHA'
                          printf,lun,'*'
                          printf,lun,'** family: carbon'
                          printf,lun,'** latice structure: A'
                          printf,lun,'** chem. comp.: c'
                          printf,lun,'** ID: 001.lnk'
                          printf,lun,'*'
                          printf,lun,'"CROSS SECTIONS:MIE"'
                          printf,lun,'/usr/local/iso/dbase2/carbon/A/c_01.80_0000_001.lnk'
                          printf,lun,'1     '+f2s(fCarb,dec=3)+'    '+f2s(log_amin,dec=3)+'    '+f2s(log_amax,dec=3)+'    6    -3.50    01.80    -1.0    0    0'
                          printf,lun,'*'
                          printf,lun,'** family: sulfides'
                          printf,lun,'** latice structure: C'
                          printf,lun,'** chem. comp.: MgS'
                          printf,lun,'** ID: 003.lnk'
                          printf,lun,'*'
                          printf,lun,'"CROSS SECTIONS:FROM TABLE"'
                          printf,lun,'/home/sacha/d1/ISO/PROJECTS/HD56126/DATA/MODUST/MATERIALS/MGS/mgs_fudge_r0.01.q'
                          printf,lun,'1      '+f2s(fMgS,dec=3)+'    -2.00    -2.00    1    -3.50    03.00    -1.0    0    0'
                          close,lun
                          free_lun,lun
                          
                          ;; now link to the modust.inp 3 dirs up
                          spawn,'ln -s ../../../modust.inp ./'
                          ;; also write a line in the general batchfile
                          printf,lun_batch,'cd '+dir_j1+'/'+dir_j2+'/'+dir_j3+'/'+dir_j4+'/'+dir_j5+'/'+dir_j6+'/'
                          printf,lun_batch,'modust'
                          printf,lun_batch,'cd ../../../../../../'
                          counter = counter+1
                          printf,lun_reading,'model'+i2s(counter,3)+'=read_modust("'+dir_j1+'/'+dir_j2+'/'+dir_j3+'/'+dir_j4+'/'+dir_j5+'/'+dir_j6+'/dust.idl")'
                          popd
                      ENDFOR
                      popd
                  ENDFOR
                  popd
              ENDFOR
              popd
          ENDFOR
          popd
      ENDFOR
      popd
  ENDFOR
  close,lun_batch
  free_lun,lun_batch
  close,lun_reading
  free_lun,lun_reading
  spawn,'chmod 755 do_batch.csh'
  popd
END  
