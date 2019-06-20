PRO wxgrid_modust,file, $
                  pmode=pmode, $
                  T_star=T_star, $
                  R_star=R_star, $
                  R_in=R_in, $
                  R_out=R_out, $
                  mdot=mdot, $
                  v_exp=v_exp, $
                  gas_to_dust=gas_to_dust
  
  openw,lun,file,/get_lun

  T_sun = 5780.
  L_STAR = R_STAR^2d0*(T_STAR/T_SUN)^(4.)
  
  printf,lun,'"MODE"'
  printf,lun,'     '+n2s(pmode)+'                              ! pmode'
  printf,lun,'"SOURCE:CENTRAL:BB"'
  printf,lun,' '+f2s(R_star,dec=1)+'    '+f2s(T_star,dec=1)+'            ! '+ $
         f2s(L_STAR,0)+' L_sun'
  printf,lun,'"SHELL:MASS LOSS"'
  printf,lun,' '+f2s(R_in,dec=1)+'  '+f2s(R_out,dec=1)+'                ! rmin, rmax '
  printf,lun,' '+f2s(mdot)+'   '+f2s(v_exp*1d-5,1)+'   '+f2s(v_exp*1d-5,1)+ $
         '       '+f2s(1d0/gas_to_dust,3)+'   ! mdot, vmin, vmax, fracdg'
  printf,lun,'"GRID SETUP"'
  printf,lun,'  15   35   115     2  1  1.0  2  8     ! ns,ni,nr,irmode,ipmode  np = ns+ni+nr'
  printf,lun,'0.20   2000.  0.040     0.  0.   0.0  0 ! wre_(min,max,step) wsp_(min,max,step)'
  printf,lun,' 100.  0.1                              ! tre, dtre_conv'
  printf,lun,'"1D-IMAGE"'
  printf,lun,'"CONVERGENCE"'
  printf,lun,' 40     1.e-3    0  0  0           0 0  ! it_REmax,epsconv,mode_LI,mode_LC,mode_FeaSmooth,dummys'
  printf,lun,'"EOF"'

  close,lun
  free_lun,lun

END

PRO wxgrid_dust,file,frac_fe=frac_fe,amin=amin,amax=amax
  
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
  printf,lun,'1     '+f2s(1d0-frac_fe,2)+'    '+f2s(alog10(amin),2)+ $
         '    '+f2s(alog10(amax),2)+'    11    -3.50    03.71    -1.0    15000.    0'
  printf,lun,'*'
  printf,lun,'** family: metals'
  printf,lun,'** latice structure: C'
  printf,lun,'** chem. comp.: fe07.87'
  printf,lun,'** ID: 020.lnk'
  printf,lun,'*'
  printf,lun,'"CROSS SECTIONS:MIE"'
  printf,lun,'/usr/local/iso/dbase2/metals/C/fe_07.87_0000_020.lnk'
  printf,lun,'1     '+f2s(frac_fe,2)+'    -2.30    -1.0    11    -3.50    07.87    -1.0    15000.    0'
  printf,lun,'EOF'

  close,lun
  free_lun,lun

END

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

PRO wxgrid, $
  L_star=L_star,T_star=T_star,R_star=R_star, $
  Mdot1=Mdot1,R_in1=R_in1,R_out1=R_out1,frac_fe1=frac_fe1,amin1=amin1,amax1=amax1, $
  Mdot2=Mdot2,R_in2=R_in2,R_out2=R_out2,frac_fe2=frac_fe2,amin2=amin2,amax2=amax2, $
  help=help,Rootdir=Rootdir,v_exp=v_exp,gas_to_dust=gas_to_dust, $
  distance=distance,execute=execute,offset=offset,clean=clean
  
  IF keyword_set(help) THEN BEGIN
      print,' PRO wxgrid, [options]'
      print,'--- options:----'

      print,'L_star      = luminosity of the central star in L_sun'
      print,'T_star      = temperature of the central star in K'
      print,'R_star      = temperature of the central star in R_sun'

      print,'Mdot1      = (array of) Mass-loss rate of shell 1 M_sun/yr'
      print,'R_in1      = (array of) Inner radius of shell 1 in Rstar'
      print,'R_out1     = (array of) Outer radius of shell 1 in Rstar'
      print,'frac_fe1   = (array of) mass fraction of Fe of shell 1'
      print,'amin1      = (array of) miniumum grainsize in shell 1 in mum'
      print,'amax1      = (array of) maximum grainsize in shell 1 in mum'
      
      print,'Mdot2      = (array of) Mass-loss rate of shell 2 M_sun/yr'
      print,'R_in2      = (array of) Inner radius of shell 2 in Rstar'
      print,'R_out2     = (array of) Outer radius of shell 2 in Rstar'
      print,'frac_fe2   = (array of) mass fraction of Fe of shell 2'
      print,'amin2      = (array of) miniumum grainsize in shell 2 in mum'
      print,'amax2      = (array of) maximum grainsize in shell 2 in mum'
      
      print,'gas_to_dust= Gas to dust mass ratios (100)'
      print,'v_exp       = The expansion velocity of the gas in km/s (20)'

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
  
  default,v_exp,20. ;; km/s
  ;; convert to cgs
  v_exp = 1d5*v_exp
  
  default,gas_to_dust,100. ;; in mass ratio
  
  ;; something about the star
  default,T_star,2500. ;; K
  nT_star = n_elements(T_star)

  ;; Be carefull here. In case R is derived from L and T then it is
  ;; not independent of T. Thus we have to discrimiate between
  ;; those R's that are going together with T_star and those
  ;; that should be varied independently. See below

  IF keyword_set(L_STAR) THEN BEGIN
      IF keyword_set(R_STAR) THEN BEGIN 
          message,/info,'R_STAR has been specified as well as L_STAR'
          message,/info,'ignoring R_STAR values'
      ENDIF 
      R_star = (L_star##(T_sun/T_star)^(4.))^(0.5)
      
      ;; In this case the number of independent R_STAR values is
      ;; derived from L_STAR
      nR_star = n_elements(L_star)
      R_STAR = reform(R_STAR,n_elements(R_STAR))
  ENDIF ELSE BEGIN
      IF keyword_set(R_STAR) THEN BEGIN 
          L_STAR = (R_STAR^2d0)#(T_STAR/T_SUN)^(4.)
          L_STAR = reform(L_STAR,n_elements(L_STAR))
          nR_star = n_elements(R_star)
      ENDIF ELSE BEGIN
          L_star=10000.
          R_star = (L_star*(T_sun/T_star)^(4.))^(0.5)
          nR_star = 1
      ENDELSE
  ENDELSE
  
  R_star_cm = R_star*R_sun
  
  ;;default values for the parameters
  default,Mdot1      ,[1e-5]
  default,R_in1      ,[10]
  default,R_out1     ,[500]
  default,frac_fe1   ,[0.1]
  default,amin1      ,[0.01]
  default,amax1      ,[1.0]
  default,Mdot2      ,[1e-6]
  default,R_in2      ,[2000]
  default,R_out2     ,[1e4]
  default,frac_fe2   ,[0.1]
  default,amin2      ,[0.01]
  default,amax2      ,[1.0]

  mdot1_cgs = mdot1*M_sun/sec_year ;; g/s
  mdot2_cgs = mdot2*M_sun/sec_year ;; g/s

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
  printf,lun,'Temperature ',T_star,' K'
  printf,lun,'Radius      ',reform(R_star,n_elements(R_star)),' R_sun = ', $
         reform(R_star_cm,n_elements(R_star_cm)),' cm'
  printf,lun,'Luminosity  ',L_star,' L_sun'

  printf,lun,'-----------------------------------------'
  printf,lun,'Outflow parameters   '
  printf,lun,'Gas to dust mass-ratio ',gas_to_dust
  printf,lun,'Expansion velocity',v_exp*1d-5,' km/s (20)'

  printf,lun,'-----------------------------------------'
  printf,lun,'Inner shell parameters'
  printf,lun,'Mass loss rate',Mdot1,' M_sun/yr'
  printf,lun,'Inner radius',R_in1,' R_star'
  printf,lun,'Outer radius',R_out1,' R_star'
  printf,lun,'Mass fraction of iron grains',frac_fe1
  printf,lun,'Minimum dust grain diameter',amin1,' mum'
  printf,lun,'Maximum dust grain diameter',amax1,' mum'

  printf,lun,'-----------------------------------------'
  printf,lun,'Outer shell parameters'
  printf,lun,'Mass loss rate',Mdot2,' M_sun/yr'
  printf,lun,'Inner radius',R_in2,' R_star'
  printf,lun,'Outer radius',R_out2,' R_star'
  printf,lun,'Mass fraction of iron grains',frac_fe2
  printf,lun,'Minimum dust grain diameter',amin2,' mum'
  printf,lun,'Maximum dust grain diameter',amax2,' mum'
  
  close,lun
  free_lun,lun

  ;; Something about the command to execute in the directory to
  ;; calculate the model
  default,model_command,'multishell201 do 1 do 2; '

  IF keyword_set(clean) THEN BEGIN
      ;; remove extra output files and compress the dust{,1,2}.out files
      model_command=model_command+'clean_modust; '
  ENDIF 

  openw,lun_batch,'do_batch.csh',/get_lun
  openw,lun_reading,'readfiles.idl',/get_lun,/append
  counter = offset

;; Make the dirnames ones here
  dir1  = make_array(n_elements(T_STAR),value='')
  dir2  = make_array(n_elements(R_STAR),value='')
  dir3  = make_array(n_elements(v_exp),value='')
  dir4  = make_array(n_elements(gas_to_dust),value='')
  dir5  = make_array(n_elements(Mdot1),value='')
  dir6  = make_array(n_elements(R_in1),value='')
  dir7  = make_array(n_elements(R_out1),value='')
  dir8  = make_array(n_elements(Mdot2),value='')
  dir9  = make_array(n_elements(R_in2),value='')
  dir10 = make_array(n_elements(R_out2),value='')
  dir11 = make_array(n_elements(frac_fe1),value='')
  dir12 = make_array(n_elements(amin1),value='')
  dir13 = make_array(n_elements(amax1),value='')
  dir14 = make_array(n_elements(frac_fe2),value='')
  dir15 = make_array(n_elements(amin2),value='')
  dir16 = make_array(n_elements(amax2),value='')
  FOR j1  = 0, n_elements(dir1 )-1 DO dir1 [j1 ] = 'T_STAR:'+f2s(T_STAR[j1],0)
  FOR j2  = 0, n_elements(dir2 )-1 DO dir2 [j2 ] = 'R_STAR:'+f2s(R_STAR[j2],1)
  FOR j3  = 0, n_elements(dir3 )-1 DO dir3 [j3 ] = 'V_EXP:'+f2s(v_exp[j3]*1d-5,1)
  FOR j4  = 0, n_elements(dir4 )-1 DO dir4 [j4 ] = 'GAS_TO_DUST:'+f2s(gas_to_dust[j4],3)
  FOR j5  = 0, n_elements(dir5 )-1 DO dir5 [j5 ] = 'MDOT1:'+f2s(Mdot1[j5])
  FOR j6  = 0, n_elements(dir6 )-1 DO dir6 [j6 ] = 'R_IN1:'+f2s(R_in1[j6],2)
  FOR j7  = 0, n_elements(dir7 )-1 DO dir7 [j7 ] = 'R_OUT1:'+f2s(R_out1[j7],2)
  FOR j8  = 0, n_elements(dir8 )-1 DO dir8 [j8 ] = 'MDOT2:'+f2s(Mdot2[j8])
  FOR j9  = 0, n_elements(dir9 )-1 DO dir9 [j9 ] = 'R_IN2:'+f2s(R_in2[j9],2)
  FOR j10 = 0, n_elements(dir10)-1 DO dir10[j10] = 'R_OUT2:'+f2s(R_out2[j10],2)
  FOR j11 = 0, n_elements(dir11)-1 DO dir11[j11] = 'FRAC_FE1:'+f2s(frac_fe1[j11])
  FOR j12 = 0, n_elements(dir12)-1 DO dir12[j12] = 'AMIN1:'+f2s(amin1[j12])
  FOR j13 = 0, n_elements(dir13)-1 DO dir13[j13] = 'AMAX1:'+f2s(amax1[j13])
  FOR j14 = 0, n_elements(dir14)-1 DO dir14[j14] = 'FRAC_FE2:'+f2s(frac_fe2[j14])
  FOR j15 = 0, n_elements(dir15)-1 DO dir15[j15] = 'AMIN2:'+f2s(amin2[j15])
  FOR j16 = 0, n_elements(dir16)-1 DO dir16[j16] = 'AMAX2:'+f2s(amax2[j16])
  

  ;; first we loop over the stellar parameters T_star, R_star
  ;; T_STAR

  FOR j1 = 0, n_elements(T_star)-1 DO BEGIN
      
   ;; R_STAR
   ;; Only loop over the independent r_star parameters which means:
   ;; r_star[j1+nT_star*j2]
   FOR j2 = 0, nR_star-1 DO BEGIN

    ;; Now do the 'outflow' params v_exp and gas_to_dust
    ;; v_exp
    FOR j3 = 0, n_elements(v_exp)-1 DO BEGIN
     
     ;; gas_to_dust
     FOR j4 = 0, n_elements(gas_to_dust)-1 DO BEGIN
      
      ;; The shell 1 and 2 params (Mdot[1,2],Rin[1,2],Rout[1,2]
      ;; Mdot1
      FOR j5 = 0, n_elements(Mdot1)-1 DO BEGIN
       
       ;; R_in1
       FOR j6 = 0, n_elements(R_in1)-1 DO BEGIN
        
        ;; R_out1
        FOR j7 = 0, n_elements(R_out1)-1 DO BEGIN

         ;; Mdot2
         FOR j8 = 0, n_elements(Mdot2)-1 DO BEGIN

          ;; R_in2
          FOR j9 = 0, n_elements(R_in2)-1 DO BEGIN
        
           ;; R_out2
           FOR j10 = 0, n_elements(R_out2)-1 DO BEGIN
         
            ;; The dust 1 and 2 params (frac_fe[1,2],amin[1,2],amax[1,2]
            ;; frac_fe1
            FOR j11 = 0, n_elements(frac_fe1)-1 DO BEGIN
             
             ;; amin1
             FOR j12 = 0, n_elements(amin1)-1 DO BEGIN
              
              ;; amax1
              FOR j13 = 0, n_elements(amax1)-1 DO BEGIN
               
               ;; frac_fe2
               FOR j14 = 0, n_elements(frac_fe2)-1 DO BEGIN
                
                ;; amin2
                FOR j15 = 0, n_elements(amin2)-1 DO BEGIN
                 
                 ;; amax2
                 FOR j16 = 0, n_elements(amax2)-1 DO BEGIN
                  
                     dir = dir1[j1]+'/'+dir2[j1+nT_star*j2]+'/'+dir3[j3]+'/'+dir4[j4]+'/'+dir5[j5]+'/'+dir6[j6]+'/'+dir7[j7]+'/'+dir8[j8]+'/'+dir9[j9]+'/'+dir10[j10]+'/'+dir11[j11]+'/'+dir12[j12]+'/'+dir13[j13]+'/'+dir14[j14]+'/'+dir15[j15]+'/'+dir16[j16]
                     IF file_test(dir,/directory) EQ 0 THEN BEGIN
                         file_mkdir,dir
                     ENDIF

                     ;; Now test to see if the output file already
                     ;; exists if so do not run the model again
                     IF (file_test(dir+'/dust.out') OR file_test(dir+'/dust.out.gz')) AND $
                       (file_test(dir+'/dust1.out') OR file_test(dir+'/dust1.out.gz')) AND $
                       (file_test(dir+'/dust2.out') OR file_test(dir+'/dust2.out.gz')) THEN BEGIN
                         
                         message,/info,'Model already exists not redoing the work'
                     ENDIF ELSE BEGIN
                         ;; And write the dust1.dat
                         ;; This is where we know enough to write the modust1.inp
                         wxgrid_modust,dir+'/'+'modust1.inp', $
                                       pmode=3, $
                                       T_star=T_star[j1], $
                                       R_star=R_star_cm[j1+nT_star*j2]/R_sun, $
                                       R_in=R_in1[j6], $
                                       R_out=R_out1[j7], $
                                       mdot=mdot1[j5], $
                                       v_exp=v_exp[j3], $
                                       gas_to_dust=gas_to_dust[j4]
                         
                         ;; And write the modust2.inp
                         wxgrid_modust,dir+'/'+'modust2.inp', $
                                       pmode=1, $
                                       T_star=T_star[j1], $
                                       R_star=R_star_cm[j1+nT_star*j2]/R_sun, $
                                       R_in=R_in2[j9], $
                                       R_out=R_out2[j10], $
                                       mdot=mdot2[j8], $
                                       v_exp=v_exp[j3], $
                                       gas_to_dust=gas_to_dust[j4]
                         
                         wxgrid_dust,dir+'/'+'dust1.dat', $
                                     frac_fe=frac_fe1[j11], $
                                     amin=amin1[j12], $
                                     amax=amax1[j13]
                         
                         ;; And write the dust2.dat
                         wxgrid_dust,dir+'/'+'dust2.dat', $
                                     frac_fe=frac_fe2[j14], $
                                     amin=amin2[j15], $
                                     amax=amax2[j16]
                         
                         ;; also write a line in the general batchfile
                         printf,lun_batch,'pushd '+dir+'; '+model_command+'popd;'
                         counter = counter+1
                         printf,lun_reading,'model'+i2s(counter,3)+'=read_multi(distance='+f2s(distance,0)+',"' $
                                +dir+'/")'
                     ENDELSE
                 ENDFOR 
                ENDFOR
               ENDFOR
              ENDFOR
             ENDFOR
            ENDFOR
           ENDFOR
          ENDFOR
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
