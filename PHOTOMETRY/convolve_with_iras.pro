FUNCTION cwi_integrate,w,f
  
  x=1d0/w
  y=f

  idx = sort(x)
  x = x[idx]
  y  = y[idx] 

  npoints=n_elements(x)
  intval=fltarr(npoints)
    
  intval[1:npoints-2]=0.5*(x[2:npoints-1]-x[0:npoints-3])
  intval[0]=0.25*(x[1]-x[0])
  intval[npoints-1]=0.25*(x[npoints-1]-x[npoints-2])

  return,total(y*intval)

END


function convolve_with_iras,p1,p2, $
                            iras_12=iras_12, $
                            iras_25=iras_25, $
                            iras_60=iras_60, $
                            iras_100=iras_100, $
                            K_12=K_12, $
                            K_25=K_25, $
                            K_60=K_60, $
                            K_100=K_100
  
  profilesdir=shell_expand('~/IA_FILES/FILTER_PROFILES/')
  
  if n_params() eq 2 then begin
     wave = p1
     f_nu_actual = p2
  endif else begin
     wave = p1.data.wave
     f_nu_actual = p1.data.flux
  endelse
  
  ;; http://lambda.gsfc.nasa.gov/product/iras/spectral_resp.cfm
  R_12  = read_fdat(profilesdir+'iras12.dat')
  R_25  = read_fdat(profilesdir+'iras25.dat')
  R_60  = read_fdat(profilesdir+'iras60.dat')
  R_100 = read_fdat(profilesdir+'iras100.dat')
  
  lambda_0_12  = 11.991698d0
  lambda_0_25  = 24.982705d0
  lambda_0_60  = 59.958492d0
  lambda_0_100 = 99.930819d0

  ;; make a fine log spaced wavelength grid
  min_wave = min([R_12.data.wave,R_25.data.wave,R_60.data.wave,R_100.data.wave],max=max_wave)
  npoints = 1001
  fine_wave  = min_wave*10d0^(alog10(max_wave/min_wave)*dindgen(npoints)/(npoints -1L))

  ;; resample on the fine grid
  fine_f_nu_actual = interpol(f_nu_actual,wave,fine_wave)
  fine_R_12   = interpol( R_12.data.flux, R_12.data.wave,fine_wave)
  fine_R_25   = interpol( R_25.data.flux, R_25.data.wave,fine_wave)
  fine_R_60   = interpol( R_60.data.flux, R_60.data.wave,fine_wave)
  fine_R_100  = interpol(R_100.data.flux,R_100.data.wave,fine_wave)

  ;; set to zero everything that is outside the input response function
  fine_R_12 [where(fine_wave lt min( R_12.data.wave) or fine_wave gt max( R_12.data.wave))]  = 0d0
  fine_R_25 [where(fine_wave lt min( R_25.data.wave) or fine_wave gt max( R_25.data.wave))]  = 0d0
  fine_R_60 [where(fine_wave lt min( R_60.data.wave) or fine_wave gt max( R_60.data.wave))]  = 0d0
  fine_R_100[where(fine_wave lt min(R_100.data.wave) or fine_wave gt max(R_100.data.wave))]  = 0d0

  f_nu_0_actual_12  = interpol(fine_f_nu_actual,fine_wave,lambda_0_12 )
  f_nu_0_actual_25  = interpol(fine_f_nu_actual,fine_wave,lambda_0_25 )
  f_nu_0_actual_60  = interpol(fine_f_nu_actual,fine_wave,lambda_0_60 )
  f_nu_0_actual_100 = interpol(fine_f_nu_actual,fine_wave,lambda_0_100)

  F_12  = f_nu_0_actual_12  * cwi_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_12 )*fine_R_12 )
  F_25  = f_nu_0_actual_25  * cwi_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_25 )*fine_R_25 )
  F_60  = f_nu_0_actual_60  * cwi_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_60 )*fine_R_60 )
  F_100 = f_nu_0_actual_100 * cwi_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_100)*fine_R_100)

  fine_f_nu_quoted_12  = fine_wave/lambda_0_12
  fine_f_nu_quoted_25  = fine_wave/lambda_0_25
  fine_f_nu_quoted_60  = fine_wave/lambda_0_60
  fine_f_nu_quoted_100 = fine_wave/lambda_0_100

  f_nu_0_quoted_12  = F_12  / cwi_integrate(fine_wave,fine_f_nu_quoted_12 *fine_R_12 )
  f_nu_0_quoted_25  = F_25  / cwi_integrate(fine_wave,fine_f_nu_quoted_25 *fine_R_25 )
  f_nu_0_quoted_60  = F_60  / cwi_integrate(fine_wave,fine_f_nu_quoted_60 *fine_R_60 )
  f_nu_0_quoted_100 = F_100 / cwi_integrate(fine_wave,fine_f_nu_quoted_100*fine_R_100)

  K_12  = f_nu_0_quoted_12 /f_nu_0_actual_12
  K_25  = f_nu_0_quoted_25 /f_nu_0_actual_25
  K_60  = f_nu_0_quoted_60 /f_nu_0_actual_60
  K_100 = f_nu_0_quoted_100/f_nu_0_actual_100

  IRAS_12  = f_nu_0_quoted_12
  IRAS_25  = f_nu_0_quoted_25
  IRAS_60  = f_nu_0_quoted_60
  IRAS_100 = f_nu_0_quoted_100

  return,transpose([[lambda_0_12,lambda_0_25,lambda_0_60,lambda_0_100],[IRAS_12,IRAS_25,IRAS_60,IRAS_100]])
END
