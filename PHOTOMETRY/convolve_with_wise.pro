FUNCTION cww_integrate,w,f
  
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


function convolve_with_wise,p1,p2, $
                            wise_1=wise_1, $
                            wise_2=wise_2, $
                            wise_3=wise_3, $
                            wise_4=wise_4, $
                            K_1=K_1, $
                            K_2=K_2, $
                            K_3=K_3, $
                            K_4=K_4
  
  profilesdir=shell_expand('~/IA_FILES/FILTER_PROFILES/')
  
  if n_params() eq 2 then begin
     wave = p1
     f_nu_actual = p2
  endif else begin
     wave = p1.data.wave
     f_nu_actual = p1.data.flux
  endelse
  
  ;; http://lambda.gsfc.nasa.gov/product/wise/spectral_resp.cfm
  R_1 = read_fdat(profilesdir+'WISE_WISE.W1.dat')
  R_2 = read_fdat(profilesdir+'WISE_WISE.W2.dat')
  R_3 = read_fdat(profilesdir+'WISE_WISE.W3.dat')
  R_4 = read_fdat(profilesdir+'WISE_WISE.W4.dat')
  
  ;; to micron from angstrom
  R_1.data.wave  = R_1.data.wave*1d-4
  R_2.data.wave  = R_2.data.wave*1d-4
  R_3.data.wave  = R_3.data.wave*1d-4
  R_4.data.wave  = R_4.data.wave*1d-4

  lambda_0_1  = 3.353
  lambda_0_2  = 4.603 
  lambda_0_3  = 11.561
  lambda_0_4  = 22.088

  ;; make a fine log spaced wavelength grid
  min_wave = min([R_1.data.wave,R_2.data.wave,R_3.data.wave,R_4.data.wave],max=max_wave)
  npoints = 1001
  fine_wave  = min_wave*10d0^(alog10(max_wave/min_wave)*dindgen(npoints)/(npoints -1L))

  ;; resample on the fine grid
  fine_f_nu_actual = interpol(f_nu_actual,wave,fine_wave)
  fine_R_1   = interpol( R_1.data.flux, R_1.data.wave,fine_wave)
  fine_R_2   = interpol( R_2.data.flux, R_2.data.wave,fine_wave)
  fine_R_3   = interpol( R_3.data.flux, R_3.data.wave,fine_wave)
  fine_R_4  = interpol(R_4.data.flux,R_4.data.wave,fine_wave)

  ;; set to zero everything that is outside the input response function
  fine_R_1 [where(fine_wave lt min( R_1.data.wave) or fine_wave gt max( R_1.data.wave))]  = 0d0
  fine_R_2 [where(fine_wave lt min( R_2.data.wave) or fine_wave gt max( R_2.data.wave))]  = 0d0
  fine_R_3 [where(fine_wave lt min( R_3.data.wave) or fine_wave gt max( R_3.data.wave))]  = 0d0
  fine_R_4[where(fine_wave lt min(R_4.data.wave) or fine_wave gt max(R_4.data.wave))]  = 0d0

  f_nu_0_actual_1  = interpol(fine_f_nu_actual,fine_wave,lambda_0_1 )
  f_nu_0_actual_2  = interpol(fine_f_nu_actual,fine_wave,lambda_0_2 )
  f_nu_0_actual_3  = interpol(fine_f_nu_actual,fine_wave,lambda_0_3 )
  f_nu_0_actual_4 = interpol(fine_f_nu_actual,fine_wave,lambda_0_4)

  F_1  = f_nu_0_actual_1  * cww_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_1 )*fine_R_1 )
  F_2  = f_nu_0_actual_2  * cww_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_2 )*fine_R_2 )
  F_3  = f_nu_0_actual_3  * cww_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_3 )*fine_R_3 )
  F_4 = f_nu_0_actual_4 * cww_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_4)*fine_R_4)

  fine_f_nu_quoted_1  = fine_wave/lambda_0_1
  fine_f_nu_quoted_2  = fine_wave/lambda_0_2
  fine_f_nu_quoted_3  = fine_wave/lambda_0_3
  fine_f_nu_quoted_4 = fine_wave/lambda_0_4

  f_nu_0_quoted_1  = F_1  / cww_integrate(fine_wave,fine_f_nu_quoted_1 *fine_R_1 )
  f_nu_0_quoted_2  = F_2  / cww_integrate(fine_wave,fine_f_nu_quoted_2 *fine_R_2 )
  f_nu_0_quoted_3  = F_3  / cww_integrate(fine_wave,fine_f_nu_quoted_3 *fine_R_3 )
  f_nu_0_quoted_4 = F_4 / cww_integrate(fine_wave,fine_f_nu_quoted_4*fine_R_4)

  K_1  = f_nu_0_quoted_1 /f_nu_0_actual_1
  K_2  = f_nu_0_quoted_2 /f_nu_0_actual_2
  K_3  = f_nu_0_quoted_3 /f_nu_0_actual_3
  K_4 = f_nu_0_quoted_4/f_nu_0_actual_4

  WISE_1  = f_nu_0_quoted_1
  WISE_2  = f_nu_0_quoted_2
  WISE_3  = f_nu_0_quoted_3
  WISE_4 = f_nu_0_quoted_4

  return,transpose([[lambda_0_1,lambda_0_2,lambda_0_3,lambda_0_4],[WISE_1,WISE_2,WISE_3,WISE_4]])
END
