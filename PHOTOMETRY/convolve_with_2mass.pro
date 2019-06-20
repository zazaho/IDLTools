FUNCTION cw2_integrate,w,f
  
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

function convolve_with_2mass,p1,p2, $
                            2mass_j=2mass_j, $
                            2mass_h=2mass_h, $
                            2mass_ks=2mass_ks, $
                            K_j=K_j, $
                            K_h=K_h, $
                            K_ks=K_ks
  

  profilesdir=shell_expand('~/IA_FILES/FILTER_PROFILES/')
  
  if n_params() eq 2 then begin
     wave = p1
     f_nu_actual = p2
  endif else begin
     wave = p1.data.wave
     f_nu_actual = p1.data.flux
  endelse

  ;; http://svo2.cab.inta-csic.es/theory/fps
  R_j = read_fdat(profilesdir+'2MASS_2MASS.J.dat')
  R_h = read_fdat(profilesdir+'2MASS_2MASS.H.dat')
  R_ks  = read_fdat(profilesdir+'2MASS_2MASS_KS.dat')

  ;; to micron from angstrom
  R_j.data.wave  = R_j.data.wave*1d-4
  R_h.data.wave  = R_h.data.wave*1d-4
  R_ks.data.wave  = R_ks.data.wave*1d-4
  
  lambda_0_j = 1.2350d0
  lambda_0_h = 1.6620d0
  lambda_0_ks = 2.1590d0

  ;; make a fine log spaced wavelength grid
  min_wave = min([R_j.data.wave,R_h.data.wave,R_ks.data.wave],max=max_wave)
  npoints = 1001
  fine_wave  = min_wave*10d0^(alog10(max_wave/min_wave)*dindgen(npoints)/(npoints -1L))

  ;; resample on the fine grid
  fine_f_nu_actual = interpol(f_nu_actual,wave,fine_wave)
  fine_R_j  = interpol(R_j.data.flux,R_j.data.wave,fine_wave)
  fine_R_h  = interpol(R_h.data.flux,R_h.data.wave,fine_wave)
  fine_R_ks   = interpol( R_ks.data.flux, R_ks.data.wave,fine_wave)

  ;; set to zero everything that is outside the input response function
  fine_R_j[where(fine_wave lt min(R_j.data.wave) or fine_wave gt max(R_j.data.wave))]  = 0d0
  fine_R_h[where(fine_wave lt min(R_h.data.wave) or fine_wave gt max(R_h.data.wave))]  = 0d0
  fine_R_ks [where(fine_wave lt min( R_ks.data.wave) or fine_wave gt max( R_ks.data.wave))]  = 0d0

  f_nu_0_actual_j = interpol(fine_f_nu_actual,fine_wave,lambda_0_j)
  f_nu_0_actual_h = interpol(fine_f_nu_actual,fine_wave,lambda_0_h)
  f_nu_0_actual_ks  = interpol(fine_f_nu_actual,fine_wave,lambda_0_ks )

  F_j = f_nu_0_actual_j * cw2_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_j)*fine_R_j)
  F_h = f_nu_0_actual_h * cw2_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_h)*fine_R_h)
  F_ks  = f_nu_0_actual_ks  * cw2_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_ks )*fine_R_ks )

  fine_f_nu_quoted_j = fine_wave/lambda_0_j
  fine_f_nu_quoted_h = fine_wave/lambda_0_h
  fine_f_nu_quoted_ks  = fine_wave/lambda_0_ks

  f_nu_0_quoted_j = F_j / cw2_integrate(fine_wave,fine_f_nu_quoted_j*fine_R_j)
  f_nu_0_quoted_h = F_h / cw2_integrate(fine_wave,fine_f_nu_quoted_h*fine_R_h)
  f_nu_0_quoted_ks  = F_ks  / cw2_integrate(fine_wave,fine_f_nu_quoted_ks *fine_R_ks )

  K_j = f_nu_0_quoted_j/f_nu_0_actual_j
  K_h = f_nu_0_quoted_h/f_nu_0_actual_h
  K_ks  = f_nu_0_quoted_ks /f_nu_0_actual_ks

  2MASS_j = f_nu_0_quoted_j
  2MASS_h = f_nu_0_quoted_h
  2MASS_ks  = f_nu_0_quoted_ks

  print,K_j,K_h,K_ks

  return,transpose([[lambda_0_j,lambda_0_h,lambda_0_ks],[2MASS_j,2MASS_h,2MASS_ks]])
END
