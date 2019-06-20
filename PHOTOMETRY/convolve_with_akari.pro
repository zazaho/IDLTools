FUNCTION cwa_integrate,w,f
  
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

function convolve_with_akari,p1,p2, $
                            akari_s9w=akari_s9w, $
                            akari_l18w=akari_l18w, $
                            akari_n60=akari_n60, $
                            akari_wides=akari_wides, $
                            akari_widel=akari_widel, $
                            akari_n160=akari_n160, $
                            K_s9w=K_s9w, $
                            K_l18w=K_l18w, $
                            K_n60=K_n60, $
                            K_wides=K_wides, $
                            K_widel=K_widel, $
                            K_n160=K_n160
  

  profilesdir=shell_expand('~/IA_FILES/FILTER_PROFILES/')
  
  if n_params() eq 2 then begin
     wave = p1
     f_nu_actual = p2
  endif else begin
     wave = p1.data.wave
     f_nu_actual = p1.data.flux
  endelse

  ;; http://svo2.cab.inta-csic.es/theory/fps
  R_s9w = read_fdat(profilesdir+'AKARI_IRC.S9W.dat')
  R_l18w = read_fdat(profilesdir+'AKARI_IRC.L18W.dat')
  R_n60  = read_fdat(profilesdir+'AKARI_FIS_N60.dat')
  R_wides  = read_fdat(profilesdir+'AKARI_FIS.WIDE-S.dat')
  R_widel  = read_fdat(profilesdir+'AKARI_FIS.WIDE-L.dat')
  R_n160 = read_fdat(profilesdir+'AKARI_FIS_N160.dat')

  ;; to micron from angstrom
  R_s9w.data.wave  = R_s9w.data.wave*1d-4
  R_l18w.data.wave  = R_l18w.data.wave*1d-4
  R_n60.data.wave  = R_n60.data.wave*1d-4
  R_wides.data.wave  = R_wides.data.wave*1d-4
  R_widel.data.wave  = R_widel.data.wave*1d-4
  R_n160.data.wave  = R_n160.data.wave*1d-4
  
  lambda_0_s9w = 9d0
  lambda_0_l18w = 18d0
  lambda_0_n60  = 65d0
  lambda_0_wides  = 90d0
  lambda_0_widel  = 140d0
  lambda_0_n160 = 160d0

  ;; make a fine log spaced wavelength grid
  min_wave = min([R_s9w.data.wave,R_l18w.data.wave,R_n60.data.wave,R_wides.data.wave,R_widel.data.wave,R_n160.data.wave],max=max_wave)
  npoints = 1001
  fine_wave  = min_wave*10d0^(alog10(max_wave/min_wave)*dindgen(npoints)/(npoints -1L))

  ;; resample on the fine grid
  fine_f_nu_actual = interpol(f_nu_actual,wave,fine_wave)
  fine_R_s9w  = interpol(R_s9w.data.flux,R_s9w.data.wave,fine_wave)
  fine_R_l18w  = interpol(R_l18w.data.flux,R_l18w.data.wave,fine_wave)
  fine_R_n60   = interpol( R_n60.data.flux, R_n60.data.wave,fine_wave)
  fine_R_wides   = interpol( R_wides.data.flux, R_wides.data.wave,fine_wave)
  fine_R_widel   = interpol( R_widel.data.flux, R_widel.data.wave,fine_wave)
  fine_R_n160  = interpol(R_n160.data.flux,R_n160.data.wave,fine_wave)

  ;; set to zero everything that is outside the input response function
  fine_R_s9w[where(fine_wave lt min(R_s9w.data.wave) or fine_wave gt max(R_s9w.data.wave))]  = 0d0
  fine_R_l18w[where(fine_wave lt min(R_l18w.data.wave) or fine_wave gt max(R_l18w.data.wave))]  = 0d0
  fine_R_n60 [where(fine_wave lt min( R_n60.data.wave) or fine_wave gt max( R_n60.data.wave))]  = 0d0
  fine_R_wides [where(fine_wave lt min( R_wides.data.wave) or fine_wave gt max( R_wides.data.wave))]  = 0d0
  fine_R_widel [where(fine_wave lt min( R_widel.data.wave) or fine_wave gt max( R_widel.data.wave))]  = 0d0
  fine_R_n160[where(fine_wave lt min(R_n160.data.wave) or fine_wave gt max(R_n160.data.wave))]  = 0d0

  f_nu_0_actual_s9w = interpol(fine_f_nu_actual,fine_wave,lambda_0_s9w)
  f_nu_0_actual_l18w = interpol(fine_f_nu_actual,fine_wave,lambda_0_l18w)
  f_nu_0_actual_n60  = interpol(fine_f_nu_actual,fine_wave,lambda_0_n60 )
  f_nu_0_actual_wides  = interpol(fine_f_nu_actual,fine_wave,lambda_0_wides )
  f_nu_0_actual_widel  = interpol(fine_f_nu_actual,fine_wave,lambda_0_widel )
  f_nu_0_actual_n160 = interpol(fine_f_nu_actual,fine_wave,lambda_0_n160)

  F_s9w = f_nu_0_actual_s9w * cwa_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_s9w)*fine_R_s9w)
  F_l18w = f_nu_0_actual_l18w * cwa_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_l18w)*fine_R_l18w)
  F_n60  = f_nu_0_actual_n60  * cwa_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_n60 )*fine_R_n60 )
  F_wides  = f_nu_0_actual_wides  * cwa_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_wides )*fine_R_wides )
  F_widel  = f_nu_0_actual_widel  * cwa_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_widel )*fine_R_widel )
  F_n160 = f_nu_0_actual_n160 * cwa_integrate(fine_wave,(fine_f_nu_actual/f_nu_0_actual_n160)*fine_R_n160)

  fine_f_nu_quoted_s9w = fine_wave/lambda_0_s9w
  fine_f_nu_quoted_l18w = fine_wave/lambda_0_l18w
  fine_f_nu_quoted_n60  = fine_wave/lambda_0_n60
  fine_f_nu_quoted_wides  = fine_wave/lambda_0_wides
  fine_f_nu_quoted_widel  = fine_wave/lambda_0_widel
  fine_f_nu_quoted_n160 = fine_wave/lambda_0_n160

  f_nu_0_quoted_s9w = F_s9w / cwa_integrate(fine_wave,fine_f_nu_quoted_s9w*fine_R_s9w)
  f_nu_0_quoted_l18w = F_l18w / cwa_integrate(fine_wave,fine_f_nu_quoted_l18w*fine_R_l18w)
  f_nu_0_quoted_n60  = F_n60  / cwa_integrate(fine_wave,fine_f_nu_quoted_n60 *fine_R_n60 )
  f_nu_0_quoted_wides  = F_wides  / cwa_integrate(fine_wave,fine_f_nu_quoted_wides *fine_R_wides )
  f_nu_0_quoted_widel  = F_widel  / cwa_integrate(fine_wave,fine_f_nu_quoted_widel *fine_R_widel )
  f_nu_0_quoted_n160 = F_n160 / cwa_integrate(fine_wave,fine_f_nu_quoted_n160*fine_R_n160)

  K_s9w = f_nu_0_quoted_s9w/f_nu_0_actual_s9w
  K_l18w = f_nu_0_quoted_l18w/f_nu_0_actual_l18w
  K_n60  = f_nu_0_quoted_n60 /f_nu_0_actual_n60
  K_wides  = f_nu_0_quoted_wides /f_nu_0_actual_wides
  K_widel  = f_nu_0_quoted_widel /f_nu_0_actual_widel
  K_n160 = f_nu_0_quoted_n160/f_nu_0_actual_n160

  AKARI_s9w = f_nu_0_quoted_s9w
  AKARI_l18w = f_nu_0_quoted_l18w
  AKARI_n60  = f_nu_0_quoted_n60
  AKARI_wides  = f_nu_0_quoted_wides
  AKARI_widel  = f_nu_0_quoted_widel
  AKARI_n160 = f_nu_0_quoted_n160

  print,K_s9w,K_l18w,K_n60,K_wides,K_widel,K_n160

  return,transpose([[lambda_0_s9w,lambda_0_l18w,lambda_0_n60,lambda_0_wides,lambda_0_widel,lambda_0_n160],[AKARI_s9w,AKARI_l18w,AKARI_n60,AKARI_wides,AKARI_widel,AKARI_n160]])
END
