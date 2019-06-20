;; function that yield a mode spectrum according to the 6 input parameters

FUNCTION fit28_model_spectrum, par
  COMMON specdata,wave,spec,stdev

;; parameters:
;; par[0] = p line positition (um)
;; par[1] = h line height (Jy)
;; par[2] = w line width (um) (FWHM)
;; cont  = a + b*x +c*x^2
;; par[3] = a (Jy)
;; par[4] = b (Jy)
;; par[c] = c (Jy)
;; spectrum = a + b*x + c*x^2 + h * exp ( -1d0 * ( (x-p)/ (2*
;; (w/(2*sqrt(2*alog(2))))^2)

;---------------------------------------------------------------------
;; to hange the order of plynomial for continuum fit
;; 
;; loop over number of dergrees n (for i=1,n) con_fit = con_fit + a[i] * wave^n
;;---------------------------------------------------------------------------
  
  width_parameter = par[2]/(2d0*sqrt(2d0*alog(2d0)))

  ;; the 28d0 is there to minimise the values of par[4,5]

  model = par[1] * exp(-1d0/2d0*((par[0]-wave)/width_parameter)^2) + $
          par[3] + par[4]*(wave-28d0) + par[5]*(wave-28d0)^2
  
  return, model
  
END

;; this returns the deviatin between data and model normalised by stdev
FUNCTION fit28_deviation, par
  COMMON specdata,wave,spec,stdev
  
  ffit=fit28_model_spectrum(par)
 
  return, (spec-ffit)/stdev
  
END

;; function that takes an array of determined parameter values and returns
;; some statistical measures like the mean, median, locations where
;; the -2,-1,1,2 sigma (for a normal distribution) occurs, where the
;; error bar is (roughly) symmetric and whether the parameter should
;; be considered an upperlimit

function fit28_quantify_parameter_distribution,in_par

  out = {mean:!values.d_nan, $
         median:!values.d_nan, $
         stdev:!values.d_nan, $
         sigma_m2:!values.d_nan, $
         sigma_m1:!values.d_nan, $
         sigma_p1:!values.d_nan, $
         sigma_p2:!values.d_nan, $
         sigma_min:!values.d_nan, $
         sigma_plus:!values.d_nan, $
         is_symmetric:0, $
         is_limit:0, $
         npoints:0, $
         status:"invalid" $
        }
  
  idx = where(finite(in_par),npar)
  ;; cannot do anything
  if npar lt 3 then return,out

  out.npoints = npar
  out.status="More than two valid points, basic statistic calculated"
  
  par = in_par[idx]

  ;; sort them which helps for the fraction calculation
  idx = sort(par)
  par=par[idx]

  out.mean = mean(par)
  out.median = median(par)
  out.stdev = stddev(par)

  fractions_than_contain_sigma_m2_m1_p1_p2 = gauss_pdf([-2,-1,1,2.])

  ;; need about 12 points to make a reasonable 1sigma estimate
  if npar ge 12 then begin
     out.sigma_m1 = par[round(fractions_than_contain_sigma_m2_m1_p1_p2[1]*npar)]
     out.sigma_p1 = par[round(fractions_than_contain_sigma_m2_m1_p1_p2[2]*npar)]
     out.sigma_min = out.median-out.sigma_m1
     out.sigma_plus = out.sigma_p1-out.median
     out.status="More than 11 valid points, first sigma fractions estimated"
  endif else return,out

  ;; need about 50 points to make a reasonable 1sigma estimate
  if npar ge 50 then begin
     out.sigma_m2 = par[round(fractions_than_contain_sigma_m2_m1_p1_p2[0]*npar)]
     out.sigma_p2 = par[round(fractions_than_contain_sigma_m2_m1_p1_p2[3]*npar)]
     out.sigma_min = mean([out.sigma_min,(out.median-out.sigma_m2)/2d0])
     out.sigma_plus = mean([out.sigma_plus,(out.sigma_p2-out.median)/2d0])
     out.status="Enough valid points to calculate all statistics"
  endif
  
  ;; determine if the distribution is roughly symmetrical 
  ;; 30% (why)
  if (out.sigma_min/out.sigma_plus ge 1d0/1.3d0) and (out.sigma_min/out.sigma_plus le 1.3d0) then out.is_symmetric = 1

  ;; determine if this consitutes an upper limit
  if (out.is_symmetric eq 1) then $
     sigma_to_use=out.stdev $
  else $
     sigma_to_use=out.sigma_min

  if out.median le 3d0*sigma_to_use then out.is_limit=1
  
  return,out
end


;; this is the main routine.
;; this routine will fit the 28 micron H2 line and make an estimate of
;; the uncertainties of the fit parameters.
;;
;; Procedure:
;; Look at each pixel (x,y) and extract the spectrum.
;; Isolate some (wavelength range) to fit polynome continuum and
;; gaussian line (range maybe fixed for the whole
;; Q: maybe straight integrate is also interesting?
;;
;; Error estimate ( moving the points in the spectrum up and down with
;; sigma values and refit N number of time -> distribution of the
;; parameters (most interesting) is the line flux
;; 
;; create line flux map and line flux uncertainty map
function fit28,cubename,uncertaintycubename, $
               wrange=wrange, $
               initial_pars=initial_pars, $
               initial_scale=initial_scale, $
               nperturb=nperturb, $
               plot=plot
  
  COMMON specdata,wave,spec,stdev
  
  c_um = 2.99792458d14                        ;; Speed of light [SI]

  ;; do we want diagnostic plots?
  if keyword_set(plot) then begin
     doplot=1 
     if size(plot,/tname) eq 'STRING' then plotname=plot else plotname='fit28_diagnostics.ps'
  endif else begin 
     doplot=0
  endelse
  
;;; first we read in the data to be fitted
;;; TODO check for file existence
  specdata = readfitscube(cubename)
  uncertaintydata  = readfitscube(uncertaintycubename)
  
;;; extract the useful bits
  cube = specdata.cube
  uncertaintycube = uncertaintydata.cube
  wave = specdata.wave

  nx = n_elements(cube[*,0,0])
  ny = n_elements(cube[0,*,0])
  
  ;;; define wavelength range to be fitted
  ;; seemingly good default value
  if n_elements(wrange) ne 2 then wrange=[26.5,29.5]
  if n_elements(nperturb) ne 1 then nperturb=300L else nperturb=long(nperturb)

  idx_wave_to_be_fitted = where(wave ge min(wrange) and wave le max(wrange))
  wave = wave[idx_wave_to_be_fitted]
  nwave = n_elements(wave)
  
;; parameters:
;; spectrum = h * exp ( -1d0/2d0 * ((x-p)/(w/(2*sqrt(2*alog(2)))))^2) + a + b*x + c*x^2
;; par[0] = p line positition (um)
;; par[1] = h line height (MJy/sr)
;; par[2] = w line width (um) (FWHM)
;; par[3] = a (Jy)
;; par[4] = b (Jy)
;; par[6] = c (Jy)
  
  ;; reasonable values that avoid falling in a local minimum

  H2_peak_position_lab = 28.21883d0

  IF n_elements(initial_pars) ne 2 then initial_pars=[H2_peak_position_lab,10.,0.2,0.,0.,0.]
  IF n_elements(initial_scale) ne 2 then initial_scale=[0.1,1.,0.1,1,1,1]
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0]}, 6)
  
;; Set some reasonable constraints
  parinfo[*].value = initial_pars
  
  parinfo[0].limited = [1,1]
  parinfo[0].limits = [H2_peak_position_lab-0.4,H2_peak_position_lab+0.4]
  
  parinfo[1].limited = [1,0]
  parinfo[1].limits = [0d0,666d0]
  
  parinfo[2].limited = [1,1]
  
  ;; we cannot expect lines narrower than given by R(=lambda/FWHM) < 90
  ;; we take fwhm < H2_peak_position_lab/100 which corresponds to 3000 km/s
  ;; http://irsa.ipac.caltech.edu/data/SPITZER/docs/irs/irsinstrumenthandbook/2
  ;; and wider than 3 times the resolution
  parinfo[2].limits = H2_peak_position_lab/100d0* [1., 2.]

  ;; set the initial_par within this range
  initial_pars[2] = H2_peak_position_lab/100d0*1.5

  ;; TUDU implement some logic for the order of the polynomial
  
  ;; to hold the outputs
  outpars = { $
            best_fit_position         : make_array(nx,ny,value=!values.d_nan), $
            best_fit_peak_fluxdensity : make_array(nx,ny,value=!values.d_nan), $
            best_fit_fwhm             : make_array(nx,ny,value=!values.d_nan), $
            best_fit_cont_p0          : make_array(nx,ny,value=!values.d_nan), $
            best_fit_cont_p1          : make_array(nx,ny,value=!values.d_nan), $
            best_fit_cont_p2          : make_array(nx,ny,value=!values.d_nan), $
            best_fit_flux             : make_array(nx,ny,value=!values.d_nan), $
            median_position           : make_array(nx,ny,value=!values.d_nan), $
            median_fwhm               : make_array(nx,ny,value=!values.d_nan), $
            median_flux               : make_array(nx,ny,value=!values.d_nan), $
            stdev_position            : make_array(nx,ny,value=!values.d_nan), $
            stdev_fwhm                : make_array(nx,ny,value=!values.d_nan), $
            stdev_flux                : make_array(nx,ny,value=!values.d_nan) $
            }
            

  ;; array that can hold to temporary output when we do the perturbing
  ;; and refitting of the spectra
  perturbed_outpars = { $
                      position         : make_array(nx,ny,value=!values.d_nan), $
                      peak_fluxdensity : make_array(nx,ny,value=!values.d_nan), $
                      fwhm             : make_array(nx,ny,value=!values.d_nan) $
                      }


  if doplot then begin
     sh_ps,plotname,/a4land,/color,thick=3,charsize=1.5,loadtable=39
  endif

  ;; now loop over each pixel and fit the spectrum
  for xx=0,nx-1 do begin
     for yy=0,ny-1 do begin
        
        print,format='("Now doing pixel (",I3,",",I3,")")',xx,yy
        
        spec = reform(cube[xx,yy,idx_wave_to_be_fitted])
        stdev = reform(uncertaintycube[xx,yy,idx_wave_to_be_fitted])
        
        ;; plot the spectrum
        if doplot then begin
           ;; title for the whole page
           xyouts,0.5,0.96,string(format='("Pixel (",I3,",",I3,")")',xx,yy),/normal,align=0.5,charsize=2
           !p.multi=[0,2,2]
           pl,transpose([[wave],[spec],[stdev]]),psym=0,xtit='wavelength',ytit='flux',ymin=0,/err,/autoy
        endif

        best_fit_pars = mpfit('fit28_deviation',initial_pars,parinfo=parinfo,status=status,errmsg=errmsg,/quiet)
        
        if status gt 0 then begin
           ;; MJy/sr: go to W/m2 but keep the /sr
           peak_position_um = best_fit_pars[0]
           peak_MJy_sr = best_fit_pars[1]
           FWHM_um = best_fit_pars[2]
           
           ;; 1d6 * height * width * sqrt(2pi) * c/l^2 * 1d-16
           flux_W_m2_sr = $
              peak_MJy_sr * $
;; >>>> HERE  FWHM_um / 2d0*sqrt(2d0*alog(2d0)) * $
              FWHM_um / (2d0*sqrt(2d0*alog(2d0))) * $
              sqrt(!dpi*2d0) * $
              c_um / (peak_position_um)^2 * $
              1d-26 * 1d6
           
           outpars.best_fit_position[xx,yy] = peak_position_um
           outpars.best_fit_peak_fluxdensity[xx,yy] = peak_MJy_sr
           outpars.best_fit_fwhm[xx,yy] = FWHM_um
           outpars.best_fit_cont_p0[xx,yy] = best_fit_pars[3]
           outpars.best_fit_cont_p1[xx,yy] = best_fit_pars[4]
           outpars.best_fit_cont_p2[xx,yy] = best_fit_pars[5]
           outpars.best_fit_flux[xx,yy] = flux_W_m2_sr

           ;; check that de residual is comparable to the given
           ;; stdevs
           
           model = fit28_model_spectrum(best_fit_pars)
           deviations = abs(spec-model)
           median_deviation = median(deviations)
           median_stdev=median(stdev)

           ;; plot the model on top of spectrum
           if doplot then begin
              pl,wave,model,psym=0,/oplot,mark=peak_position_um+[-0.5,0,0.5]*FWHM_um,mstyle=2,mcolor=kleur('red')
           endif

           ;; logic to perturb the spectrum (Nperturb times) and refit
           ;; to see how the bests fits change
           original_spec = spec

           for pp=0,Nperturb-1 do begin   
;;               perturbation = RANDOMN(seed, nwave)*stdev
              perturbation = RANDOMN(seed, nwave)*median_deviation
              Spec = original_Spec + perturbation
              
              ;; A small trick to increase the speed. Assuming
              ;; that the spectrum does not change to much between perturbations
              ;; we start with the best solution unperturbed fit
              perturbed_fit_pars = mpfit('fit28_deviation',best_fit_pars,parinfo=parinfo,status=status,errmsg=errmsg,/quiet)
              
              if status gt 0 then begin
                 perturbed_outpars.position[pp] = perturbed_fit_pars[0]
                 perturbed_outpars.peak_fluxdensity[pp] = perturbed_fit_pars[1]
                 perturbed_outpars.fwhm[pp] = perturbed_fit_pars[2]
              endif
              
           endfor
           
           fitted_idx = where(finite(perturbed_outpars.position))
           perturbed_peak_positions_um = perturbed_outpars.position[fitted_idx]
           perturbed_peak_fluxdensities_MJy_sr = perturbed_outpars.peak_fluxdensity[fitted_idx]
           perturbed_FWHMs_um = perturbed_outpars.fwhm[fitted_idx]
           
           perturbed_fluxes_W_m2_sr = $
              perturbed_peak_fluxdensities_MJy_sr * $
;;>>>> HERE   perturbed_FWHMs_um / 2d0*sqrt(2d0*alog(2d0)) * $
              perturbed_FWHMs_um / (2d0*sqrt(2d0*alog(2d0))) * $
              sqrt(!dpi*2d0) * $
              c_um / (perturbed_peak_positions_um)^2 * $
              1d-26 * 1d6

           detections_idx = where(perturbed_fluxes_W_m2_sr ne 0d0,detection_cnt)
           
           ;; make sure we have some line detections to do the stats
           if detection_cnt ge 3 then begin
              flux_stats = fit28_quantify_parameter_distribution(perturbed_fluxes_W_m2_sr)
              peak_position_stats = fit28_quantify_parameter_distribution(perturbed_peak_positions_um[detections_idx])
              FWHM_stats = fit28_quantify_parameter_distribution(perturbed_FWHMs_um[detections_idx])
              
              outpars.median_position[xx,yy] = peak_position_stats.median
              outpars.median_fwhm[xx,yy] = fwhm_stats.median
              outpars.median_flux[xx,yy] = flux_stats.median

              outpars.stdev_position[xx,yy] = peak_position_stats.stdev
              outpars.stdev_fwhm[xx,yy] = fwhm_stats.stdev
              outpars.stdev_flux[xx,yy] = flux_stats.stdev
              
              ;; we could update the flux parameter here with the
              ;; median value from the ensemble of perturbed fits
              
              if flux_stats.is_limit eq 1 then begin
                 outpars.best_fit_flux[xx,yy] = 0d0
                 outpars.median_flux[xx,yy] = 0d0
              endif
              
              ;; plot the histograms of the distribtions
              if doplot then begin
                 
                 value = perturbed_fluxes_W_m2_sr*1d10
                 nn = n_elements(value)
                 hh = histogram(value,nbins=nn/5,locations=ll)
                 pl,ll,hh,ymin=0,xtit='Perturbed Fluxes [10e-10 * W/m2/sr]',ytit='#',xstyle=0
                 
                 value = perturbed_peak_positions_um[detections_idx]
                 nn = n_elements(value)
                 hh = histogram(value,nbins=nn/5,locations=ll,min=parinfo[0].limits[0],max=parinfo[0].limits[1])
                 pl,ll,hh,ymin=0,xtit='Perturbed Positions [um]',ytit='#',xstyle=0

                 value = perturbed_FWHMs_um[detections_idx]
                 nn = n_elements(value)
                 hh = histogram(value,nbins=nn/5,locations=ll,min=parinfo[2].limits[0],max=parinfo[2].limits[1])
                 pl,ll,hh,ymin=0,xtit='Perturbed FWHM [um]',ytit='#',xstyle=0
                 
              endif
           endif 

;           else begin
;              ;; set everything to undefined without stats !!
;              outpars.(*)[xx,yy] = !values.d_nan
;           endelse


;;; calculate error based on the perturbed parameters
;;; calculate the line flux using the output per_parametrs based on
;;; P[1] and P[0] -- look at the distribution and quantify the
;;;                  dispersion and consider sigma as the stddev if the
;;;                  dispersion is symmetric otherwise take the
;;;                  assymetric error bars with extreme 
;;;    
;
;;; line flux -------------------------------------------------------        
;;; the line flux intensity is obtained by multiplying line hight in Jy
;;; and line width in mm, multiply a constant 6.215e-6 to obtain the unit
;;; in w/m^2/Sr for LL1 25.806*2.3505e-11 Sr/px
;
;       Fluxline_per= Par[2]*Par[3]*6.215e-6 
;
;;;make a histogram of the lines flux to see the distribution
;       bin_size = 0.001
;       hist_flux = histogram(Fluxline_per, 0.001)
;       bin_num = n_elements(spec)
;       bin_hist = findgen(bin_num) * binsize
;       plot, bin_hist, hist_flux
;
;;; error estimation---------------------------------------------------
;;; find mean, standard deviation, mode of perturbed lines flux and
;;; determine the dispersion (Skewness)
;       Fluxline_mean=mean(Fluxline_per), print,Fluxline_mean
;       Fluxline_stdev=stdev(Fluxline_per)
;       Fluxline_mod= mode(Fluxline_per)
;; dispersion
;
;       SKEW = (Fluxline_mean - Fluxline_mod)/Fluxline_stdev
;        
;
;; Dispersion is symmetric when SKEW=0 and Sigma=Fluxline_stdev
;       if SKEW eq 0 then sigma=Fluxline_stdev
;       if SKEW gt -0.1 and SKEW lt +0.1 then sigma=0.99*Fluxline_stdev
;
;; positive dispersion
;       if SKEW gt +0.1 then sigma = max(Fluxline_per)-Fluxline_mod
;
;; negative dispersion
;
;       if SKEW lt -0.1 then sigma = Fluxline_mod-min(Fluxline_per)
;
;     endfor
        endif
     endfor
  endfor

  if doplot then begin
     sh_ps
  endif
  
  return,outpars

END
