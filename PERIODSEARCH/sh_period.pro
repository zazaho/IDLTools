;;------------------------------------------------------------------
;; routine to update or add a field in the vardata structure
;; there are two params in addition to the sInfo.
;; Field: the name of the field to add
;; Value: the value to give to it
PRO sh_period_modify_vardata,sInfo,field,value
  old = (*(sInfo.ptrVarData))
  oldtags = strupcase(tag_names(old))
  uppfield = strupcase(field)
  
  execstr = 'newstruct = {'+field+':value'
  FOR i = 0,n_elements(oldtags)-1 DO BEGIN
      IF oldtags[i] NE uppfield THEN BEGIN
          execstr = execstr+','+oldtags[i]+':old.'+oldtags[i]
      ENDIF
  ENDFOR
  execstr = execstr+'}'
  foo = execute(execstr)
  newptr = ptr_new(newstruct)
  
  ptr_free, sInfo.ptrVarData
  sInfo.ptrVarData=newptr
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; The chi-square of a distribution
FUNCTION sh_period_stdev2,values,errors,wmean=wmean
  
  weight = 1d0/Errors^2d0
  ;; normalise the weigths against possible very small or large
  ;; numbers
  weight = weight/max(weight)
  wmean = total(Values*weight)/total(weight)
  wstdev2 = total(weight*(Values-wmean)^2d0) * $
            total(weight) / $
            (total(weight)^2d0-total(weight^2d0))
  return,wstdev2
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
PRO sh_period_sin,X,A,F,pder
  phase = A[0]
  freq = A[1]
  Amplitude = A[2]
  mean = A[3]

  F = mean + Amplitude*sin(2d0*!dpi*(X*freq-phase))
  
  ;; the partial derivatives are:
  pder = [ $
         [-2d0*!dpi*Amplitude*cos(2d0*!dpi*(X*freq-phase))], $ ;; phase
         [2d0*!dpi*X*Amplitude*cos(2d0*!dpi*(X*freq-phase))], $ ;; freq
         [sin(2d0*!dpi*(X*freq-phase))], $  ;; amplitude
         [make_array(n_elements(x),val=1d0)] $
         ]
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
PRO sh_period_TwoSin,X,A,F,pder
  phase1 = A[0]
  freq1 = A[1]
  Amplitude1 = A[2]
  mean1 = A[3]
  
  phase2 = A[4]
  freq2 = A[5]
  Amplitude2 = A[6]
  mean2 = A[7]

  F = mean1 + Amplitude1*sin(2d0*!dpi*(X*freq1-phase1)) + $
      mean2 + Amplitude2*sin(2d0*!dpi*(X*freq2-phase2)) 
  
  ;; the partial derivatives are:
  pder = [ $
         [-2d0*!dpi*Amplitude1*cos(2d0*!dpi*(X*freq1-phase1))], $ ;; phase
         [2d0*!dpi*X*Amplitude1*cos(2d0*!dpi*(X*freq1-phase1))], $ ;; freq
         [sin(2d0*!dpi*(X*freq1-phase1))], $  ;; amplitude
         [make_array(n_elements(x),val=1d0)], $
         [-2d0*!dpi*Amplitude2*cos(2d0*!dpi*(X*freq2-phase2))], $ ;; phase
         [2d0*!dpi*X*Amplitude2*cos(2d0*!dpi*(X*freq2-phase2))], $ ;; freq
         [sin(2d0*!dpi*(X*freq2-phase2))], $  ;; amplitude
         [make_array(n_elements(x),val=1d0)] $
         ]
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; Try to fit an estimate of the phase of the lightcurve given a frequency
PRO sh_period_fit_phase,sInfo,freq,amplitude
  
;; keep track of the success of the steps
  havefit = 0
  sInfo.Status.SinFitStatus = -1
  
  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]

  phase = times*freq - floor(times*freq)
  
  weights = 1d0/errors^2d0

  wmean = total(Values*weights)/total(weights)
  
  normalized = (values-wmean)/amplitude

  ;;initial guess depth
  A = [0.5,1d0,1d0,0d0]
  yfit = CURVEFIT(phase,normalized,weights,A, $
                  FUNCTION_NAME='sh_period_sin', $
                  tol=1d-3,/double,itmax=50, $
                  status=status,FitA=[1,0,0,0])  
  
  IF status EQ 0 THEN havefit=1
  
;; if success then store the result and update the status
  IF havefit THEN BEGIN
      sInfo.Status.SinFitStatus = status
      sInfo.FitData.SinFitPhase = A[0]
   ENDIF
  
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; Try to fit the lightcurve with a sin wave
PRO sh_period_fit_sin,sInfo, $
                      freq=freq, $
                      amplitude=amplitude, $
                      phase=phase, $
                      mean=mean, $
                      fitphase=fitphase
  
  ;; First take fetch the data
  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
  weights = 1d0/errors^2d0
  
  ;; if no initial frequency assume 1 period in the whole sample
  default,freq,1d0/(max(times)-min(times))
  default,amplitude,(max(values)-min(values))/2d0
  default,mean,total(Values*weights)/total(weights)
  
  ;; first see if a seperate phase fit is needed
  IF keyword_set(fitphase) THEN BEGIN
     sh_period_fit_phase,sInfo,freq,Amplitude
     IF sInfo.Status.SinFitStatus EQ 0 THEN BEGIN
        phase=sInfo.FitData.SinFitPhase
     ENDIF
  ENDIF
  default,phase,0.5d0
  
;; keep track of the success of the steps
  havefit = 0
  sInfo.Status.SinFitStatus = -1
  
  ;;initial guess depth
  A = [phase,freq,amplitude,mean]
  yfit = CURVEFIT(Times,Values,weights,A, $
                  FUNCTION_NAME='sh_period_sin', $
                  tol=1d-3,/double,itmax=50, $
                  status=status,FitA=[1,1,1,1])  
  
  IF status EQ 0 THEN havefit=1
  
;; if success then store the result and update the status
  IF havefit THEN BEGIN

     ;; check against negtive amplitudes:
     IF A[2] LT 0d0 THEN BEGIN
        ;; swap the sign and shift the curve by half a period
        A[2]  = -1d0*A[2]
        A[0] = A[0]-0.5d0
     ENDIF
     
     ;; Take only the modulus of the phase
     A[0] = A[0] - floor(A[0])
     
     sInfo.Status.SinFitStatus = status
     sInfo.FitData.SinFitPhase = A[0]
     sInfo.FitData.SinFitfrequency  = A[1]
     sInfo.FitData.SinFitamplitude  = A[2]
     sInfo.FitData.SinFitMean  = A[3]
     
     ;; Make the button active
     widget_control,sInfo.Handles.wWhiteSin,/sensitive
     
     ;; Write the value in the button box
     widget_control,sInfo.Handles.wFSin, $
                    set_value = 'f sinus: '+ $
                    string(format='(F7.5)',A[1]), $
                    /sensitive
  ENDIF
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; Try to fit the lightcurve with a sin wave
PRO sh_period_fit_TwoSin,sInfo
  
  ;; Check to see if we have two curves to start with
  IF sInfo.FitData.NSinFits EQ 2 THEN BEGIN
     p1 = sInfo.FitData.SinFit1Phase
     f1 = sInfo.FitData.SinFit1frequency
     A1 = sInfo.FitData.SinFit1amplitude
     M1 = sInfo.FitData.SinFit1Mean
     p2 = sInfo.FitData.SinFit2Phase
     f2 = sInfo.FitData.SinFit2frequency
     A2 = sInfo.FitData.SinFit2amplitude
     M2 = sInfo.FitData.SinFit2Mean
     
     ;; First take fetch the original data
     Times  = sInfo.Data.Times [where(sInfo.DelIndex EQ 0)]
     Values = sInfo.Data.Values[where(sInfo.DelIndex EQ 0)]
     Errors = sInfo.Data.Errors[where(sInfo.DelIndex EQ 0)]
     weights = 1d0/errors^2d0
  
;; keep track of the success of the steps
     havefit = 0
     sInfo.Status.SinFitStatus = -1
     
     ;;initial guess depth
     A = [p1,f1,a1,m1,p2,f2,a2,m2]
     yfit = CURVEFIT(Times,Values,weights,A, $
                     FUNCTION_NAME='sh_period_TwoSin', $
                     tol=1d-3,/double,itmax=50, $
                     status=status,FitA=[1,1,1,1])  
     
     IF status EQ 0 THEN havefit=1


     
;; if success then store the result and update the status
     IF havefit THEN BEGIN
        
        ;; check against negtive amplitudes:
        IF A[2] LT 0d0 THEN BEGIN
           ;; swap the sign and shift the curve by half a period
           A[2]  = -1d0*A[2]
           A[0] = A[0]-0.5d0
        ENDIF
        
        ;; Take only the modulus of the phase
        A[0] = A[0] - floor(A[0])
        
        ;; check against negtive amplitudes:
        IF A[6] LT 0d0 THEN BEGIN
           ;; swap the sign and shift the curve by half a period
           A[6]  = -1d0*A[6]
           A[4] = A[4]-0.5d0
        ENDIF
        
        ;; Take only the modulus of the phase
        A[4] = A[4] - floor(A[4])

        ;;Use the sine with the largest amplitude as the Fit1
        IF A[2] LT A[6] THEN A = shift(A,4)
        
        ;; Make the button active
        widget_control,sInfo.Handles.wWhiteTwoSin,/sensitive
     
        sInfo.Status.TwoSinFitStatus = status
        ;; Fill the values in the place of the individual sine fits
        sInfo.FitData.SinFit1Phase      = A[0]
        sInfo.FitData.SinFit1frequency  = A[1]
        sInfo.FitData.SinFit1amplitude  = A[2]
        sInfo.FitData.SinFit1Mean       = A[3]
        sInfo.FitData.SinFit2Phase      = A[4]
        sInfo.FitData.SinFit2frequency  = A[5]
        sInfo.FitData.SinFit2amplitude  = A[6]
        sInfo.FitData.SinFit2Mean       = A[7]
     ENDIF
  ENDIF
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; Simple function the fit a parabola with the continuum at 1
PRO sh_period_parabola,X,A,F,pder
  x0 = A[0]
  depth = A[1]
  width = A[2]
  
  F = (1-depth) + ((X-x0)/width)^2d0
  
  ;; the partial derivatives are:
  pder = [[-2d0*(X-x0)/width^2d0],[replicate(-1d0,n_elements(x))],[-0.5d0*(x-x0)^2d0/width^3d0]]
END
;;------------------------------------------------------------------
  
;;------------------------------------------------------------------
;; select a small region around the selected
;; frequency and fit a parabola to it
FUNCTION sh_period_fit_parabola,sInfo,cfreq

;; keep track of the success of the steps
  havefit = 0
  sInfo.Status.ParStatus = -1

  f = (*(sInfo.ptrVarData)).PDMfreqs
  t = (*(sInfo.ptrVarData)).PDMthetas

  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]

  delta_f = 1d0/2d0/(max(times)-min(times))
  
  limits = 0.75d0*delta_f*[-1,1]+cfreq
  
  idx = where((f GE limits[0]) AND (f LE limits[1]),cnt)
  IF cnt GT 3 THEN BEGIN
      sf = f[idx]
      st = t[idx]
      sweight = st*0d0+1d0
      ;;initial guess depth
      A = [cfreq,1d0,delta_f]
      yfit = CURVEFIT(sf,st,sweight,A, $
                      FUNCTION_NAME='sh_period_parabola', $
                      tol=1d-3,/double,itmax=50,status=status)  
      
      IF status EQ 0 THEN BEGIN
          
          havefit = 1
          
          ;; remember the outcome
          fcfreq = A[0]
          ff = sf
          fy = yfit
          
          ;; repeat the procedure for robustness:
          limits = 0.75d0*delta_f*[-1,1]+cfreq
          ;;limits = [0.9d0,1d0/0.9d0]*fcfreq
          
          idx = where((f GE limits[0]) AND (f LE limits[1]),cnt)
          IF cnt GT 3 THEN BEGIN
              sf = f[idx]
              st = t[idx]
              sweight = st*0d0+1d0
              ;;initial guess depth
              A = [fcfreq,1d0,delta_f]
              yfit = CURVEFIT(sf,st,sweight,A, $
                              FUNCTION_NAME='sh_period_parabola', $
                              tol=1d-3,/double,itmax=50,status=status)  
              ;; if success then store the result and update the status
              IF status EQ 0 THEN BEGIN
                  ;;store the new result
                  ;; remember the outcome
                  fcfreq = A[0]
                  ff = sf
                  fy = yfit
               ENDIF
          ENDIF
      ENDIF
  ENDIF
  
;; if success then store the result and update the status
  IF havefit THEN BEGIN
      sInfo.FitData.ParFrequency = fcfreq
      sInfo.Status.ParStatus = status
      ;; Write the value in the button box
      widget_control,sInfo.Handles.wFPar, $
                     set_value = 'f parabola: '+ $
                     string(format='(F7.5)',fcfreq),/sensitive
      return,transpose([[ff],[fy]])
  ENDIF ELSE BEGIN
      return,-1
  ENDELSE
  
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
;; function to calculate a PDM analysis given ,times, values, errors
;; Optional inputs:
;; nb number of bins in phase space, 
;; nc number of subbins in phase space
;; min_freq lowest value of the frequency to calculate
;; max_freq highest frequency
;; nfreq number of frequencies to calculate
FUNCTION sh_period_pdm,times,values,errors, $
  nb=nb,nc=nc, $
  min_freq=min_freq,max_freq=max_freq,nfreq=nfreq
  
  ntimes = n_elements(Times)
  ;; number of bins to divide the 
  IF n_elements(nb) EQ 0 THEN BEGIN
     ;; take the biggest of at least 10 points per bin on average and
     ;; at least 5 bins and at most 20 bins
      nbins = ((round(ntimes/10d0) > 5) < 20)
  ENDIF
  
  default,nc,2
  
  ;; number of overlapping bins
  bins = [dindgen(nb*nc+1)/nb/nc]

  IF n_elements(max_freq) EQ 0 THEN BEGIN
     ;; calculate the mean time between observations and use this as
     ;; some indication for calculating the nyquist frequency
     delta_times = (times - shift(times,1))[1:*]
     mean_delta_time = median(delta_times)
     max_freq = 1d0/(2d0*mean_delta_time)
  ENDIF ELSE BEGIN
     IF max_freq LE 0d0 THEN BEGIN
        ;; calculate the mean time between observations and use this as
        ;; some indication for calculating the nyquist frequency
        delta_times = (times - shift(times,1))[1:*]
        mean_delta_time = mean(delta_times)
        max_freq = 1d0/(2d0*mean_delta_time)
     ENDIF
  ENDELSE

  IF n_elements(min_freq) EQ 0 THEN BEGIN
      min_freq = 3d0/(max(times)-min(times))
  ENDIF ELSE BEGIN
      IF min_freq LE 0d0 THEN BEGIN
          min_freq = 3d0/(max(times)-min(times))
      ENDIF
  ENDELSE

  IF min_freq GT max_freq THEN BEGIN
      tmp = max_freq
      max_freq = min_freq
      min_freq = tmp
  ENDIF
  
  ;; The half width of the line according to eqn 10 of stellingwerf
  IF n_elements(nfreq) EQ 0 THEN BEGIN
      delta_f = 1d0/2d0/(max(times)-min(times))/10d0
      nfreq = ceil((max_freq-min_freq)/delta_f)+1
  ENDIF ELSE BEGIN
      IF nfreq LT 1 THEN BEGIN
          delta_f = 1d0/2d0/(max(times)-min(times))/10d0
          nfreq = ceil((max_freq-min_freq)/delta_f)+1
      ENDIF
  ENDELSE
  
  freqs = min_freq+dindgen(nfreq)*(max_freq-min_freq)/(nfreq-1)
  
  ;;we follow closely the description of Stellingwerf 1978 ApJ 224,
  ;;953 except that we use the errors to define weights
  
  weight = 1d0/Errors^2d0
  ;; normalise the weigths against possible very small or large
  ;; numbers
  weight = weight/max(weight)
  wmean = total(Values*weight)/total(weight)
  wstdev2 = total(weight*(Values-wmean)^2d0) * $
            total(weight) / $
            (total(weight)^2d0-total(weight^2d0))

  ;; the results will be kept in the following matrix:
  wvariances = make_array(nfreq,nb*nc,value=0d0)
  npoints  = make_array(nfreq,nb*nc,value=0)
  
  ;; loop over the trial frequencies
  FOR i = 0,nfreq-1 DO BEGIN
      ;; determine the phase of each point for this frequency
      phase = times*freqs[i] - floor(times*freqs[i])

      ;; Now calculate the stdev2 in each subbin
      FOR j = 0,nb*nc-1 DO BEGIN
          idx = where((phase GE bins[j]) AND (phase LT bins[j+1]),cnt)
          npoints[i,j] = cnt
          CASE cnt OF
              0: BEGIN
              END
              1: BEGIN
                  wvariances[i,j] = 1d0
              END
              ELSE: BEGIN
                  wmeansub = total(Values[idx]*weight[idx])/total(weight[idx])
                  wstdev2sub = total(weight[idx]*(Values[idx]-wmeansub)^2d0) * $
                               total(weight[idx]) / $
                               (total(weight[idx])^2d0-total(weight[idx]^2d0))
                  wvariances[i,j] = wstdev2sub
              END
          ENDCASE
      ENDFOR
  ENDFOR
  
  ws2 = make_array(nfreq)
  
  FOR i = 0,nfreq-1 DO BEGIN
      ws2[i] = total((npoints[i,*]-1)*wvariances[i,*])/(ntimes-total(npoints[i,*] NE 0))
  ENDFOR
  
  ;; return the frequencies and the thetas
  return,transpose([[freqs],[ws2/wstdev2]])

END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; routine to calculate a mean light curve in phase
pro sh_period_pdm_meanlightcurve,sInfo
  
  ;; Reset the meanstatus to indicate that there is no mean curve yet
  sInfo.Status.MeanStatus = -1

  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
  

  ;; see which frequency to use
  freq = widget_info(sInfo.Handles.wFMin,/button_set)*sInfo.FitData.MinFrequency + $
         widget_info(sInfo.Handles.wFMou,/button_set)*sInfo.FitData.SelectedFrequency + $
         widget_info(sInfo.Handles.wFSin,/button_set)*sInfo.FitData.SinFitFrequency + $
         widget_info(sInfo.Handles.wFPar,/button_set)*sInfo.FitData.ParFrequency
  
  weight = 1d0/Errors^2d0
  ;; normalise the weigths against possible very small or large
  ;; numbers
  weight = weight/max(weight)

  nbins = 11
  
  ;; number of overlapping bins
  bins = dindgen(nbins+1)/nbins
  wmeansub = make_array(nbins,value=-666d0)
  
  phase = times*freq - floor(times*freq)
  
  FOR j = 0,nbins-1 DO BEGIN
      idx = where((phase GE bins[j]) AND (phase LT bins[j+1]),cnt)
      CASE cnt OF
          0: BEGIN
          END
          1: BEGIN
              wmeansub[j] = values[idx]
          END
          ELSE: BEGIN
              wmeansub[j] = total(Values[idx]*weight[idx])/total(weight[idx])
          END
      ENDCASE
  ENDFOR
  
  idx = where(wmeansub NE -666d0,cnt)
  
  IF cnt NE 0 THEN BEGIN
      ;; The mean lightcurve is defined at the following points:
      midbins = (bins[0:nbins-1]+0.5d0/nbins)[idx]
      wmeansub = wmeansub[idx]

      IF n_elements(wmeansub) GE 4 THEN begin
         ;; interpolate to the original phases
         PDMmean = interpol(wmeansub,midbins,phase,/lsquadratic)
      ENDIF ELSE BEGIN
         PDMmean = interpol(wmeansub,midbins,phase)
      ENDELSE

      ;; store the results 
      ;; Data for the PDM calculation
      sh_period_modify_vardata,sInfo,'PDMmean',PDMmean
      sh_period_modify_vardata,sInfo,'MeanInPhase',[wmeansub,wmeansub[0]]
      sh_period_modify_vardata,sInfo,'Phase',[midbins,midbins[0]+1]
      sh_period_modify_vardata,sInfo,'FreqMean',freq
      sInfo.Status.MeanStatus = 0
  ENDIF
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
;; routine to calculate the residue after removing a fit to the
;; lightcurve. The result depends on which method is chosen
PRO sh_period_residual,sInfo
  
  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
  
  ;; check is a new residual is needed
  IF sInfo.Status.ResStatus NE 0 THEN BEGIN
     f = 0d0
     
     CASE 1 of
        widget_info(sInfo.Handles.wWhiteSin,/button_set): BEGIN
           ;; if we have no succesfull fit then make one
           IF sInfo.Status.SinFitStatus NE 0 THEN BEGIN
              f = widget_info(sInfo.Handles.wFMin,/button_set)*sInfo.FitData.MinFrequency + $
                  widget_info(sInfo.Handles.wFMou,/button_set)*sInfo.FitData.SelectedFrequency + $
                  widget_info(sInfo.Handles.wFSin,/button_set)*sInfo.FitData.SinFitFrequency + $
                  widget_info(sInfo.Handles.wFPar,/button_set)*sInfo.FitData.ParFrequency
              ;; use the selected frequency to make a new fit
              sh_period_fit_sin,sInfo,freq=f, $
                                /fitphase
           ENDIF
           ;; check again to see if the fit was succesful
           IF sInfo.Status.SinFitStatus EQ 0 THEN BEGIN
              ;; calculate the sinus at the times given by the good data
              sh_period_sin,times, $
                            [sInfo.FitData.SinFitPhase, $
                             sInfo.FitData.SinFitfrequency, $
                             sInfo.FitData.SinFitamplitude, $
                             sInfo.FitData.SinFitMean], $
                            sin,pder
              resi = values - sin
              f = sInfo.FitData.SinFitfrequency
              A = sInfo.FitData.SinFitamplitude
              p = sInfo.FitData.SinFitPhase
              M = sInfo.FitData.SinFitMean
              method='Sine fit'
           ENDIF
        END
        widget_info(sInfo.Handles.wWhitePDM,/button_set): BEGIN
           ;; if we have no succesfull fit then make one
           IF sInfo.Status.MeanStatus NE 0 THEN BEGIN
              sh_period_pdm_meanlightcurve,sInfo
           ENDIF
           IF sInfo.Status.MeanStatus EQ 0 THEN BEGIN
              PDMMean = (*(sInfo.ptrVarData)).PDMMean
              Resi = values-PDMMean
              f = (*(sInfo.ptrVarData)).FreqMean
              A = max(PDMMean)-min(PDMMean)
              M = mean(PDMmean)
              ;; this is not correct but what is the phase of PDM
              p = 0.5d0
              method='PDM Curve'
           ENDIF
        END
        widget_info(sInfo.Handles.wWhiteTwoSin,/button_set): BEGIN
           ;; if we have no succesfull fit then make one
           IF sInfo.Status.TwoSinFitStatus NE 0 THEN BEGIN
              sh_period_fit_TwoSin,sInfo
           ENDIF
           IF sInfo.Status.TwoSinFitStatus EQ 0 THEN BEGIN
              ;; calculate the two sinus at the times given by the good data
              sh_period_twosin,times, $
                               [ $
                               sInfo.FitData.SinFit1Phase, $
                               sInfo.FitData.SinFit1frequency, $
                               sInfo.FitData.SinFit1amplitude, $
                               sInfo.FitData.SinFit1Mean, $
                               sInfo.FitData.SinFit2Phase, $
                               sInfo.FitData.SinFit2frequency, $
                               sInfo.FitData.SinFit2amplitude, $
                               sInfo.FitData.SinFit2Mean $
                               ], $
                               twosin,pder

              ;; Use the original data unwhitened
              Values = sInfo.Data.Values[where(sInfo.DelIndex EQ 0)]
              resi = values - twosin

              ;; Use the found values of the first (main) frequency
              f = sInfo.FitData.SinFit1frequency
              A = sInfo.FitData.SinFit1amplitude
              p = sInfo.FitData.SinFit1Phase
              M = sInfo.FitData.SinFit1Mean
              method='Double Sine Fit'
           ENDIF
        END
        ELSE: BEGIN
        END
     ENDCASE
     
     IF n_elements(resi) NE 0 THEN BEGIN
        sh_period_modify_vardata,sInfo,'Residual',Resi
        sInfo.Status.ResStatus=0
        sInfo.FitData.ResFrequency=f
        sInfo.FitData.ResAmplitude = A
        sInfo.FitData.ResPhase = p
        sInfo.FitData.ResMean = M
        sInfo.FitData.ResMethod = Method
     ENDIF
  ENDIF
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
PRO sh_period_startlog,sInfo
  ;; add lines to the log file with the name the mean value, and the
  ;; chi^2
  stdev2 = sh_period_stdev2(sInfo.Data.Values,sInfo.Data.Errors,wmean=wmean)
  new = ['#'+sInfo.Data.Name,'Mean:'+n2s(wmean), $
         'Stdev2:'+n2s(stdev2), $
         '-------------------------------------' $
        ]
  widget_control,sInfo.Handles.wLog,set_value=new
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
PRO sh_period_writelog,sInfo,status=status
  ;; add lines to the log file with the name the mean value, and the
  ;; chi^2
  ;; The current content of the log file
  widget_control,sInfo.Handles.wLog,get_value=log
  ;; append the status if given
  log = [log,'#status:'+strtrim(status,2)]
  openw,lun,sInfo.LogFileName,/append,/get_lun
  printf,lun,transpose(log)
  close,lun
  free_lun,lun
END

;;------------------------------------------------------------------
PRO sh_period_pdmplot,sInfo,reset=reset
  
  IF keyword_set(reset) THEN BEGIN
      sInfo.PDMPlotData.xRange = [0,0]
      sInfo.PDMPlotData.yRange = [0,0]
  ENDIF

  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]

  ;; First see if we need a PDM calculation, either because the data
  ;; has changed or one of the PDM parameters has changed
  IF sInfo.Status.PDMStatus NE 0 THEN BEGIN
      ;; store the results 
      widget_control,sInfo.Handles.wNb,get_value=nb
      widget_control,sInfo.Handles.wNc,get_value=nc
      widget_control,sInfo.Handles.wNfreq,get_value=nfreq
      
      pdm = sh_period_pdm(times,values,errors,nb=nb,nc=nc, $
                          min_freq=sInfo.PDMPlotData.xRange[0], $
                          max_freq=sInfo.PDMPlotData.xRange[1],nfreq=nfreq)
      
      ;; find the minimum value and store it:
      foo = min(pdm[1,*],idx)
      
      ;; Store the Fitted Frequency
      sInfo.FitData.MinFrequency = pdm[0,idx]
                              
      ;; Write the value in the button box
      widget_control,sInfo.Handles.wFMin, $
                     set_value = 'f minimum: '+ $
                     string(format='(F7.5)',pdm[0,idx])
      
      ;; store the results for later use
      sh_period_modify_vardata,sInfo,'PDMfreqs',reform(pdm[0,*])
      sh_period_modify_vardata,sInfo,'PDMthetas',reform(pdm[1,*])
      
      ;; Make the button active
      widget_control,sInfo.Handles.wWhitePDM,/sensitive
      
      sInfo.Status.PDMStatus=0
  ENDIF
  
  oldID =  !D.WINDOW
  wset,sInfo.PDMPlotData.wID
  
  f = (*(sInfo.ptrVarData)).PDMfreqs
  t = (*(sInfo.ptrVarData)).PDMthetas
  
  pl,f,t, $
     psym=-1, ymin=0,title='PDM Analysis', $
     xtitle='frequency ['+sInfo.LCData.Units[0]+'!U-1!N]', $
     ytitle='theta', $
     xrange=sInfo.PDMPlotData.xRange

  sInfo.PDMPlotData.yrange = !y.crange
  
  sInfo.PDMPlotData.x=!x
  sInfo.PDMPlotData.y=!y
  sInfo.PDMPlotData.z=!z
  
  ;; Copy the plotted image to a pixmap
  wset, sInfo.PDMPlotData.pixID
  Device, Copy=[0, 0, sInfo.xsize, sInfo.ysize, 0, 0, sInfo.PDMPlotData.wid]
  
  wset,oldID

END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
PRO sh_period_lcplot,sInfo,reset=reset
  
  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
  
  IF keyword_set(reset) THEN BEGIN
     sInfo.LCPlotData.xRange = [0,0]
     sInfo.LCPlotData.yRange = [min(Values-Errors),max(Values+Errors)]
  ENDIF

  oldID =  !D.WINDOW
  wset,sInfo.LCPlotData.wID
  
  pl,transpose([[times],[values],[errors]]), $
     /error,psym=4, title='Light Curve', $
     xtitle='time ['+sInfo.LCData.Units[0]+']', $
     ytitle='value ['+sInfo.LCData.Units[1]+']', $
     xrange=sInfo.LCPlotData.xrange, $
     yrange=shift(sInfo.LCPlotData.yrange,strupcase(sInfo.LCData.Units[1]) EQ 'MAG')
  
  IF sInfo.Status.MeanStatus NE 0 THEN BEGIN
     sh_period_pdm_meanlightcurve,sInfo
  ENDIF

  IF sInfo.Status.MeanStatus EQ 0 THEN BEGIN
      MeanInphase = (*(sInfo.ptrVarData)).MeanInPhase
      Phase = (*(sInfo.ptrVarData)).Phase
      Freqmean = (*(sInfo.ptrVarData)).FreqMean
      FOR i = floor(!x.crange[0]*FreqMean),floor(!x.crange[1]*FreqMean) DO BEGIN
          pl,(i+phase)/FreqMean,MeanInPhase,ps=0,/oplot,color=100,linestyle=2,thick=2,/noclip
      ENDFOR
  ENDIF
  
  ;; Do we have a succesful fit to show?
  IF sInfo.Status.SinFitStatus EQ 0 THEN BEGIN
     ;; make some time values to have the function properly sampled
     mintim = min(times,max=maxtim)
     manytimes = dindgen(201)/200d0*(maxtim-mintim)+mintim
     sh_period_sin,manytimes, $
                   [sInfo.FitData.SinFitPhase,sInfo.FitData.SinFitfrequency,sInfo.FitData.SinFitamplitude,sInfo.FitData.SinFitMean], $
                   sin,pder
     pl,manytimes,sin,ps=0,/oplot,color=150,linestyle=2,thick=2,/noclip
  ENDIF
  
  sInfo.LCPlotData.x=!x
  sInfo.LCPlotData.y=!y
  sInfo.LCPlotData.z=!z
  
  ;; Create a pixmap. Store its ID. Copy window contents into it.
  wset,sInfo.LCPlotData.pixID
  Device, Copy=[0, 0, sInfo.xsize, sInfo.ysize, 0, 0, sInfo.LCPlotData.wid]

  wset,oldID
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
;; make a plot of the light curve after removing the mean lightcurve
PRO sh_period_resplot,sInfo,reset=reset
  
  ;; Do we need to make a recalculation of the residuals?
  IF sInfo.Status.ResStatus NE 0 THEN BEGIN
      sh_period_residual,sInfo
  ENDIF

  IF sInfo.Status.ResStatus EQ 0 THEN BEGIN
     Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
     Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
     Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
      
      oldID =  !D.WINDOW
      wset,sInfo.RESPlotData.wID
  
;; make the same scale as the LC plot but around 0
      yr = abs(sInfo.LCPlotData.yrange[1]-sInfo.LCPlotData.yrange[0])* $
           [-0.5,0.5]
      pl,transpose([[times],[(*(sInfo.ptrVarData)).Residual],[errors]]), $
         /error,psym=4, title='Residual Light Curve (P='+n2s(1d0/sInfo.FitData.ResFrequency)+')', $
         xtitle='time ['+sInfo.LCData.Units[0]+']', $
         ytitle='value ['+sInfo.LCData.Units[1]+']', $
         yrange=shift(yr,strupcase(sInfo.LCData.units[1]) EQ 'MAG')
      
      sInfo.RESPlotData.x=!x
      sInfo.RESPlotData.y=!y
      sInfo.RESPlotData.z=!z
      
      ;; Create a pixmap. Store its ID. Copy window contents into it.
      wset,sInfo.RESPlotData.pixID
      Device, Copy=[0, 0, sInfo.xsize, sInfo.ysize, 0, 0, sInfo.RESPlotData.wid]
      
      wset,oldID
      
      ;; Update the stdev2 box to indicate the stdev2 after subtracting the
      ;; mean light curve
      stdev2 = sh_period_stdev2((*(sInfo.ptrVarData)).Residual,errors)
      widget_control,sInfo.Handles.wResStdev2,set_value=n2s(stdev2)
      
  ENDIF
  
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; generate a synthethic light curve give the frequency and amplitude
FUNCTION sh_period_sinthetic,sInfo,x,A
  Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
  Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
  Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
  sinthetic_values = A*sin(2d0*!dpi*Times*x)
  
  widget_control,sInfo.Handles.wNb,get_value=nb
  widget_control,sInfo.Handles.wNc,get_value=nc
  widget_control,sInfo.Handles.wNfreq,get_value=nfreq
  
  pdmsin = sh_period_pdm(times,sinthetic_values,errors, $
                         nb=nb,nc=nc, $
                         min_freq=sInfo.PDMPlotData.xRange[0], $
                         max_freq=sInfo.PDMPlotData.xRange[1], $
                         nfreq=nfreq)
  return,pdmsin
END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
;; Simple collector routine to reset status and paramters of various
;; parts of the widget
PRO sh_period_reset,sInfo,all=all, $
                    PDM=PDM, $
                    FitSin=FitSin, $
                    FitTwoSin=FitTwoSin, $
                    FitPar=FitPar, $
                    Residual=Residual, $
                    MeanCurve=MeanCurve, $
                    PlotLC=PlotLC, $
                    PlotPDM=PlotPDM, $
                    PlotRes=PlotRes, $
                    Data=Data, $
                    Log=Log
  
  IF keyword_set(all) OR keyword_set(Log) THEN BEGIN
      sh_period_startlog,sInfo
  ENDIF

  IF keyword_set(all) OR keyword_set(Data) THEN BEGIN
     ;; Restart with the original data
     sInfo.LCData = sInfo.Data
     sInfo.DelIndex = make_array(n_elements(sInfo.Data.Times),value=0)
     widget_control,sInfo.Handles.wUndo,sensitive=0
     sInfo.NUndo = 0
     sInfo.FitData.NSinFits = 0
  ENDIF
  
  IF keyword_set(all) OR keyword_set(PDM) THEN BEGIN
      sInfo.Status.PDMStatus = -1
  ENDIF
  
  IF keyword_set(all) OR keyword_set(MeanCurve) THEN BEGIN
      sInfo.Status.MeanStatus = -1
  ENDIF

  IF keyword_set(all) OR keyword_set(FitSin) THEN BEGIN
      sInfo.Status.SinFitStatus = -1
  ENDIF
  
  IF keyword_set(all) OR keyword_set(FitTwoSin) THEN BEGIN
      sInfo.Status.TwoSinFitStatus = -1
  ENDIF
  
  IF keyword_set(all) OR keyword_set(FitPar) THEN BEGIN
      sInfo.Status.ParStatus = -1
  ENDIF
  
  IF keyword_set(all) OR keyword_set(Residual) THEN BEGIN
     sInfo.Status.ResStatus = -1
  ENDIF

  IF keyword_set(all) OR keyword_set(PlotLC) THEN BEGIN
     sh_period_lcplot,sInfo,/reset
  ENDIF
  
  IF keyword_set(all) OR keyword_set(PlotPDM) THEN BEGIN
      sh_period_PDMplot,sInfo,/reset
  ENDIF
  
  IF keyword_set(all) OR keyword_set(PlotRes) THEN BEGIN
      sh_period_ResPlot,sInfo,/reset
  ENDIF

END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; Initialise the widget by calculating the PDM, mean light curve and residuals
PRO sh_period_init,sInfo,field,value
  
  ;; Do the first PDM calculation
  sh_period_pdmplot,sInfo,/reset
  
  ;; Make the default minimum freq button active
  widget_control,sInfo.Handles.wFMin,/set_button
  
  ;; Calculate the mean light curve
  sh_period_pdm_meanlightcurve,sInfo
  
  ;; try to fit a parabola around the minimum
  fit = sh_period_fit_parabola(sInfo,sInfo.FitData.MinFrequency)
  
  ;; if the fit was succesful then
  IF sInfo.Status.ParStatus EQ 0 THEN BEGIN
     ;; Make the button active
     widget_control,sInfo.Handles.wFPar,/sensitive,/set_button
   ENDIF
  
  ;; try to fit a sinus 
  ;; Make the button active
  widget_control,sInfo.Handles.wWhiteSin,/set_button
  sh_period_residual,sInfo
  
  ;; if the fit was succesful then
  IF sInfo.Status.SinFitStatus EQ 0 THEN BEGIN
     ;; Make the button active
     widget_control,sInfo.Handles.wWhiteSin,/sensitive
  ENDIF ELSE BEGIN
     widget_control,sInfo.Handles.wWhiteSin,sensitive=0
     widget_control,sInfo.Handles.wWhitePDM,/sensitive,/set_button
  ENDELSE

  sh_period_reset,sInfo,/plotLC,/plotRES

END
;;------------------------------------------------------------------

;;------------------------------------------------------------------
PRO sh_period_event, sEvent
  
  ;; Which event?
  WIDGET_CONTROL, sEvent.id, GET_UVALUE=eventUValue
  
  ;; get the info structure from the top
  WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo,/NO_COPY
  
  CASE eventUValue OF
      
      'LCPLOT': BEGIN

          ;; Deal only with DOWN, UP, and MOTION events.
          IF sEvent.type GT 2 THEN RETURN
          
          ;; Make sure we have the proper device selected
          oldID =  !D.WINDOW
          WSet, sInfo.LCPlotData.wId

          !x=sInfo.LCPlotData.x
          !y=sInfo.LCPlotData.y
          !z=sInfo.LCPlotData.z
          
          foo = convert_coord([sEvent.x],[sEvent.y],/device,/to_data)
          x = foo[0]
          y = foo[1]
          
          ;; What kind of event is this?
          eventTypes = ['DOWN', 'UP', 'MOTION']
          thisEvent = eventTypes[sEvent.type]
          
          CASE thisEvent OF
              
              'DOWN': BEGIN
                  ;; Turn motion events on for the draw widget.
                  Widget_Control, sInfo.LCPlotData.wLCPlot, Draw_Motion_Events=1
                  
                  ;; Get and store the static corner of the box.
                  sInfo.LCPlotData.sx = x
                  sInfo.LCPlotData.sy = y
              ENDCASE
              
              'UP': BEGIN
                  xmin = Min([x,sInfo.LCPlotData.sx], Max=xmax)
                  ymin = Min([y,sInfo.LCPlotData.sy], Max=ymax)
                  
                  Device, Copy=[0, 0, sInfo.xsize, sInfo.ysize, 0, 0, sInfo.LCPlotData.pixID]
                  wset,oldID
                  
                  ;; Turn draw motion events off. Clear any events queued for widget.
                  Widget_Control, sInfo.LCPlotData.wLCPlot, Draw_Motion_Events=0, Clear_Events=1
                  
                  CASE sEvent.Release OF
                      ;; Zoom 
                      1: BEGIN
                          ;; Order the box coordinates.
                          sInfo.LCPlotData.xrange = [xmin,xmax]
                          sInfo.LCPlotData.yrange = [ymin,ymax]
                          sh_period_lcplot,sInfo
                      END
                      ;; Right button zap data
                      4: BEGIN
                         ;; We want to remove some points from the LC data
                         Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
                         Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
                         
                         delete = (Times GT xmin) AND $
                                  (Times LT xmax) AND $
                                  (Values GT ymin) AND $
                                  (Values LT ymax)
                         
                         didx = where(delete EQ 1,cnt)
                         kidx = where(delete EQ 0)
                         
                         ;; update the Variable Data if there are
                         ;; data left and some point have been selected
                         ntot = n_elements(Times)
                         IF (cnt NE 0) AND (cnt NE ntot) THEN BEGIN

                            sInfo.NUndo = sInfo.NUndo+1
                            
                            ;; now we have to translate the index of
                            ;; the selected points to the original
                            ;; index:
                            sInfo.DelIndex[(where(sInfo.DelIndex EQ 0))[didx]] = sInfo.NUndo

                            widget_control,sInfo.Handles.wUndo,sensitive=1
                            sh_period_lcplot,sInfo
                            
                            ;; Add lines to log file
                            ;;widget_control,sInfo.Handles.wLog,set_value=new,/append
                            
                            ;; Make sure the status of the
                            ;; Subsequent windows is
                            ;; modified since we are dealing with
                            ;; 'new' data
                            sh_period_reset,sInfo,/PDM,/FitPar,/FitSin, $
                                            /Residual,/PlotLC,/PlotPDM,/PlotRes
                         ENDIF
                      END
                      ELSE: BEGIN
                      END
                  ENDCASE
              ENDCASE
              
              'MOTION': BEGIN
                  
                  ;; Here is where the actual box is drawn and erased.
                  ;; First, erase the last box.
                  Device, Copy=[0, 0, sInfo.xsize, sInfo.ysize, 0, 0, sInfo.LCPlotData.pixID]
                  ;; Get the coodinates of the new box and draw it.
                  sx = sInfo.LCPlotData.sx
                  sy = sInfo.LCPlotData.sy
                  oplot, [sx, sx, x, x, sx], [sy, y, y, sy, sy], $
                         Color=sInfo.LCPlotData.boxColor
              ENDCASE
              
          ENDCASE
          wset,oldID
          
          ;; Store the info structure.
          
      END
      
      'PDMPLOT': BEGIN
          
          ;; Deal only with DOWN, UP, and MOTION events.
          IF sEvent.type GT 2 THEN RETURN
          
          ;; Make sure we have the proper device selected
          oldID =  !D.WINDOW
          WSet, sInfo.PDMPlotData.wId

          ;; restore the coordinates
          !x=sInfo.PDMPlotData.x
          !y=sInfo.PDMPlotData.y
          !z=sInfo.PDMPlotData.z
          
          foo = convert_coord([sEvent.x],[sEvent.y],/device,/to_data)
          x = foo[0]
          y = foo[1]
          
          ;; What kind of event is this?
          eventTypes = ['DOWN', 'UP', 'MOTION']
          thisEvent = eventTypes[sEvent.type]
          
          CASE thisEvent OF
              
              'UP': BEGIN
                  ;; Now the have 3 options.
                  ;; Left button Zoom on this region and make a more
                  ;; precise PDM analysis

                  ;; Middle button use frequency to make mean lightcurve
                  ;; and synthesized PDM

                  ;; Right button fit a parabola to this valley 
                  
                  ;; Left button
                  CASE sEvent.Release OF 
                      1: BEGIN
                          ;; fit a parabola locally around the
                          ;; selected frequency
                          fit = sh_period_fit_parabola(sInfo,x)

                          ;; if the fit was succesful then
                          IF sInfo.Status.ParStatus EQ 0 THEN BEGIN
                             ;; Make the button active
                             widget_control,sInfo.Handles.wFPar,/set_button
                             
                             ;; Calculate the mean light curve
                             sh_period_pdm_meanlightcurve,sInfo
                             
                             ;;  update PDM plot
                             pl,fit,/opl,ps=0,thick=2,col=100
                             
                             ;; Fit a sinus using the selected frequency
                             sh_period_fit_sin,sInfo,freq=sInfo.FitData.ParFrequency,ampl=Amplitude,mean=mean, $
                                               /fitphase
                             
                             ;; replot the lightcurve with the mean lightcurve
                             ;; overplotted.
                             sh_period_reset,sInfo,/MeanCurve,/Residual, $
                                             /PlotLC,/PlotRes
                          ENDIF
                      END
                      2: BEGIN
                          ;; Store the selected Frequency
                          sInfo.FitData.SelectedFrequency = x
                          
                          ;; Write the value in the button box
                          widget_control,sInfo.Handles.wFMou, $
                                         set_value = 'f mouse: '+ $
                                         string(format='(F7.5)',x), $
                                         /sensitive,/set_button
                          
                          ;; Calculate the mean light curve
                          sh_period_pdm_meanlightcurve,sInfo

                          ;; now we also want to calculate a synthetic PDM
                          ;; using a single sin wave with the same amplitude 
                          PDMMean = (*(sInfo.ptrvardata)).PDMmean
                          mmin = min(PDMMean,max=mmax)
                          Amplitude = (mmax-mmin)/2d0
                          mean = (mmax+mmin)/2d0

                          pl,sh_period_sinthetic(sInfo,x,Amplitude),/opl,ps=0
                          
                          ;; Fit a sinus using the selected frequency
                          sh_period_fit_sin,sInfo,freq=x,ampl=Amplitude,mean=mean, $
                                            /fitphase
                          
                          ;; replot the lightcurve with the mean lightcurve
                          ;; overplotted.
                          sh_period_reset,sInfo,/MeanCurve,/Residual, $
                                          /PlotLC,/PlotRes
                      END
                      4: BEGIN
                          
                          ;; Make a zoom of a factor ~2
                          old_xr = !x.crange[1]-!x.crange[0]

                          ;; try to make the range a factor 2 smaller
                          ;; but do not go beyond the present
                          ;; boundaries, that should be good limits
                          new_xr=[((x-old_xr/4d0)>!x.crange[0]),((x+old_xr/4d0)<!x.crange[1])]
                          sInfo.PDMPlotData.xrange = new_xr

                          ;; Announce that the current PDM calculation
                          ;; is not good anymore
                          sh_period_reset,sInfo,/PDM,/FitPar
                          sh_period_pdmplot,sInfo
                      END
                  ENDCASE
              ENDCASE
              
              'DOWN': BEGIN
              ENDCASE
              
              'MOTION': BEGIN
                  
                  ;; Here is where the indicative lines are drawn and erased.
                  ;; First, erase the last box.
                  Device, Copy=[0, 0, sInfo.xsize, sInfo.ysize, 0, 0, sInfo.PDMPlotData.pixID]
                  
                  oplot, [x,x], sInfo.PDMPlotData.yrange,  $
                         Color=sInfo.PDMPlotData.FreqColor
                  
                  ;; One year aliases
                  oplot, [x+1d0/365d0,x+1d0/365d0], sInfo.PDMPlotData.yrange , $
                         Color=sInfo.PDMPlotData.AliasColor,linestyle=2
                  oplot, [x-1d0/365d0,x-1d0/365d0], sInfo.PDMPlotData.yrange , $
                         Color=sInfo.PDMPlotData.AliasColor,linestyle=2
                  
                  ;; One year aliases
                  oplot, [x*2d0,x*2d0], sInfo.PDMPlotData.yrange , $
                         Color=sInfo.PDMPlotData.HarmColor,linestyle=2
                  oplot, [x/2d0,x/2d0], sInfo.PDMPlotData.yrange , $
                         Color=sInfo.PDMPlotData.HarmColor,linestyle=2
                  
              ENDCASE
              
          ENDCASE
          wset,oldID
          ;; Store the info structure.
          
      END
      
      'RESTART': BEGIN
          sh_period_reset,sInfo,/all
      END
      
      'UNZOOM': BEGIN
;          Times  = sInfo.LCData.Times [where(sInfo.DelIndex EQ 0)]
;          Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
;          Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
;          sInfo.LCPlotData.xrange = [min(Times),max(Times)]
;          sInfo.LCPlotData.yrange = [min(Values-Errors),max(Values+Errors)]
         sh_period_reset,sInfo,/PlotLC
       END
      
      'UNDO': BEGIN
         sInfo.DelIndex[where(sInfo.DelIndex EQ sInfo.NUndo)] = 0
         sInfo.NUndo = sInfo.NUndo -1
         
         IF sInfo.NUndo EQ 0 THEN BEGIN
            widget_control,sInfo.Handles.wUndo,sensitive=0
         ENDIF
         
         ;; Make sure the status of the
         ;; Subsequent windows is
         ;; modified since we are dealing with
         ;; 'new' data
         sh_period_reset,sInfo,/PDM,/FitPar,/FitSin, $
                         /Residual,/PlotPDM,/PlotRes, $
                         /PlotLC
      END
      
      'PDMSLIDE': BEGIN
          sInfo.Status.PDMStatus = -1
          sh_period_pdmplot,sInfo
      END
      
      'PDMUNZOOM': BEGIN
          sh_period_reset,sInfo,/pdm,/plotPDM
      END
      
      'FREQSEL': BEGIN
         sh_period_reset,sInfo, $
                         /Residual, $
                         /MeanCurve, $
                         /PlotLC, $
                         /PlotRes
      END
      
      'WHITESEL': BEGIN
         sh_period_reset,sInfo,/Residual,/PlotRes
      END
      
      'WHITEN': BEGIN
         ;; replace the gooddata value by the mean subtracted
         IF sInfo.Status.ResStatus NE 0 THEN BEGIN
            sh_period_residual,sInfo
         ENDIF
         
         IF sInfo.Status.ResStatus EQ 0 THEN BEGIN

            ;; Replace the working data by the data with the
            ;; fitted/mean curve removed
            sInfo.LCData.Values = (*(sInfo.ptrVarData)).Residual

            ;; Add the found frequency, period and stdev2 after to the log
            Values = sInfo.LCData.Values[where(sInfo.DelIndex EQ 0)]
            Errors = sInfo.LCData.Errors[where(sInfo.DelIndex EQ 0)]
            stdev2 = sh_period_stdev2(Values,Errors)
            
            CASE strupcase(sInfo.FitData.ResMethod) OF
               'SINE FIT': BEGIN
                  new = [ $
                        'Whitened using:'+sInfo.FitData.ResMethod, $
                        'Frequency:'+n2s(sInfo.FitData.ResFrequency), $
                        'Period:'+n2s(1d0/sInfo.FitData.ResFrequency), $
                        'Stdev2:'+n2s(stdev2), $
                        'Amplitude:'+n2s(sInfo.FitData.ResAmplitude), $
                        '-------------------------------------' $
                        ]
                  ;; Store the paramters from the removed curve for later
                  ;; reference
                  CASE sInfo.FitData.NSinFits OF 
                     0: BEGIN
                        sInfo.FitData.SinFit1Phase     = sInfo.FitData.ResPhase     
                        sInfo.FitData.SinFit1frequency = sInfo.FitData.Resfrequency 
                        sInfo.FitData.SinFit1amplitude = sInfo.FitData.Resamplitude 
                        sInfo.FitData.SinFit1Mean      = sInfo.FitData.ResMean      
                        sInfo.FitData.NSinFits = 1
                     END
                     1: BEGIN
                        sInfo.FitData.SinFit2Phase     = sInfo.FitData.ResPhase     
                        sInfo.FitData.SinFit2frequency = sInfo.FitData.Resfrequency 
                        sInfo.FitData.SinFit2amplitude = sInfo.FitData.Resamplitude 
                        sInfo.FitData.SinFit2Mean      = sInfo.FitData.ResMean      
                        sInfo.FitData.NSinFits = 2
;;                        widget_control,sInfo.Handles.wWhiteTwoSin,/sensitive
                        widget_control,sInfo.Handles.wTwoSin,/sensitive
                        widget_control,sInfo.Handles.wIterTwoSin,/sensitive
                     END
                     ELSE: BEGIN
                     END
                  ENDCASE
                  
               END
               'PDM CURVE': BEGIN
                  new = [ $
                        'Whitened using:'+sInfo.FitData.ResMethod, $
                        'Frequency:'+n2s(sInfo.FitData.ResFrequency), $
                        'Period:'+n2s(1d0/sInfo.FitData.ResFrequency), $
                        'Stdev2:'+n2s(stdev2), $
                        'Amplitude:'+n2s(sInfo.FitData.ResAmplitude), $
                        '-------------------------------------' $
                        ]
               END
               'DOUBLE SINE FIT': BEGIN
                  sh_period_startlog,sInfo
                  new = [ $
                        'Whitened using:'+sInfo.FitData.ResMethod, $
                        'First Frequency:'+n2s(sInfo.FitData.SinFit1Frequency), $
                        'First Period:'+n2s(1d0/sInfo.FitData.SinFit1Frequency), $
                        'First Amplitude:'+n2s(sInfo.FitData.SinFit1Amplitude), $
                        'Second Frequency:'+n2s(sInfo.FitData.SinFit2Frequency), $
                        'Second Period:'+n2s(1d0/sInfo.FitData.SinFit2Frequency), $
                        'Second Amplitude:'+n2s(sInfo.FitData.SinFit2Amplitude), $
                        'Stdev2:'+n2s(stdev2), $
                        '-------------------------------------' $
                        ]
               END
               ELSE: BEGIN
               END

            ENDCASE
            
            widget_control,sInfo.Handles.wLog,set_value=new,/append
            
            ;; make sure the old values are no longer used
            sh_period_reset,sInfo, $
                            /Residual, $
                            /PDM, $
                            /MeanCurve, $
                            /FitPar, $
                              /FitSin
            sh_period_init,sInfo
         ENDIF
      END
      
      'TWOSINFIT' : BEGIN
         sh_period_fit_TwoSin,sInfo
         IF sInfo.Status.TwoSinFitStatus EQ 0 THEN BEGIN
            widget_control,sInfo.Handles.wWhiteTwoSin,/sensitive,/set_button
            sh_period_reset,sInfo,/Residual,/PlotRes
         ENDIF
      END             
      
      'ITERTWOSIN' : BEGIN
         widget_control,sInfo.Handles.wTolIterTwoSin,get_value=tol
         
         ;; Do the first step
         sh_period_fit_TwoSin,sInfo
         IF sInfo.Status.TwoSinFitStatus EQ 0 THEN BEGIN
            
            relchange = 2*tol
            
            ;; remember the previous values
            OldSinFit1Frequency = sInfo.FitData.SinFit1Frequency
            OldSinFit1Amplitude = sInfo.FitData.SinFit1Amplitude
            OldSinFit2Frequency = sInfo.FitData.SinFit2Frequency
            OldSinFit2Amplitude = sInfo.FitData.SinFit2Amplitude
               
            WHILE (sInfo.Status.TwoSinFitStatus EQ 0) AND (relchange GT tol) DO BEGIN

               sh_period_fit_TwoSin,sInfo
               relchange = max([ $          
                           abs(OldSinFit1Frequency/sInfo.FitData.SinFit1Frequency-1d0), $
                           abs(OldSinFit1Amplitude/sInfo.FitData.SinFit1Amplitude-1d0), $
                           abs(OldSinFit2Frequency/sInfo.FitData.SinFit2Frequency-1d0), $
                           abs(OldSinFit2Amplitude/sInfo.FitData.SinFit2Amplitude-1d0) $
                               ])

               ;; remember the previous values
               OldSinFit1Frequency = sInfo.FitData.SinFit1Frequency
               OldSinFit1Amplitude = sInfo.FitData.SinFit1Amplitude
               OldSinFit2Frequency = sInfo.FitData.SinFit2Frequency
               OldSinFit2Amplitude = sInfo.FitData.SinFit2Amplitude
            ENDWHILE
         ENDIF
         
         IF sInfo.Status.TwoSinFitStatus EQ 0 THEN BEGIN
            widget_control,sInfo.Handles.wWhiteTwoSin,/sensitive,/set_button
            sh_period_reset,sInfo,/Residual,/PlotRes
         ENDIF
      END             
      
      'SUCCES' : BEGIN
         sh_period_writelog,sInfo,status='SUCCESS'
         ptr_free, sInfo.ptrVarData
         *(sInfo.Status.ptrExitStatus)='SUCCESS'
         WIDGET_CONTROL, sEvent.top, /DESTROY
         return
      END             
      
      'FAILED' : BEGIN
         sh_period_writelog,sInfo,status='FAILED'
         *(sInfo.Status.ptrExitStatus)='FAILED'
         ptr_free, sInfo.ptrVarData
         WIDGET_CONTROL, sEvent.top, /DESTROY
         return
      END             
      
      'STOP' : BEGIN
         ptr_free, sInfo.ptrVarData
         *(sInfo.Status.ptrExitStatus)='STOP'
         WIDGET_CONTROL, sEvent.top, /DESTROY
         return
      END             
      
      'HELP' : BEGIN
          htxt = 'Display and/or read lnk data from local database'
          htxt = [htxt,'Use the list in the middle to display absorption values of the materials']
          htxt = [htxt,'The column on the left (constraints) can be used to limit the materials']
          htxt = [htxt,'Select a source to only show the data comming from that particular database']
          htxt = [htxt,'Material are either in descriptive form (amorphous olivine) or']
          htxt = [htxt,'  in chemical notation (Al2O3), depending on the database.']
          htxt = [htxt,'Filename selects on the name of the .lnk file like Ag.lnk']
          htxt = [htxt,'Comments is most general and can be used to select data from a certain'] 
          htxt = [htxt,'  reference (jaeger) or with a certain keyword (temperature)']
          htxt = [htxt,'Wavelength 1/2 can be used to select only data that bracket those numbers']
          htxt = [htxt,'  in micrometer. Use both to ensure a range to be present (e.g. 2,45)'] 
          htxt = [htxt,'  reference (jaeger) or with a certain keyword (temperature)']
          htxt = [htxt,'']
          htxt = [htxt,'In anycase a certain familiarity with the type of data is needed']
          htxt = [htxt,'The fullpath box shows the place where the data can be read from.']
          htxt = [htxt,'The plotting command can be adapted to taste but probable plq is most useful.']
          htxt = [htxt,'Useful options are /CDE,/NEEDLE,/DISK to choose a shape (distibution)']
          htxt = [htxt,'                   TEMP=1000,RADIUS=0.1']
          htxt = [htxt,'Use /help for general plotting options; output in the IDL window.']
          htxt = [htxt,'']
          htxt = [htxt,'By Double clicking a material the widget is exited and the clicked data is']
          htxt = [htxt,'returned (in read_lnk mode).']

          display_text_widget,htxt,title='Read LNK Help'
     
      END ;; help

      ELSE:                     ;  do nothing
  ENDCASE

  WIDGET_CONTROL, sEvent.top, SET_UVALUE=sInfo,/NO_COPY
  
END
;;------------------------------------------------------------------


;;------------------------------------------------------------------
;; Widget program to identify and whiten the data to find further
;; periodicities
;; The function takes a with tags {times, values, errors, units,
;; name}. 

PRO sh_period,par,no_block=no_block,logfilename=logfilename,status=status
  
;; if this is set then do not block the widget but instead return
;; right after creating the widget
  default,no_block,0
  
;; check the input
  IF n_elements(par) EQ 0 THEN return
  IF size(par,/tname) NE 'STRUCT' THEN return
  tags = strupcase(tag_names(par))

  ;; these we absolutely need to do anything
  IF total(tags EQ 'TIMES') EQ 0 THEN return
  times = par.times
  IF  total(tags EQ 'VALUES') EQ 0 THEN return
  values = par.values
  ;; These would be nice to have too but other wise fudge then
  IF  total(tags EQ 'ERRORS') EQ 0 THEN errors = make_array(n_elements(values),value=1d0) ELSE errors = par.errors
  IF  total(tags EQ 'UNITS') EQ 0 THEN units = ['','',''] ELSE units = par.units
  IF  total(tags EQ 'NAME') EQ 0 THEN name = 'anonymous' ELSE name=par.name
  
  data = {times:times,values:values,errors:errors,units:units,name:name}

  default,logfilename,'sh_period_logfile.txt'

;; now create the widget
  wbase = widget_base(title="sh_period", /column, map=0, mbar=barbase)
  
;; create the TOP menu
  wfilebutton = widget_button(barbase, value='File')
  wOpenLogbutton = widget_button(wfilebutton, value='Open Log file', uvalue='OPENLOG',/separator)
  wSucces = WIDGET_BUTTON(wfilebutton, $
                          VALUE='Quit with Success',  $
                          UVALUE='SUCCES',accelerator="Alt+s")

  wFail = WIDGET_BUTTON(wfilebutton, $
                        VALUE='Quit with Failed',  $
                        UVALUE='FAILED',accelerator="Alt+f")

  wStop = WIDGET_BUTTON(wfilebutton, $
                        VALUE='Quit and Stop',  $
                        UVALUE='STOP',accelerator="Alt+c")
  
  whelpbutton = widget_button(barbase, value='Help')
  whelpbutton = widget_button(whelpbutton, value='Display help', uvalue='HELP')
  
  
;;; change the default font
;  widget_control, wbase, $                            
;                  default_font="-adobe-helvetica-bold-r-normal-*-14-100-*-*-*-*-*"


;; The complete layout for the widget is in a row
  wmainbase = widget_base(wbase, /column)
  
  ;; we divide the layout in 4 parts
  wTbase =  widget_base(wbase , /row)
  wBbase =  widget_base(wbase , /row)


  screen_geometry = get_screen_size()
  x_scr= screen_geometry[0]/3d0
  y_scr= screen_geometry[1]/3d0
  
  ;; the dimensions of a single plot windows
  xsize=floor(0.8*x_scr)
  ysize=floor(0.8*y_scr)
  
  ;; This holds the lightcurve
  wTLbase = widget_base(wTbase, /column,/frame,scr_xsize=x_scr,scr_ysize=y_scr)
  ;; This holds the pdm analysis
  wTRbase = widget_base(wTbase, /column,/frame,scr_xsize=x_scr,scr_ysize=y_scr)
  ;; This shows the residu window
  wBLbase = widget_base(wBbase, /column,/frame,scr_xsize=x_scr,scr_ysize=y_scr)
  ;; General control window or FFT if we manage to do it
  wBRbase = widget_base(wBbase, /column,/frame,scr_xsize=x_scr,scr_ysize=y_scr)

  
  ;; Now the basic components in each quarter:
  ;;wTRbase:
  w = widget_base(wTLbase, /row)
  ww = widget_base(w, /column)
  wLCPlot = widget_draw(ww,uvalue='LCPLOT', $
                        xsize=xsize,ysize=ysize, Button_Events=1)
  www = widget_label(ww, value='Button-1:Zoom -- Button-2: '+ $
                     ' nothing -- Button-3:Delete Points',/frame)
  
  ww = widget_base(w, /column)
  wUnZoom = widget_button(ww, value='Unzoom', uvalue='UNZOOM')
  wRestart  = widget_button(ww, value='Restart', uvalue='RESTART')
  wUndo  = widget_button(ww, value='Undo', uvalue='UNDO',SENSITIVE=0)
  
  ;;wTRbase:
  w = widget_base(wTRbase, /row)
  ww = widget_base(w, /column)
  wPDMPLOT = widget_draw(ww,uvalue='PDMPLOT', $
                         xsize=xsize,ysize=ysize, Button_Events=1, Motion_Events=1)
  www = widget_label(ww, value='Button-1:Fit Parabola -- Button-2: Select'+ $
                     ' Frequency -- Button-3:Zoom',/frame)

  ww = widget_base(w, /column)
  wPDMUnZoom = widget_button(ww, value='Unzoom', uvalue='PDMUNZOOM')
  wNb = WIDGET_SLIDER(ww, MINIMUM=5,  $
                      MAXIMUM=25, VALUE=11, $
                      TITLE='Nb', UVALUE='PDMSLIDE')
  wNc = WIDGET_SLIDER(ww, MINIMUM=1,  $
                      MAXIMUM=5, VALUE=2, $
                      TITLE='Nc', UVALUE='PDMSLIDE')
  wNfreq = WIDGET_SLIDER(ww, MINIMUM=0,  $
                         MAXIMUM=200, VALUE=0, $
                         TITLE='Nfreq', UVALUE='PDMSLIDE')
  ww = WIDGET_BASE(wTRBase, /exclusive,/row)

  wFMin = WIDGET_BUTTON(ww, $
                        VALUE='f minimum: 0.00000',  $
                        UVALUE='FREQSEL')

  wFMou = WIDGET_BUTTON(ww, $
                        VALUE='f mouse: 0.00000',  $
                        UVALUE='FREQSEL',sensitive=0)

  wFPar = WIDGET_BUTTON(ww, $
                        VALUE='f parabola: 0.00000',  $
                        UVALUE='FREQSEL',sensitive=0)
  
  wFSin = WIDGET_BUTTON(ww, $
                        VALUE='f sinus: 0.00000',  $
                        UVALUE='FREQSEL',sensitive=0)
  
  ;;wBLbase:
  w = widget_base(wBLbase, /row)
  ww = widget_base(w, /column)
  wRESPLOT = widget_draw(ww,uvalue='RESPLOT', $
                         xsize=xsize,ysize=ysize, Button_Events=1)
;  www = widget_label(ww, value='Button-1: -- Button-2:'+ $
;                     ' -- Button-3: ',/frame)
  
  ww = widget_base(w, /column)
  wWhiten = widget_button(ww, value='Whiten', uvalue='WHITEN')
  www = widget_label(ww, value='Chi^2')
  wResStdev2 = widget_text(ww, value='',xsize=10)

  ww = WIDGET_BASE(wBLBase, /exclusive,/row)

  wWhiteSin = WIDGET_BUTTON(ww, $
                            VALUE='Sine',  $
                            UVALUE='WHITESEL',sensitive=0)
  
  wWhitePDM = WIDGET_BUTTON(ww, $
                            VALUE='PDM',  $
                            UVALUE='WHITESEL')

  wWhiteTwoSin = WIDGET_BUTTON(ww, $
                               VALUE='Two Sine',  $
                               UVALUE='WHITESEL',sensitive=0)
  
  widget_control,wWhitePDM,/set_button

;  ;;wBRbase:
  w = widget_base(wBRbase, /row)
  ww = widget_base(w, /column,/frame)
  www = widget_label(ww, value='Log')
  wLog = widget_text(ww,uvalue='LOG',xsize=60,ysize=15,/editable)
  
  ww = widget_base(w, /column)
  wTwoSin = widget_button(ww, value='Double Sine Fit', $
                        uvalue='TWOSINFIT',sensitive=0)
  
  wIterTwoSin = widget_button(ww, value='Interate DSine', $
                              uvalue='ITERTWOSIN',sensitive=0)
  
  wTolIterTwoSin =  CW_FSLIDER(ww, MINIMUM=0.0001,  $
                               MAXIMUM=0.01, $
                               VALUE=0.001, $
                               format='(F7.5)', $
                               TITLE='Tolerance', $
                               UVALUE='TOLITERTWOSIN')
  
  w = WIDGET_BASE(wBRBase,/row,/frame)
  
  wSucces = WIDGET_BUTTON(w, $
                          VALUE='Success (Alt+s)',  $
                          UVALUE='SUCCES')
  
  wFail = WIDGET_BUTTON(w, $
                        VALUE='Failed (Alt+f)',  $
                        UVALUE='FAILED')

  wStop = WIDGET_BUTTON(w, $
                        VALUE='Stop (Alt+c)',  $
                        UVALUE='STOP')
  
  
  
  ;;  realize the widget hierarchy.
  widget_control, wbase, /realize

  ;; ask these id only after the widget has been realised
  WIDGET_CONTROL, wLCPlot, GET_VALUE=LCPlotID
  WIDGET_CONTROL, wPDMPlot, GET_VALUE=PDMPlotID
  WIDGET_CONTROL, wRESPlot, GET_VALUE=RESPlotID
  
  ;; create the structure that can hold variable sized data, like the
  ;; good points to use for fittin.
  ptrVarData = ptr_new({dummy:0})
  
  ;; Create a pointer to hold the exitstatus
  ptrExitStatus = ptr_new('FAILED')
  
  oldID = !D.window
  Window, /Free, /Pixmap, $
          XSize=xsize, $
          YSize=ysize
  
  ;; Data for the LC Plot
  LCPlotData = { $
               wLCPlot:wLCPlot, $
               wID:LCPlotID, $
               xrange:[0d0,0d0], $
               yrange:[min(data.Values-Data.Errors),max(data.Values+Data.Errors)], $
               BoxColor:100, $
               pixID:!D.Window, $
               sx:-1d0, $
               sy:-1d0, $
               x:!x, $
               y:!y, $
               z:!z, $
               dummy:0}
  
  Window, /Free, /Pixmap, $
          XSize=xsize, $
          YSize=ysize
  
  ;; Data for the PDM calculation
  PDMPlotData = { $
                wPDMPlot:wPDMPlot, $
                wID:PDMPlotID, $
                pixID:!D.window, $
                xrange:[0d0,0d0], $
                yrange:[0d0,0d0], $
                FreqColor:100, $
                AliasColor:200, $
                HarmColor:100, $
                x:!x, $
                y:!y, $
                z:!z, $
                dummy:0}
  
  Window, /Free, /Pixmap, $
          XSize=xsize, $
          YSize=ysize
  
  ;; Data for the RES calculation
  RESPlotData = { $
                wRESPlot:wRESPlot, $
                wID:RESPlotID, $
                pixID:!D.window, $
                xrange:[0d0,0d0], $
                yrange:[0d0,0d0], $
                x:!x, $
                y:!y, $
                z:!z, $
                dummy:0}
  
  wset,oldID

;; hold the handles of the various widgets  
  Handles = { $
            wUndo: wUndo, $
            wNb: wNb, $
            wNc: wNc, $
            wNfreq: wNfreq, $
            wFMin: wFMin, $
            wFMou: wFMou, $
            wFPar: wFPar, $
            wFSin: wFSin, $
            wTwoSin: wTwoSin, $
            wIterTwoSin: wIterTwoSin, $
            wTolIterTwoSin: wTolIterTwoSin, $
            wWhiteSin: wWhiteSin, $
            wWhiteTwoSin: wWhiteTwoSin, $
            wWhitePDM: wWhitePDM, $
            wLog: wLog, $
            wResStdev2: wResStdev2, $
            dummy:0}
  
  Status = { $
           ParStatus: -1, $
           SinFitStatus: -1, $
           TwoSinFitStatus: -1, $
           ResStatus: -1, $
           PDMStatus: -1, $
           MeanStatus: -1, $
           ptrExitStatus: ptrExitStatus, $
           dummy:0}
  
  ;; piece of info derived from the diagnostics
  FitData = { $
            ;; The freq selected with the mouse
            SelectedFrequency: 0d0, $
            ;; The minimum chi2 in the PDM window
            MinFrequency: 0d0, $
            ;; The minimum from the parabola fit to the PDM
            ParFrequency: 0d0, $
            ;; From the sin fit 
            SinFitfrequency: 0d0, $
            SinFitPhase: 0d0, $
            SinFitamplitude: 0d0, $
            SinFitMean: 0d0, $
            ;; From the sin fit to do a multi sine fit
            SinFit1frequency: 0d0, $
            SinFit1Phase: 0d0, $
            SinFit1amplitude: 0d0, $
            SinFit1Mean: 0d0, $
            ;; From the sin fit 
            SinFit2frequency: 0d0, $
            SinFit2Phase: 0d0, $
            SinFit2amplitude: 0d0, $
            SinFit2Mean: 0d0, $
            NSinFits: 0, $
            ;; The frequency used to calculate the residue
            ResFrequency: 0d0, $
            ResAmplitude: 0d0, $
            ResPhase: 0d0, $
            ResMean: 0d0, $
            ResMethod: '', $
            dummy:0}
  
;;  create the info structure to pass all info between routines
  sinfo = { $
          ;; Structures
          Handles: Handles, $
          Status: Status, $
          LCPlotData: LCPlotData, $
          PDMPlotData: PDMPlotData, $
          ResPlotData: ResPlotData, $
          FitData: FitData, $
          ptrVarData: ptrvardata, $
          ;; Global variables
          LogFileName: LogFileName , $
          xSize: xsize, $
          ySize: ysize, $
          Data: Data, $
          DelIndex: make_array(n_elements(Data.Times),value=0), $
          LCData: Data, $
          NUndo: 0, $
          dummy:'' $
          }
  
  ;;  map the top level base.
  widget_control, wbase, map=1
  
  sh_period_startlog,sInfo

  sh_period_init,sInfo
;;; store the sinfo in the uvalue of the top widget
  widget_control, wbase, set_uvalue=sinfo, /no_copy
  
  ;; register with the big guy, xmanager!
  xmanager, "sh_period", wbase, $
            event_handler="sh_period_event", $
            no_block=no_block

  ;; Only test the exit status and free the variable data if we did
  ;; block the widget
  IF no_block EQ 0 THEN BEGIN
     ;; things to get back out of the widget
     Status = *ptrExitStatus
     ptr_free, ptrExitStatus
  ENDIF
END
