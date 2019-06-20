;;*****************************************************************************
;;*
;;*             COMPUTE THE ERROR ON THE RATIO OF TWO VARIABLES
;;*
;;*****************************************************************************


;; Compute the error on the ratio a/b, knowing the asymmetric
;; independent error on each variable and the correlated calibration
;; error on both 

;;(SH May  4 2011) corrected testing for upper and lower limits
;;(SH May  4 2011) made a matrix version avoiding as much as possible loops
;;(SH May  4 2011) added mad2sig = 1.4826 to go from media absolute
;;                 deviation to estimated sigma
;;(SH May  4 2011) add check on non nan input 

FUNCTION erratio, aIN, bIN, $
                  SIGA=sigaIN, $
                  SIGB=sigbIN, $
                  CALIBRERROR=rcalIN, $
                  NMC=Nmc, $
                  MEDIAN=medOUT

  ;; Parameter check
  ;;----------------
  ;; Manual
  IF (N_PARAMS() EQ 0) THEN BEGIN
     PRINT, "[sigdown(a/b),sigup(a/b)][N,2] "
     PRINT, "  = ERRATIO( a[N], b[N], SIGA=sig(a)[N,2], SIGB=sig(b)[N,2]"
     PRINT, "             CALIBRERROR=rcal[N], NMC=1000, MEDIAN=med )"
     PRINT, "  The errors are computed using a Monte-Carlo method."
     PRINT, "  a and b can be vectors or scalar. Siga and sigb can be symmetric"
     PRINT, " ([N,1]) or asymmetric; they are independent."
     PRINT, "  Rcal is the calibration error proportional to the two variables"
     PRINT, " (correlated)."
     RETURN, !VALUES.D_NaN
  ENDIF
  
  ;; Sizes
  a = aIN
  b = bIN
  N = N_ELEMENTS(a)
  IF (N_ELEMENTS(b) NE N) THEN BEGIN
     PRINT, "!!! ERRRATIO: incompatibel sizes between A and B..."
     RETURN, !VALUES.D_NaN
  ENDIF
  
  ;; valid elements
  if (total(finite(a)) eq 0) or $
     (total(finite(b)) eq 0) then begin
     PRINT, "!!! ERRRATIO: arrays A, B or both contain only NaNs"
     RETURN, !VALUES.D_NaN
  endif

  wrong_input =0 

  ;; RMS errors (convention *,0 down error *,1 up error)
  case (SIZE(sigaIN))[0] of 
     0: siga = make_array(N,2,value=sigaIN)
     1: if n_elements(sigaIN) eq   N then siga = sigaIN#[1d0,1d0] else wrong_input=1
     2: if n_elements(sigaIN) eq 2*N then siga = sigaIN           else wrong_input=1
     else: wrong_input=1
  endcase
  
  if wrong_input then begin
     message,/info, "wrong input for SIGA..."
     RETURN, !VALUES.D_NaN
  endif

  case (SIZE(sigbIN))[0] of 
     0: sigb = make_array(N,2,value=sigbIN)
     1: if n_elements(sigbIN) eq   N then sigb = sigaIN#[1d0,1d0] else wrong_input=1
     2: if n_elements(sigbIN) eq 2*N then sigb = sigaIN           else wrong_input=1
     else: wrong_input=1
  endcase
  
  if wrong_input then begin
     message,/info, "wrong input for SIGB..."
     RETURN, !VALUES.D_NaN
  endif
  
  ;; Calibration errors
  IF n_elements(rcalIN) eq 0 then begin
     use_cal = 0
     rcal=0d0
  endif else begin
     use_cal = 1
     rcal = rcalIN
     CASE N_ELEMENTS(rcal) OF
        1: rcal = REPLICATE(rcal,N)
        N: 
        ELSE: BEGIN
           PRINT, "!!! ERRATIO: wrong size for RCAL..."
           RETURN, !VALUES.D_NaN
        END
     ENDCASE
  endelse
  
  ;; valid elements in siga sigb rcal
  if (total(finite(siga)) eq 0) or $
     (total(finite(sigb)) eq 0) or $
     (total(finite(rcal)) eq 0) then begin
     PRINT, "!!! ERRRATIO: arrays sigA, sigB, Rcal or several contain only NaNs"
     RETURN, !VALUES.D_NaN
  endif
  
  ;; Number of Monte-Carlo iterations
  IF n_elements(Nmc) eq 0 THEN Nmc = 10000L
  
  ;; Monte-Carlo errors
  ;;-------------------
  theta_a = RANDOMN(seed,N,Nmc)
  theta_b = RANDOMN(seed,N,Nmc)

  ;; array used to expand 1D (N) to 2D N,Nmc
  ones = make_array(Nmc,value=1d0)
  
  if use_cal then begin
     delta_cal = (rcal#ones)*RANDOMN(seed,N,Nmc) ;; in fractional deviation
  endif else begin
     delta_cal = make_array(N,Nmc,value=0d0) ;; in fractional deviation
  endelse
  
  ratio = ((a#ones)*(1d0+delta_cal)+((reform(siga[*,0])#ones)*theta_a*(theta_a lt 0d0)+(reform(siga[*,1])#ones)*theta_a*(theta_a ge 0d0))) / $
          ((b#ones)*(1d0+delta_cal)+((reform(sigb[*,0])#ones)*theta_b*(theta_b lt 0d0)+(reform(sigb[*,1])#ones)*theta_b*(theta_b ge 0d0)))
  
  median_ratio = median(ratio,dimension=2)
  
  ;; check happy results
  idx_valid = where(finite(median_ratio),nvalid)
  if nvalid eq 0 then begin
     PRINT, "!!! not a single non-NaN median ratio has been found"
     RETURN, !VALUES.D_NaN
  endif
  
  ;; the median absolute deviation (MAD) in the limit of a normal
  ;; distribution is related to the standard deviation as
  ;; sigma = 1.4826 * MAD (http://en.wikipedia.org/wiki/Median_absolute_deviation)
  MAD2sig = 1.4826d0
  
  ;; this needs a loop since we have to median only those elements
  ;; that are above or below 0d0 (therefore not a simple matrix)
  sigmed = make_array(N,2,value=!VALUES.D_NAN)
  FOR i=0,nvalid-1 DO BEGIN
     idx = idx_valid[i]
     deviations = reform(ratio[idx,*] - median_ratio[idx])
     idx_u = WHERE(deviations ge 0d0,COMP=idx_d)
     if (idx_d[0] ne -1) then sigmed[idx,0] = MAD2sig*ABS(MEDIAN(deviations[idx_d]))
     if (idx_u[0] ne -1) then sigmed[idx,1] = MAD2sig*ABS(MEDIAN(deviations[idx_u]))
  ENDFOR  
  
  medOUT = median_ratio
  RETURN, sigmed

END


;; Test ERRATIO
;;-------------
;; 1) 1D symmetric case, no calibration error
a = 0.3D
b = 1.D
siga = 0.1*a
sigb = 0.075*b
rcal = 0.D
sigc = ERRATIO(a,b,SIGA=siga,SIGB=sigb,CALIBRERR=rcal,MED=medc)
PRINT 
PRINT, '1) 1D symmetric case:'
PRINT, STRING("Naive: a/b = ",F="(A20)") + STRING(a/b,F="(F10.7)") + " +/- " $
     + STRING(a/b*SQRT((siga/a)^2+(sigb/b)^2),F="(F10.7)")
PRINT, STRING("MC: a/b = ",F="(A20)") + STRING(medc,F="(F10.7)") + " - " $
     + STRING(sigc[0],F="(F10.7)") + " + " + STRING(sigc[1],F="(F10.7)")

;; 2) 1D symmetric case, with calibration error
a = 0.3D
b = 1.D
siga = 0.1*a
sigb = 0.075*b
rcal = 0.3D
sigc = ERRATIO(a,b,SIGA=siga,SIGB=sigb,CALIBRERR=rcal,MED=medc)
PRINT 
PRINT, '2) 1D symmetric case, with calibration error:'
PRINT, STRING("Naive: a/b = ",F="(A20)") + STRING(a/b,F="(F10.7)") + " +/- " $
     + STRING(a/b*SQRT((siga/a)^2+(sigb/b)^2),F="(F10.7)")
PRINT, STRING("MC: a/b = ",F="(A20)") + STRING(medc,F="(F10.7)") + " - " $
     + STRING(sigc[0],F="(F10.7)") + " + " + STRING(sigc[1],F="(F10.7)")

;; 3) 1D asymmetric case, with calibration error
a = 0.3D
b = 1.D
siga = [[0.05],[0.1]]*a
sigb = [[0.075],[0.15]]*b
rcal = 0.3D
sigc = ERRATIO(a,b,SIGA=siga,SIGB=sigb,CALIBRERR=rcal,MED=medc)
PRINT 
PRINT, '3) 1D asymmetric case, with calibration error:'
PRINT, STRING("Naive: a/b = ",F="(A20)") + STRING(a/b,F="(F10.7)") + " +/- " $
     + STRING(a/b*SQRT((MEAN(siga)/a)^2+(MEAN(sigb)/b)^2),F="(F10.7)")
PRINT, STRING("MC: a/b = ",F="(A20)") + STRING(medc,F="(F10.7)") + " - " $
     + STRING(sigc[0],F="(F10.7)") + " + " + STRING(sigc[1],F="(F10.7)")

;; 4) N D asymmetric case, with calibration error
N = 10
a = 0.3D*(DINDGEN(N)+1)
b = a/0.3D
siga = [[0.05*a],[0.1*a]]
sigb = [[0.075*b],[0.15*b]]
rcal = 0.3D
sigc = ERRATIO(a,b,SIGA=siga,SIGB=sigb,CALIBRERR=rcal,MED=medc)
PRINT 
PRINT, '4) N D asymmetric case, with calibration error:'
FOR i=0,N-1 DO BEGIN
PRINT, STRING("Naive: a/b = ",F="(A20)") + STRING((a/b)[i],F="(F10.7)")+" +/- "$
     + STRING((a/b*SQRT((MEAN(siga[i,*])/a)^2+(MEAN(sigb[i,*])/b)^2))[i],F="(F10.7)")
PRINT, STRING("MC: a/b = ",F="(A20)") + STRING(medc[i],F="(F10.7)") + " - " $
     + STRING(sigc[i,0],F="(F10.7)") + " + " + STRING(sigc[i,1],F="(F10.7)")
ENDFOR


END
