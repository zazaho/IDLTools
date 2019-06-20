;; (SH Jul 25 2000)
;; Noise filtering function from Annique.
;; Input should be properly flatfielded and deglitched aar
;; Enter only 1 subband at the time
;;
;; (SH Jul 25 2000)
;; Modifications:
;; changed the function so it will accept whole subband instead of
;; only one scan.
;; Added option for playing with the smoothing params 

;; (SH Jul 28 2000)
;; More carefull in the smoothing business
;; Always make sure that the average is symmetric around point
;; make sure to divide by real number of points
;; What happens to negative part of fft
FUNCTION sh_noisefilter,aar,smoothing=smo,plot=plot,hcut=hcut, $
                        perc=perc,_extra=_extra
  
  ;; hcut determines where to usse the very smooth function
  default,hcut,50
  default,perc,5
  default,plot,0
  default,smo,[3,7,11,21]
  IF n_elements(smo) LT 4 THEN smo = smo[0]*[3,7,11,21]
  ;; define the smo2 that smo2 = (smo-1)/2
  smo2 = (smo-1)/2
  
  ;; sort the data according to wavlength only make sure to separate
  ;; the up and down scan so do sdir*wave and sort that one
  sort_order = sort(aar.data.wave*aar.data.sdir)
  
;  ;; stupid fix to stop sorting
;  sort_order = indgen(n_elements(aar.data.flux))
  
  Flux=aar.data[sort_order].flux
  n=n_elements(Flux)
  debruite=aar

  ;; The number of points in the real part of the fft transform
  m = (n+1)/2
  
  Y=fft(Flux,-1)
  Yf=alog10((abs(Y))^2)
  model=Yf[0:m]
  model[0]=Yf[0]
  
;  ;; use a percentage of the number of points
;  perc_smo = floor(m*perc/100d0)
;  FOR j=0 ,m DO BEGIN
;    ;; calculate the maximum number of points to take around j
;    base = min([j,perc_smo])
;    model[j]=total(Yf[j-base:j+base])/(2*base+1)
;  ENDFOR
FOR j=1 ,5 DO BEGIN
  ;; calculate the maximum number of points to take around j
  base = min([j,smo[0],m-j])
  model[j]=total(Yf[j-base:j+base])/(2*base+1)
ENDFOR
FOR j=6 ,20 DO BEGIN
  base = min([j,smo[1],m-j])
  model[j]=total(Yf[j-base:j+base])/(2*base+1)
ENDFOR
FOR j=21,hcut DO BEGIN
  base = min([j,smo[2],m-j])
  model[j]=total(Yf[j-base:j+base])/(2*base+1)
ENDFOR
FOR j=hcut+1,m-hcut-1 DO BEGIN 
  base = min([j,smo[3],m-j])
  model[j]=total(Yf[j-base:j+base])/(2*base+1)
ENDFOR
;;; These are the highest freqs so always smooth these
;;; over a lerge number of points say 300.
;;; This means that we use the negative values aswell to smooth
FOR j=m-hcut,m DO BEGIN
  base = min([j,n-j,300])
  model[j]=total(Yf[j-base:j+base])/(2*base+1)
ENDFOR
  
  modele=10^(model)
  btfs=10^(min(model))
  
  f = (modele-btfs)/modele
  ;; combine with the negative part
  ;; make sure not to duplicate the last point if the number of points
  ;; is even (n mod 2) = 1 
  f=[f,(reverse(f))[1-(n MOD 2):*]]
  
  debruit=Y*f
  debruite.data[sort_order].flux=float(fft(debruit,1))
  
  print,'SH_NOISEFILTER: filtered the data with smoothing params:',smo
  
  IF (plot) THEN BEGIN
    plot,Yf,/xs,/ys,_extra=_extra
    oplot,model,col=100
    print,'SH_NOISEFILTER: Press enter to continue'
    a = ''
    read,a
  ENDIF
  return,debruite
END

PRO defilter,resultat2=resultat2,resutat3=resultat3,niste=niste

  loop=0
  FOR i=0,1 DO BEGIN
    aar=read_faar(resultat2[loop])
    
    aara=select(aar,aar.data.line EQ 1)
    aarau=select(aara,aara.data.sdir EQ -1)
    aarad=select(aara,aara.data.sdir EQ 1)
    aarauf=filtering(aarau)
    aaradf=filtering(aarad)
    aarafin=combine(aarauf,aaradf)
    
    aarb=select(aar,aar.data.line EQ 2)
    aarbu=select(aarb,aarb.data.sdir EQ -1)
    aarbd=select(aarb,aarb.data.sdir EQ 1)
    aarbuf=filtering(aarbu)
    aarbdf=filtering(aarbd)
    aarbfin=combine(aarbuf,aarbdf)
    
    
    aard=select(aar,aar.data.line EQ 3)
    aard=select(aard,aard.data.wave LT 3.8)
    aardu=select(aard,aard.data.sdir EQ -1)
    aardd=select(aard,aard.data.sdir EQ 1)
    aarduf=filtering(aardu)
    aarddf=filtering(aardd)
    aardfin=combine(aarduf,aarddf)
    
    
    C = STRMID(niste[loop],0,1)
    IF (C EQ '8') OR (C EQ '9') THEN BEGIN
      aare=select(aar,aar.data.line EQ 5)
    ENDIF ELSE aare=select(aar,aar.data.line EQ 4)
    
    aareu=select(aare,aare.data.sdir EQ -1)
    aared=select(aare,aare.data.sdir EQ 1)
    aareuf=filtering(aareu)
    aaredf=filtering(aared)
    aarefin=combine(aareuf,aaredf)
    
    aarab=combine(aarafin,aarbfin)
    aarde=combine(aardfin,aarefin)
    aarae=combine(aarab,aarde)
    
    write_fitstable,aarae,resultat3[loop]
    
    loop=loop+1
    
  ENDFOR
END
