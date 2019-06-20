;; Simple tool to plot q values.
;; They have the structure .wave,.q0,..,.qn,.descr
PRO plq,in,qin=qin,temp=temp,tau=tau,abs=abs,cont=cont,fact=fact, $
        alpha=alpha,eV=eV,radius=radius,return=return,_extra=_extra
  
  default,qin,0
  ytit='Q!Dabs!N'
  tit='Cross section'

  ;; radius of the particle in micron
  default, radius,0.1d0

  IF size(in,/type) EQ 8 THEN BEGIN
      ;; maybe is it simply an aar with values
      names = tag_names(in)
      IF (WHERE(names EQ 'TYPE'))[0] NE -1 THEN BEGIN
          wave = in.data.wave
          q = in.data.flux
      ENDIF ELSE BEGIN
          wave = in.wave
          foo = execute('q = in.q'+n2s(qin))
          ntags = n_tags(in)
          tit=(tag_names(in))[ntags-1]+'='+ $
            strtrim(string((in.(ntags-1))[qin]),2)
      ENDELSE 
  ENDIF ELSE BEGIN
      ;; Not a structure so assume lnk values
      wave = in[0,*]
      q = c_abs(radius,in,_extra=_extra)/(!dpi*radius^2d0)
      ytit='Q values'
  ENDELSE 

  ;; In case needed for tau
  kappa = q
  
  ;; to convolve with a bb of T=temp
  IF keyword_set(temp) THEN BEGIN
      a1 = 3.97296d19
      a2 = 1.43875d4
      bb = a1/(wave^3d0)/(exp(a2/(wave*temp))-1d0)
      IF keyword_set(tau) THEN BEGIN
          q=bb*(1d0-exp(-tau*kappa/max(kappa)))
          tit = tit+'; T='+f2s(temp)+'; tau='+f2s(tau)
      ENDIF ELSE BEGIN
          q = q*bb
          tit = tit+'; T='+f2s(temp)
      ENDELSE 
  ENDIF

  IF keyword_set(abs) THEN BEGIN
      q = q*exp(-abs*kappa/max(kappa))
  ENDIF 

  IF keyword_set(fact) THEN BEGIN
      q = fact*q/max(q)
  ENDIF 

  IF keyword_set(cont) THEN BEGIN
      q = q+shc_interpol(cont.data.wave,cont.data.flux,wave)
  ENDIF 
  
  ;; Should do proper checking
  IF keyword_set(alpha) THEN BEGIN
      count = n_elements(in)/3
      wave = reform(in[0,*],count)
      k = reform(in[2,*],count)
      ;; exticntion coeff in cm-1
      q = 4d0*!dpi*k/wave*1d4
      ytit='alpha [cm!U-1!N]'
      tit=''
  ENDIF 

  IF keyword_set(eV) THEN BEGIN
      c = 2.9979d8    ;;M/s 
      h = 6.62620d-34 ;; J/s
      eV = 1.602189d-19 ;;J
      wave = h*c/eV*1d6/wave
  ENDIF 

;; Stupid things needed to plot ps fonts too  
  IF !p.font EQ -1 THEN BEGIN
      mu      = '!7l!X'
  ENDIF ELSE BEGIN 
      mu    = '!Mm!X'
  ENDELSE

  pl,wave,q,ytit=ytit,xtit='Wavelength ['+mu+'m]', $
     ps=0,title=tit,return=return,_extra=_extra
END
