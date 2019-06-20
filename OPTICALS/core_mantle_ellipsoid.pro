;; See Bohren en Huffman p.149
FUNCTION cme_L_INT,q
  COMMON COMMON_CME,a1,a2,axes,idx,eps1,eps2,epsm
  return,1d0/(axes[idx]^2d0+q)/ $
         sqrt((axes[0]^2d0+q)*(axes[1]^2d0+q)*(axes[2]^2d0+q))
END 

;; See Bohren en Huffman p.149
FUNCTION cme_L,i,k
  COMMON COMMON_CME,a1,a2,axes,idx,eps1,eps2,epsm
  foo = execute('axes=a'+n2s(k))
  idx = i
  ;; where to stop the integration
  int_end=1d2*sqrt(axes[0]*axes[1]*axes[2]*axes[idx])
  return,axes[0]*axes[1]*axes[2]/2d0*qsimp('cme_L_INT',0d0,int_end,eps=1d-5)
END

;; See Bohren en Huffman p.149
FUNCTION cme_nu
  COMMON COMMON_CME,a1,a2,axes,idx,eps1,eps2,epsm
  return,4*!PI*a2[0]*a2[1]*a2[2]/3d0
END

;; See Bohren en Huffman p.149
FUNCTION cme_f
  COMMON COMMON_CME,a1,a2,axes,idx,eps1,eps2,epsm
  return,a1[0]*a1[1]*a1[2]/(a2[0]*a2[1]*a2[2])
END

;; See Bohren en Huffman p.149 (5.35)
FUNCTION cme_alpha,i
  COMMON COMMON_CME,a1,a2,axes,idx,eps1,eps2,epsm
  nu = cme_nu()
  f  = cme_f()
  L1 = cme_L(i,1)
  L2 = cme_L(i,2)
  alpha = nu*((eps2-epsm)*(eps2+(eps1-eps2)*(L1-f*L2))+f*eps2*(eps1-eps2))/ $
    ((eps2+(eps1-eps2)*(L1-f*L2))*(epsm+(eps2-epsm)*L2)+f*L2*eps2*(eps1-eps2))
  return,alpha
END

;; Function to construct optical constants for a core mantle grain in
;; the rayleigh limit of grain with spheroidal shapes. So l >> a,
;; where l is the wavelength and a is 
;; the radius of the grain. Following Bohren and Huffman pages
;; 140,149,227. The following equations apply:
;; The absorption crosssection C_abs:
;; C_abs = k/3*Im(alpha1+alpha2+alpha3) where k = 2*pi/l and alpha:
;; Alphax -s given by (eq 5.35 , p149)
;; where eps is the dielectric constant the indices m,1,2 correspond
;; to the surrounding medium, the core and the mantle resp.
;; The eps is given by eps = eps_prime + i*eps_primeprime, where
;; eps_prime = n^2-k^2 and
;; eps_primePrime = 2nk
;; for the medium=vacuum we get n=1,k=0 and eps_m = 1
;;
;; The routine calculates C_abs for given R_in,R_out and given optical
;; constants lnk_1 and lnk_2.
;; If needed the lnk_2 will be interpolated to the same wavelengths as
;; the lnk_1 values.

;; Input axesr_in,R_out and lnk_1,lnk_2.
;; lnk[0,*] = l
;; lnk[1,*] = n
;; lnk[2,*] = k

;; Options:
;; aar_out produce an aar with the optical constants with the
;; wavelengths and the c_abs in the wave and flux tags

FUNCTION core_mantle_ellipsoid,ax1,ax2,lnk_1,lnk_2, $
                               aar_out=aar_out,volume=volume

  COMMON COMMON_CME,a1,a2,axes,idx,eps1,eps2,epsm
  default,aar_out,0
  
  a1=ax1
  a2=ax2
  
  ;; Check input
  IF n_params() NE 4 THEN BEGIN
    print,'CORE_MANTLE_ELLIPSOID: input params; axes_core,axes_mantle,'
    print,'                       lnk_core,lnk_mantle[,/aar]'
    return,0
  END
  
  IF n_elements(a2) EQ 1 THEN BEGIN
      IF keyword_set(volume) THEN BEGIN
          a2=a1*(a2)^(1d0/3d0)
      ENDIF ELSE BEGIN
          a2=a1*a2
      ENDELSE
  ENDIF
  
  ;; Make nice variable from lnk structure
  nlnk_1 = n_elements(lnk_1)/3
  l_1 = reform(lnk_1[0,*],nlnk_1)
  n_1 = reform(lnk_1[1,*],nlnk_1)
  k_1 = reform(lnk_1[2,*],nlnk_1)
  
  nlnk_2 = n_elements(lnk_2)/3
  l_2 = reform(lnk_2[0,*],nlnk_2)
  n_2 = reform(lnk_2[1,*],nlnk_2)
  k_2 = reform(lnk_2[2,*],nlnk_2)
  
  ;; Do we need to interpolate ???
  IF (nlnk_1 NE nlnk_2) OR (total(l_1 NE l_2) GT 0) THEN BEGIN
    n_2 = shc_interpol(l_2,n_2,l_1)
    k_2 = shc_interpol(l_2,k_2,l_1)
  ENDIF
  
  eps_prime_1 = n_1^2d0-k_1^2d0
  eps_primeprime_1 = 2d0*n_1*k_1
  eps1 = complex(eps_prime_1,eps_primeprime_1)
  
  eps_prime_2 = n_2^2d0-k_2^2d0
  eps_primeprime_2 = 2d0*n_2*k_2
  eps2 = complex(eps_prime_2,eps_primeprime_2)
  
  epsm = 1d0
  
  alpha0 = cme_alpha(0)
  alpha1 = cme_alpha(1)
  alpha2 = cme_alpha(2)

  k = 2*!pi/l_1
  
  C_abs = k/3d0*imaginary(alpha0+alpha1+alpha2)
  
  IF keyword_set(aar_out) THEN BEGIN
    aout = sh_define_aar(length=nlnk_1)
    aout.data.wave = l_1
    aout.data.flux = c_abs
    c_abs = aout
  ENDIF
  
  return,C_abs
END
