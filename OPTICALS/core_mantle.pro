;; Function to construct optical constants for a core mantle grain in
;; the rayleigh limit. So l >> a, where l is the wavelength and a is
;; the radius of the grain. Following Bohren and Huffman pages
;; 140,149,227. The following equations apply:
;; The absorption crosssection C_abs:
;; C_abs = k*Im(alpha) where k = 2*pi/l and alpha:
;; alpha = 4*pi*R_out^3*
;; ((eps_2-eps_m)(eps_1+2eps_2)+f*(eps_1-eps_2)(eps_m+2eps_2)) /
;; ((eps_2+2eps_m)(eps_1+2eps_2)+f(2eps_2-2eps_m)(eps_1-eps_2))
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

;; Input r_in,R_out and lnk_1,lnk_2.
;; lnk[0,*] = l
;; lnk[1,*] = n
;; lnk[2,*] = k

;; Options:
;; aar_out produce an aar with the optical constants with the
;; wavelengths and the c_abs in the wave and flux tags
;;
;; volume ratios use the R_1 as the ratio of volume of the core over
;; the whole particle rather than the radius it self
FUNCTION core_mantle,R_1,R_2,lnk_1,lnk_2,aar_out=aar_out,volume=volume
  
  ;; Check input
  IF n_params() NE 4 THEN BEGIN
    print,'CORE_MANTLE: input params; r_core,r_mantle,lnk_core,lnk_mantle'
    return,0
  END
  ;; Use the r_1 as a volume ratio
  IF keyword_set(volume) THEN BEGIN
    IF R_1 GT 1d0 THEN BEGIN 
      print,'the volume ratio cannot be larger than one'
      return,0
    ENDIF
    R_1 = R_2*(R_1)^(1d0/3d0)
  ENDIF
  
  IF R_1 GT R_2 THEN BEGIN
    print,'CORE_MANTLE: input params; r_core,r_mantle,lnk_core,lnk_mantle'
    print,'CORE_MANTLE: r_core cannot be bigger than r_mantle'
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
  eps_1 = complex(eps_prime_1,eps_primeprime_1)
  
  eps_prime_2 = n_2^2d0-k_2^2d0
  eps_primeprime_2 = 2d0*n_2*k_2
  eps_2 = complex(eps_prime_2,eps_primeprime_2)
  
  eps_m = 1d0
  
  f = (r_1/r_2)^3d0
  
  k = 2*!pi/l_1
  
  ;; Now fill in the large formula
  alpha = 4d0*!pi*R_2^3d0 * $
    ((eps_2-eps_m)*(eps_1+2d0*eps_2)+f*(eps_1-eps_2)*(eps_m+2d0*eps_2)) / $
    ((eps_2+2d0*eps_m)*(eps_1+2d0*eps_2)+f*(2d0*eps_2-2d0*eps_m)*(eps_1-eps_2))
  
  C_abs = k*imaginary(alpha)
  
  IF keyword_set(aar_out) THEN BEGIN
    aout = sh_define_aar(length=nlnk_1)
    aout.data.wave = l_1
    aout.data.flux = c_abs
    c_abs = aout
  ENDIF
  
  return,C_abs
END
