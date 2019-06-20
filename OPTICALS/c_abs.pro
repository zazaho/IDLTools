;; Get abs crossections in rayleigh limit
;; usage mgs = c_abs(1,mgs_lnk)
;; where 1 is the radius of the particle in micron
;; mgs_lnk are the wavelength, real and complex parf of the refractive
;; index of the material
;;
;; Options:
;; /aar_out make a aar structure as output with wave = l and flux is c_abs
;;
;; Used Bohren and Huffman, absorption and scattering of light by
;; small particles. Page 139(eq. 5.15) 140, 227 (eq. 9.1)
FUNCTION c_abs,R_in,lnk_in,aar_out=aar_out,cde=cde,needle=needle,disk=disk, $
               sphere=sphere
  ;; R_in in micron !!!
  ;; Out put also in micron meter

  ;; if only one param assume radius=1
  IF n_params() EQ 1 THEN BEGIN
      lnk = R_in
      R = 1d0
  ENDIF ELSE BEGIN 
      R = R_in
      lnk = lnk_in
  ENDELSE

  ;; Make some friendly variables
  count = n_elements(lnk)/3
  l = reform(lnk[0,*],count)
  n = reform(lnk[1,*],count)
  k = reform(lnk[2,*],count)
  
;; 9.1 p227
  eps_prime = n^2d0-k^2d0
  eps_primeprime = 2d0*n*k    
  eps = complex(eps_prime,eps_primeprime)
  ;; wavenumber in micron: 2*pi/lambda
  waven = 2d0*!dpi/l
  ;; volume in micron^3
  vol = 4d0/3d0*!pi*R^3d0

  CASE 1 OF
      keyword_set(cde): BEGIN
          ;; In this case use 12.36 p356
          C_abs=waven*vol*imaginary(2d0*eps/(eps-1d0)*Alog(eps))
      END
      keyword_set(needle): BEGIN
          ;; 12.34 p350
          c_abs = waven*vol/3d0* $
            (8d0/((eps_prime+1d0)^2d0+eps_primeprime^2d0)+1d0)*eps_primeprime
      END 
      keyword_set(disk): BEGIN
          ;; 12.34 p350
          c_abs = waven*vol/3d0* $
            (1d0/(eps_prime^2d0+eps_primeprime^2d0)+2d0)*eps_primeprime
      END 
      keyword_set(sphere): BEGIN
          ;; 12.34 p350
          c_abs = waven*vol/3d0* $
            (27d0/((eps_prime+2d0)^2d0+eps_primeprime^2d0))*eps_primeprime
      END 
      ELSE: BEGIN
          a = 3d0*vol*(eps-1d0)/(eps+2d0)
          C_abs = waven*imaginary(a)
      END 
  ENDCASE
  
  IF keyword_set(aar_out) THEN BEGIN
    aout = sh_define_aar(length=count)
    aout.data.wave = l
    aout.data.flux = c_abs
    c_abs=aout
  ENDIF

  return,c_abs
END

