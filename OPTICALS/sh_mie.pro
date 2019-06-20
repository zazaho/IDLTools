FUNCTION sh_mie,r_in,lnk,aar_out=aar_out
  
; NAME:
;       SH_MIE
; PURPOSE:
;       Calculates the scattering parameters of a series of particles
;       using the Mie scattering theory.
; CATEGORY:
;       EODG Mie routines
; CALLING SEQUENCE:
;       sh_mie, r_in , lnk [, /aar_out]
; INPUTS:
;       r_in:       A 1D array of particle size parameters
;       Cm:         The complex refractive index of the particles
; MODIFICATION HISTORY
;       (SH Apr  6 2006) simplified for single angle
;       G. Thomas 1998 mie_uoc.pro (translation of mieint.f to IDL)
;
;       D. Grainger 2001(?) mie_uoc_d.pro (Added support for arrays of
;       particle sizes and included calculation of phase function)
;
;       G. Thomas Sept. 2003 (Put into EODG routines format)
;
;       G. Thomas Feb. 2004 (Introduced explicit double precision
;       numerical values into all computational expressions)
;       G. Thomas Apr. 2005 (NmX assignment changed to type long)
;       G. Thomas Apr. 2005 (Added dlm keyword)
;       G. Thomas Apr. 2005 (Changed code to ensure Qbsc is always
;       caculated for the backscatter direction)
;       G. Thomas Jun. 2005 (Added calculation of phase function after
;       calling mie_dlm_single, since the DLM no longer returns it)

;   If an array of cos(theta) is provided, calculate phase function

  ;; The wavelengths
  wave = reform(lnk[0,*])
  ;; The size parameter
  x = r_in/wave
  
  ;; take the complex part /real part of the refractive index
  cm = reform(lnk[1,*]/lnk[2,*])
  
  ;; The output array
  c_ext = make_array(n_elements(wave),value=0d0)
  
  Imaxx = 12000l

  FOR i = 0,n_elements(x)-1 DO BEGIN

     IF (X[i] GT Imaxx) THEN BEGIN
        MESSAGE, 'Error: Size Parameter Overflow in Mie'
     ENDIF ELSE BEGIN
        Ir = 1.D0 / cm[I]
        Y =  X[i] * cm[I]
        
        IF (X[i] LT 0.02) THEN  NStop = 2 ELSE BEGIN
           CASE 1 OF
              (X[i] LE 8.0)    : NStop = X[i] + 4.00*X[i]^(1./3.) + 2.0
              (X[i] LT 4200.0) : NStop = X[i] + 4.05*X[i]^(1./3.) + 2.0
              ELSE             : NStop = X[i] + 4.00*X[i]^(1./3.) + 2.0
           ENDCASE
        END
        
        NmX = LONG(MAX([NStop,ABS(Y)]) + 15.)
        D = DCOMPLEXARR(Nmx+1)
        
        FOR N = Nmx-1,1,-1 DO BEGIN
           A1 = (N+1) / Y
           D[N] = A1 - 1/(A1+D[N+1])
        END
        
        Psi0 = Cos(X[i])
        Psi1 = Sin(X[i])
        Chi0 =-Sin(X[i])
        Chi1 = Cos(X[i])
        Xi1 = DCOMPLEX(Psi1,Chi1)
     
        Dqxt = 0.D0
        Tnp1 = 1D0
        
        FOR N = 1l,Nstop DO BEGIN
        
           DN = Double(N)
           
           Tnm1 = Tnp1
           Tnp1 = Tnp1 + 2D0
        
           Rnx = DN/X[i]
           
           Psi = DOUBLE(Tnm1)*Psi1/X[i] - Psi0
           
           Chi = Tnm1*Chi1/X[i]       - Chi0
        
           Xi = DCOMPLEX(Psi,Chi)
        
           A = ((D[N]*Ir+Rnx)*Psi-Psi1) / ((D[N]*Ir+Rnx)*  Xi-  Xi1)
           B = ((D[N]*cm[I]+Rnx)*Psi-Psi1) / ((D[N]*cm[I]+Rnx)*  Xi-  Xi1)
           
           Dqxt = Tnp1 * DOUBLE(A + B) + Dqxt
           
           Psi0 = Psi1
           Psi1 = Psi
           Chi0 = Chi1
           Chi1 = Chi
           Xi1 = DCOMPLEX(Psi1,Chi1)
        
        ENDFOR
        c_ext[i] =  4d0 *!dpi * Dqxt
     ENDELSE 
  ENDFOR
  
;; loop over x (relative size)
  return,c_ext
  
END
