;; wise from http://adsabs.harvard.edu/abs/2011ApJ...735..112J
FUNCTION mag2jy, $
   U=U, $
   B=B, $
   V=V, $
   R=R, $
   I=I, $
   J=J, $
   H=H, $
   K=K, $
   L=L, $
   M=M, $
   WISE1=WISE1, $
   WISE2=WISE2, $
   WISE3=WISE3, $
   WISE4=WISE4, $
   e_U=e_U, $
   e_B=e_B, $
   e_V=e_V, $
   e_R=e_R, $
   e_I=e_I, $
   e_J=e_J, $
   e_H=e_H, $
   e_K=e_K, $
   e_L=e_L, $
   e_M=e_M, $
   e_WISE1=e_WISE1, $
   e_WISE2=e_WISE2, $
   e_WISE3=e_WISE3, $
   e_WISE4=e_WISE4, $
   wave=wave, $
   noprint=noprint

  selected = [keyword_set(U),keyword_set(B),keyword_set(V),keyword_set(R), $
              keyword_set(I),keyword_set(J),keyword_set(H),keyword_set(K), $
              keyword_set(L),keyword_set(M), $
              keyword_set(WISE1),keyword_set(WISE2),keyword_set(WISE3),keyword_set(WISE4)]
  
  idx = where(selected,cnt)
  IF cnt EQ 0 THEN BEGIN
      print,'usage jy=mag2jy(U|B|V|R|I|J|H|K|L|M|WISE{1,2,3,4}=mag[,wave=wave])'
      return,0
  ENDIF 
  
  default,U,0
  default,B,0
  default,V,0
  default,R,0
  default,I,0
  default,J,0
  default,H,0
  default,K,0
  default,L,0
  default,M,0
  default,WISE1,0
  default,WISE2,0
  default,WISE3,0
  default,WISE4,0

  default,e_U,0
  default,e_B,0
  default,e_V,0
  default,e_R,0
  default,e_I,0
  default,e_J,0
  default,e_H,0
  default,e_K,0
  default,e_L,0
  default,e_M,0
  default,e_WISE1,0
  default,e_WISE2,0
  default,e_WISE3,0
  default,e_WISE4,0

  default,noprint,0

  WV     = [   0.36,   0.44,   0.55,   0.70,   0.90,  1.235 ,   1.662, $
               2.159,     3.6,     4.8, $
               3.3526, 4.6028, 11.5608, 22.0883]
  
  ZP_lam = [4.35d-8,7.19d-8,3.92d-8,1.76d-8,8.30d-9,3.129d-9,1.133d-9, $
            4.283d-10,6.41d-11,2.13d-11, $
            8.1787d-11, 2.4150d-11, 6.5151d-13, 5.0901d-14]
  
  ZP_jy = ZP_lam * WV^2d0 / 3d14 * 1d26

  BANDS = ['U','B','V','R','I','J','H','K','L','M','WISE1','WISE2','WISE3','WISE4']

  MAGS = [U,B,V,R,I,J,H,K,L,M,WISE1,WISE2,WISE3,WISE4]
  e_MAGS = [e_U,e_B,e_V,e_R,e_I,e_J,e_H,e_K,e_L,e_M,e_WISE1,e_WISE2,e_WISE3,e_WISE4]
  
  FLX_jy = ZP_jy*1d1^(-1d0*MAGS/2.5d0)

  e_FLX_jy = FLX_jy * ((10.^(e_MAGS/2.5)-1.)+(1.-10.^(-1d0*e_MAGS/2.5)))/2.
  
  wave = WV[idx]
  flx  = FLX_jy[idx]
  e_flx  = e_FLX_jy[idx]

  IF noprint EQ 0 THEN BEGIN
      FOR i=0,n_elements(wave)-1 DO BEGIN
          print,format='(3F12.3)',wave[i],flx[i],e_flx[i]
      ENDFOR
  ENDIF
  return,flx
END
