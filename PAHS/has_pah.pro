;; (SH Oct 13 2003)
;; Simple function to determine if a spectrum contains PAHs.
;; The idea is simple: Take WELL CHOSEN pasbands and do photometry
;; do a ''CONTINUUM'' determination and subtract. If positive FOR MOST
;; bands than we have detected pahs

;; The tricky bits are the well chosen bands and the continuum regions
;; and the threshold to say we have positive detection

;; For now it does not work very well :-(

FUNCTION has_pah,aarin,threshold=threshold

  default,threshold,3 ;; Jy

  aar = sh_cut(aarin)
  
  b3    = [3.2,3.35]
  c3_1  = [2.7 ,2.9]
  c3_2  = [3.75,4.0]
  b6    = [6.1,6.45]
  c6_1  = [5.8 ,6.0]
  c6_2  = [6.6 ,6.8]
  b8    = [7.5 ,8.4]
  c8_1  = [6.6 ,6.8]
  c8_2  = [9.15,9.4]
  b11   = [11.1,11.7]
  c11_1 = [10.6,10.9]
  c11_2 = [11.8,12.35]
  
  cen_b3    = (b3   [1]+b3   [0])/2.0
  cen_c3_1  = (c3_1 [1]+c3_1 [0])/2.0
  cen_c3_2  = (c3_2 [1]+c3_2 [0])/2.0
  cen_b6    = (b6   [1]+b6   [0])/2.0
  cen_c6_1  = (c6_1 [1]+c6_1 [0])/2.0
  cen_c6_2  = (c6_2 [1]+c6_2 [0])/2.0
  cen_b8    = (b8   [1]+b8   [0])/2.0
  cen_c8_1  = (c8_1 [1]+c8_1 [0])/2.0
  cen_c8_2  = (c8_2 [1]+c8_2 [0])/2.0
  cen_b11   = (b11  [1]+b11  [0])/2.0
  cen_c11_1 = (c11_1[1]+c11_1[0])/2.0
  cen_c11_2 = (c11_2[1]+c11_2[0])/2.0

  I_b3    = sh_integrate(aar,xrange=b3   ,/quiet,/noplot)/(b3   [1]-b3   [0])*cen_b3   ^2d0/3d14*1d26
  I_c3_1  = sh_integrate(aar,xrange=c3_1 ,/quiet,/noplot)/(c3_1 [1]-c3_1 [0])*cen_c3_1 ^2d0/3d14*1d26
  I_c3_2  = sh_integrate(aar,xrange=c3_2 ,/quiet,/noplot)/(c3_2 [1]-c3_2 [0])*cen_c3_2 ^2d0/3d14*1d26
  I_b6    = sh_integrate(aar,xrange=b6   ,/quiet,/noplot)/(b6   [1]-b6   [0])*cen_b6   ^2d0/3d14*1d26
  I_c6_1  = sh_integrate(aar,xrange=c6_1 ,/quiet,/noplot)/(c6_1 [1]-c6_1 [0])*cen_c6_1 ^2d0/3d14*1d26
  I_c6_2  = sh_integrate(aar,xrange=c6_2 ,/quiet,/noplot)/(c6_2 [1]-c6_2 [0])*cen_c6_2 ^2d0/3d14*1d26
  I_b8    = sh_integrate(aar,xrange=b8   ,/quiet,/noplot)/(b8   [1]-b8   [0])*cen_b8   ^2d0/3d14*1d26
  I_c8_1  = sh_integrate(aar,xrange=c8_1 ,/quiet,/noplot)/(c8_1 [1]-c8_1 [0])*cen_c8_1 ^2d0/3d14*1d26
  I_c8_2  = sh_integrate(aar,xrange=c8_2 ,/quiet,/noplot)/(c8_2 [1]-c8_2 [0])*cen_c8_2 ^2d0/3d14*1d26
  I_b11   = sh_integrate(aar,xrange=b11  ,/quiet,/noplot)/(b11  [1]-b11  [0])*cen_b11  ^2d0/3d14*1d26
  I_c11_1 = sh_integrate(aar,xrange=c11_1,/quiet,/noplot)/(c11_1[1]-c11_1[0])*cen_c11_1^2d0/3d14*1d26
  I_c11_2 = sh_integrate(aar,xrange=c11_2,/quiet,/noplot)/(c11_2[1]-c11_2[0])*cen_c11_2^2d0/3d14*1d26
  
  cnt_b3  = (I_c3_2 - I_c3_1)/(cen_c3_2 - cen_c3_1)*(cen_b3  - cen_c3_1) + I_c3_1
  cnt_b6  = (I_c6_2 - I_c6_1)/(cen_c6_2 - cen_c6_1)*(cen_b6  - cen_c6_1) + I_c6_1
  cnt_b8  = (I_c8_2 - I_c8_1)/(cen_c8_2 - cen_c8_1)*(cen_b8  - cen_c8_1) + I_c8_1
  cnt_b11 = (I_c11_2 - I_c11_1)/(cen_c11_2 - cen_c11_1)*(cen_b11 - cen_c11_1) + I_c11_1

  I_f3  = I_b3  - cnt_b3 
  I_f6  = I_b6  - cnt_b6 
  I_f8  = I_b8  - cnt_b8 
  I_f11 = I_b11 - cnt_b11
  
  ;; do we detect 3 out of the four bands?
  has_pah = ((I_f3 GE threshold) + (I_f6 GE threshold) + (I_f8 GE threshold) + (I_f11 GE threshold)) GE 3
  
  return,has_pah

END
