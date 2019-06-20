;c IDL function rfilter_ekh.pro (adapted from NOD2 routine CALRFUN8)
;
;      SUBROUTINE CALRFUN8(LEN1,LEN2,ICENTR,PTSEP,UNBAL,PI,ISTAT)
;*
;     REAL*8    RFUN8(1), PTSEP, UNBAL, PI, SINCS, ABSUNBAL, DX, ALPHA,
;    $SN, SUM, PHI_IJ, SINC8, ANORM, X, SMALLIM
;      INTEGER*4 IDIMRF8, LEN1, LEN2, ICENTR, NSINCS, ISTAT, NSINCS2, I,
;     $J
;      COMMON /RFUN8/ IDIMRF8, RFUN8! dimension should be = idimrf8
;c Subroutine CALRFUN8 works out a convolution function for the 2-beam
;c restoration program, where the 2-beam separation need not be an integral
;c number of map sampling intervals. This subroutine interpolates to a more
;c general grid using a SINC8 function sampled at information-theory limit.
;c UNBAL is the ratio in amplitude of the two beams (e.g. the ratio left-hand
;c beam amplitude over right-hand beam amplitude). Note that if the two beams
;c are not equal in size, then the pre-filtering usually carried out in
;c routine FILTSCAN or FILTMAP is suppressed.
;c Written by Darrel Emerson ?
;c real*8 version 08.07.1993, Robert Zylka, MPIfR Bonn
;*
FUNCTION RFILTER_EKH_kosma, BMSEP, naz, pas_az
;
;COMMON SHARE_NUMB, nchan, nrecord, eff_pos, eff_neg, naz, xinc, npairwobb, echant, ndim
;
;
;
;LEN1 = n_elements(SCAN)
;LEN2 = n_elements(CFUN)
LEN1 = nAz
LEN2 = 2*nAz-1
ICENTR = nAz-1
;
xinc = pas_az
ptsep = abs(bmsep)/abs(xinc)
;
      SMALLIM = 1.0D-35
      NSINCS = floor(LEN1/PTSEP+0.5D0)
      NSINCS2 = 2*NSINCS
      SINCS = NSINCS + 1.
;      UNBAL = -1.0
;      ABSUNBAL = ABS(UNBAL)
;      IF (ABSUNBAL GT 1.0D0) ABSUNBAL = 1.0E0/ABSUNBAL
;c******* Chris Salter modified this on Jun 13th 1985.****
;c The necessary function is calculated with a factor ABSUNBAL less than one
;c and corrected if necessary afterwards.
;*
;
DX = findgen(LEN2) + 1. - LEN1
;print, DX(ICENTR)
;print, DX
SN = (findgen(NSINCS2)+1.)*PTSEP + (0.5D0 - SINCS)*PTSEP
;print, SN(NSINCS)
;print, SN
;
PHI_IJ = ((dx#(fltarr(nsincs2)+1.) - (fltarr(len2)+1.)#sn))*!pi
;
;c             PHI_IJ is the phase angle at point i relative to SINC8 j
;
SINC_PHI_IJ = fltarr(len2,nsincs2)+1.
IND = WHERE(abs(PHI_IJ) GT SMALLIM, count)
IF (count ne 0) THEN BEGIN
  SINC_PHI_IJ(IND) = SIN(PHI_IJ(IND))/PHI_IJ(IND)
ENDIF
;
;         ALPHA = 1.0D0
;         SN = (0.5D0 - SINCS)*PTSEP
;         SUM = 0.0D0
;c ALPHA is a scaling factor for each of the delta functions in the convolutio
;c function, depending on the ratio of amplitudes of the two beams.
;
;       SUM = SUM - DSIGN(ALPHA,UNBAL)*SINC8(PHI_IJ)*DSIGN(1.0D0,SN)
SIGN = (fltarr(len2)+1.)#sgn(sn)
RFUN = - Total(SINC_PHI_IJ*SIGN,2)
;
;wset, 0 & plot, rfun
; 
RETURN, RFUN
END
 
