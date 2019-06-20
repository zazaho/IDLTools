;c IDL function filter_ekh.pro (adapted from NOD2 routine CALFFUN8)
;c
;       SUBROUTINE CALFFUN8(LEN1,LEN2,ICENTR,PTSEP,PI,ISTAT)
;*
;      REAL*8    FFUN8(1), PTSEP, PI, SINCS, SINCSNEG, SUMT, DX,
;     $SN, SUM, PHI_IJ, SINC8
;     INTEGER*4 IDIMFF8, LEN1, LEN2, ICENTR, NSINCS, ISTAT,
;     $NSINCS2, I, J
;      COMMON /FFUN8/ IDIMFF8, FFUN8! dimension should be = idimff8
;*
;c SUMT is to be a normalizing factor for the convolution function.
;c Subroutine CALFFUN8 is a modified version of CONF2. CALFUN8 calculates a
;c convolution function which will filter out spatial frequencies of
;c wavelength equal to the spacing of the original two-beam system, and
;c harmonics of this frequency. If any of these spatial frequencies exist in
;c the map, they must result from noise or interference, since the two-beam
;c observation should have acted as a spatial frequency filter. Hopefully if
;c a map is restored after the application of filtering by this convolution
;c function, the resultant signal-to-noise ratio and any effects of incorrect
;c restoration will be substantially improved.
;c Note that CALFFUN8 calculates an odd number (=2*nsincs+1) of delta
;c functions, while CALRFUN8 works on an even number. CALFFUN8 produces a
;c symetric function, CALRFUN8 an anti-symetric function.
;c SUMT is to be a normalizing factor for the convolution function.
;c Written by Darrel Emerson ?
;c real*8 version 08.07.1993, Robert Zylka, MPIfR Bonn
;
;*
FUNCTION FILTER_EKH_kosma, BMSEP, naz, pas_az
;
;COMMON SHARE_NUMB, nchan, nrecord, eff_pos, eff_neg, naz, xinc, npairwobb, echant, ndim
;
;
;LEN1 = n_elements(SCAN)
;LEN2 = n_elements(CFUN)
LEN1 = nAz
LEN2 = 2*nAz-1
ICENTR = nAz-1
;
FFUN = fltarr(LEN2)
;
;bmsep = 32.
xinc = pas_az
ptsep = abs(bmsep)/abs(xinc)
;
NSINCS = floor(LEN1/PTSEP + 0.5E0)
NSINCS2 = 2*NSINCS + 1
SINCS =  NSINCS + 1.
SINCSNEG = -SINCS
;
DX = findgen(LEN2) + 1. - LEN1
;print, DX(ICENTR)
;print, DX
SN = (findgen(NSINCS2)+1.)*PTSEP + SINCSNEG*PTSEP
;print, SN(NSINCS)
;print, SN
;
PHI_IJ = ((dx#(fltarr(nsincs2)+1.) - (fltarr(len2)+1.)#sn))*!pi
;
;c             PHI_IJ is the phase angle at point i relative to SINC8 j
;
SINC_PHI_IJ = fltarr(len2,nsincs2)+1.
SMALLIM = 1.0E-35
IND = WHERE(abs(PHI_IJ) GT SMALLIM, count)
IF (count ne 0) THEN BEGIN
  SINC_PHI_IJ(IND) = SIN(PHI_IJ(IND))/PHI_IJ(IND)
ENDIF
;
FFUN = - Total(SINC_PHI_IJ,2)
SUMT = Total(FFUN)            ; a normalizing factor for the filtering func 
;print, sumt
SUMT = -SUMT*FLOAT(LEN1)/FLOAT(LEN2)
;print, sumt
;c This is to make the normalizing factor sumt for the array of sincs such
;c that the sum over the map size length is equal to -1.
FFUN  = FFUN/SUMT
print, total(ffun)
;wset, 0 & plot, ffun
;
print, icentr
FFUN(ICENTR) = FFUN(ICENTR) + 1.0E0
;wset, 1 & plot, ffun
;c The centre element of FFUN8 is supposed to be 1.0 greater than it would be
;c from the sum of the various sincs.
;*
RETURN, FFUN
END

