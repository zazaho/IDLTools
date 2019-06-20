;c IDL function conv_ekh.pro (adapted from NOD2 routine CONV8)
;c
;c CONV8 convolves vector SCAN8 with vector CFUN8, putting the result into
;c vector CSCAN8. LEN1 and LEN2 are the sizes of SCAN8 and CFUN8; the centre
;c element of CFUN8 is at element ICENTR. The convolution is normalized by
;c dividing the answer in CSCAN8 by the factor DNORM.
;c real*8 version 09.06.1993, Robert Zylka MPIfR Bonn
;*
;*
FUNCTION CONV_ekh1dim_kosma, Signal_1dim, CFUN, DNORM, naz
;
;COMMON SHARE_NUMB, nchan, nrecord, eff_pos, eff_neg, naz, xinc, npairwobb, echant, ndim
;
;COMMON SHARE_NUMB, nchan, nrecord, eff_pos, eff_neg
;COMMON SHARE_GRID, XDim, YDim, XRef, YRef, XVal, YVal, Xinc, Yinc
;COMMON SHARE_AZEL, nAz, nEl

;
; NB: nrecord = nAz*nEl
;
;Signal_arr = fltarr(nchan,nrecord)
;Signal_arr(0:nchan*nrecord-1) = Signal(0:nchan*nrecord-1)
;ichan = 1
;signal1 = Transpose(Signal_arr(ichan-1,*))
;Signal_arr = Transpose(Signal_arr)   ;   nrecord X nchan = nAz X nEL X nchan
;
;LEN1 = n_elements(SCAN)
;LEN2 = n_elements(CFUN)
LEN1 = nAz
LEN2 = 2*nAz-1
ICENTR = nAz-1
;
CSCAN_ARR   = fltarr(LEN2,LEN1)
;
CFUN_REP = CFUN#(fltarr(LEN1)+1.)
CFUN_ARR = fltarr(LEN2,LEN1)
CFUN_ARR(0:LEN2*LEN1-1) = CFUN_REP(0:LEN2*LEN1-1)
CFUN_REP = 0
;
AUX_ARR = (intarr(LEN2)+1)#(indgen(len1))
;
AUX_ARR2 = (indgen(LEN2)-ICENTR)#(intarr(LEN1)+1)
AUX_ARR = AUX_ARR+AUX_ARR2
AUX_ARR2 = 0
AUX_VEC = intarr(LEN2*LEN1)
AUX_VEC(0:LEN2*LEN1-1) = AUX_ARR(0:LEN2*LEN1-1)
IND = where((AUX_VEC lt 0) and (AUX_VEC gt LEN1-1), count)
IF (count ne 0) THEN BEGIN
;  AUX_VEC(IND) = -10*nEl*LEN1      ; "dummy" negative value
 AUX_VEC(IND) = -10*LEN1      	; "dummy" negative value
ENDIF
;
;AUX_VEC = AUX_VEC#(intarr(nEl)+1)
;AUX_VEC = AUX_VEC+[indgen(LEN2*LEN1*nEl)/(LEN2*LEN1)]*LEN1
AUX_ARR = intarr(LEN2,LEN1)
AUX_ARR(0:LEN1*LEN2-1) = AUX_VEC(0:LEN1*LEN2-1)
;
IND = where(AUX_ARR ge 0, count)
;
IF (count ne 0) THEN BEGIN
  CSCAN_ARR(IND) = Signal_1dim(AUX_ARR(IND))
ENDIF
;
CSignal1 = Total(CSCAN_ARR*cfun_arr,1)/DNORM
;Csignal_arr = fltarr(LEN1,nEL*nchan)
;CSignal1     = fltarr(LEN1*nEl)
;CSignal1(0:LEN1*nEl-1) = Csignal_arr(0:LEN1*nEl-1)
;
return, CSignal1 
end

