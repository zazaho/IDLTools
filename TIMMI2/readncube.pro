; ----------------------------------------------------------
pro readncube, file, ttbb, cube, inttime, hdr=hdr, wait=wait, istart=istart, istop=istop
;
; IDL procedure to read a TIMMI datacube from a EXTENDED FITS file of type raw_ymmddhhmm_imag.fits
;
; call: IDL> readncube,'raw_103121022_imag.fits', ttbb, cube, hdr=hdr
;
; the cube is returned as an 3D IDL array of type float (320 * 240 * nplanes)
; in addition, a variable inttime is returned which contains the total integration time
; float variablettbb  contains chopping and nodding corrected 2D coadded frame
; the first two planes of the TIMMI data cube contain general header information,
; the header is returned in keyword hdr
; optionally, variables istart and istop can be set, if only a sub-cube is to be extracted
; CAUTION: POSSIBLE MEMORY ALLOCATION PROBLEMS.
; cube can be very large, depending on how many planes are contained
;
; Version 1.0, 9.3.2001, MST
; questions/problems email to: msterzik@eso.org
;
;
; INPUT:	file:    filename of cuberaw of timmi pipeline
; OUTPUT:	ttbb:    chopping, nodding and bias corrected final 2D (320 * 240) image
;			inttime: integration time in sec ( = dit * ndit * nvid * nminor)
;			cube:    3D image (320 * 240 * nplanes)
; KEYWORD   hdr:	  = extended fits hdr of cube2dd
;			istart:  = start adding planes from istart onward
;	        istop:   = add only istop video frames and exit
;           wait:    = wait 1 sec in display
; --------------------------------------------------------
;

	if not keyword_set(istop) then istop = -999
	if not keyword_set(istart) then istart = 2

	catch, Error_status
	If Error_status ne 0 then begin
	 ;print, hdr
	 print, 'total number of OFF positions = ',  inod_minus
	 print, 'total number of ON positions = ',  inod_plus
	 intot = inod_minus + inod_plus + 1
	 cube = reform(ic2d,320,240,intot)
	 cube = cube(*,*,1:*)
	 print, ' '
 	 print, 'INTTIME [sec] 		       = ',  inttime
	 print, ' '
         if ( inod_minus ne inod_plus) then STOP
 	 return
	endif
;
	i		= istart-1
 	inod_plus  	= 0
 	inod_minus	= 0
	Err 		= 0
	ttbb 		= fltarr(320,240)
	is			= 320*240L
	ic2d = reform(ttbb,is)
	c1 		= ' '
	hdr		= ' '
        while Err eq 0 do begin

	    tt2d 	= readfits(file,hh,ext=i)
	    xtension 	= (strmid(hh(0), 10,10))
	    c1	 	= strmid(xtension,1,1)
	    IF c1 ne  'I' and i le 3 THEN begin
		i 		= i + 1
		tt2d = readfits(file,hh,ext=i)
		xtension 	= (strmid(hh(0), 10,10))
		c1 	= strmid(xtension,1,1)

		if c1 ne  'I' THEN begin
			print, 'Exit c1_xtension'
			stop
		endif
	     endif
	     ;print,i,Error_status,Err
 	     i_hkey     = i

	      if i eq i_hkey then  begin
		hdit 	= 'HIERARCH ESO DET DIT'
		hndit 	= 'HIERARCH ESO DET NDIT'
		hnvid 	= 'HIERARCH ESO PRO REDU XPECT VIDEO'
		hnminor = 'HIERARCH ESO PRO REDU XPECT MINOR'

		res	= strpos(hh,hdit)
		stdit	= hh(where(res eq 0))
		res	= strpos(hh,hndit)
		stndit	= hh(where(res eq 0))
		res	= strpos(hh,hnvid)
		stnvid	= hh(where(res eq 0))
		res	= strpos(hh,hnminor)
		stnminor= hh(where(res eq 0))

		dit 	= float(strmid(stdit,25,10))/1000.
		ndit 	= fix(strmid(stndit,25,10))
		nvid 	= fix(strmid(stnvid,42,10))
		nminor 	= fix(strmid(stnminor,42,10))

		inttime = dit*ndit*nvid*nminor

	       endif


	i = i+1
	tt = tt2d(*,*,0)
	bias1d = total(tt,2)/240.
	bias2d = rebin(bias1d,320,240)

	hnod = 'HIERARCH ESO OBS NODPOS'
	res= strpos(hh,hnod)
	stnod= hh(where(res eq 0))

; makes it being positiv or negativ fo On or OFF positon
	nod = float(strmid(stnod,25,10))*2. - 1.

	ttb = tt-bias2d
	ic1d = reform(nod(0)*ttb,is)
	ic2d = [[ic2d],[ic1d]]

	ttbb = ttbb + nod(0)*ttb

; count total number of on and off positons
	if (nod(0) eq -1) then inod_minus = inod_minus + 1
	if (nod(0) eq 1)  then inod_plus  = inod_plus + 1
	print,'frame # =',i, nod
	;ttbb = rebin(ttbb,640,480)
	tvscl,ttbb
	if keyword_set(wait) then wait, 1
	hdr = hh
	if i eq istop then begin
	 print, 'exit programm 1 cycle on for uc1'
	 save, /xdr,  ttbb, filename='readncube.xdr'
	 print, 'total number of OFF positions = ',  inod_minus
	 print, 'total number of ON positions = ',  inod_plus
	 print, 'written on  readncube.xdr, change inttime!!'
	 intot = inod_minus + inod_plus + 1
	 cube = reform(ic2d,320,240,intot)
	 cube = cube(*,*,1:*)
	 return
	endif


endwhile

return
end
;
;  ----------------------------------------------------------------
;