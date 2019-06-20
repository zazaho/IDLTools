
pro readplanes, DIR, MINOR, STACK, CUBE, INTTIME, hdr=hdr, wait=wait, istart=istart, istop=istop
;
; simple IDL procedure to read delivered TIMMI data 
;
; call: IDL> readplanes,'104081022_imag_HD132356',1,stack,cube,hdr=hdr
;
; a data CUBE is returned as a 3D IDL array of type float (320,240,n_planes)
; in addition, a variable INTTIME is returned which contains the total integration tim
; the float array STACK contains the chopping corrected, bias substracted, coadded 2D image
; optionally, variables istart and istop can be given to extract specific sub-frames
;
; Version 1.0   11/Apr/2001  M Sterzik (msterzik@eso.org)
;
; INPUT:	DIR (string) - root directory name containing TIMMI2 data
; 		MINOR (int)  - minor number (= nodding position) for which planes should be retrieved
; OUTPUT:	STACK fltarr(320,240)-  coadded, chopping and bias corrected 2D image
;		CUBE  fltarr(320,240,n_planes) -  3D data cube containing n_planes of chopping and bias corrected images
;               INTTIME  total integration time
;			 dit*ndit*nvid*nminor*2
; 	        	(2 because NDIT*DIT (chop_on) + NDIT*DIT (chopp_off))
; KEYWORD hdr	  = fits header of last image plane read
;         istart  = start processing only at video frame istart
;	  istop   = process only until istop video frames and exit
;         wait    = wait 1 sec in display
; --------------------------------------------------------


minorstring = string(minor,format='(i5.5)')
ttb2d = fltarr(320,240)
is = 320*240L
ttb2d = reform(ttb2d,is)
inttime = 0.

; 1: gunzip the files in the directory to be read

foo = findfile(dir+'/'+minorstring+'/*.fits.gz',count=cnt)
IF cnt NE 0 THEN BEGIN
    cmd = 'gunzip '+ dir+'/'+minorstring+'/*.fits.gz'
    spawn, cmd
ENDIF 

; 2: get list of filenames to be read

fname = findfile(dir+'/'+minorstring+'/01*.fits*',count=cnt)

	if not keyword_set(istop) then istop = cnt
	if not keyword_set(istart) then istart = 1

; 3: read in each plane file by file, substract bias, and store in image cube

for ii = istart,istop do begin
	fnamei = fname(ii-1)
	tt2d = readfits(fnamei,hdr)

; extract header info for each plane ...

		hdit 	= 'HIERARCH ESO DET DIT'
		hndit 	= 'HIERARCH ESO DET NDIT'
		res	= strpos(hdr,hdit)
		stdit	= hdr(where(res eq 0))
		res	= strpos(hdr,hndit)
		stndit	= hdr(where(res eq 0))
		dit 	= float(strmid(stdit,25,10))/1000.
		ndit 	= fix(strmid(stndit,25,10))
		inttime = inttime + 2.*dit*ndit

	tt = tt2d(*,*,0)
	bias1d = total(tt,2)/240.
	bias2d = rebin(bias1d,320,240)
	ttb = tt - bias2d
	tvscl, ttb
	if keyword_set(wait) then wait, 1
	ttb1d = reform(ttb,is)
	ttb2d = [[ttb2d],[ttb1d]]
endfor

; prepare final data cube, and perform coadding 

n_planes = istop-istart+1
ttb2d = ttb2d(*,1:*)
cube = reform(ttb2d,320,240,n_planes)
stack = total(cube,3)

return
end

