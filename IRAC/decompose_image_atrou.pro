;+
; NAME:
;      decompose_image_atrou
;
; PURPOSE:
;      - Decompose input FITS image in wavelet images (run atrou_hd on
;        an image) and save the FITS cube
;      - Select the output scales to create 2 output FITS images: small and
;        large scales
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;    file_in: string of FITS file
;	
; KEYWORD PARAMETERS:
;    smallscalescutoff: integer: includ in small scales images all
;    scales from 0 to smallscalescutoff (default=5)
;    shotnoise: integer: do not take into account the first shotnoise scales
;    norm: multiply resulting map by norm
;
; OUTPUTS:
;     3 FITS files: 1 cube, 1 image (small scales) 1 image (large scales)
;
; PROCEDURE:
;   atrou_hd
;
; EXAMPLE:
;    decompose_image_atrou, 'map_obslf_0000_0160_nh20.0.fits'
;    decompose_image_atrou, 'map_obslf_0000_0070_nh20.0.fits'
;    decompose_image_atrou, 'map_obslf_0000_0024_nh20.0.fits'
;
;    decompose_image_atrou, 'map_obslf_0000_0160_nh20.0.fits', smallscalescutoff=3
;    decompose_image_atrou, 'map_obslf_0000_0070_nh20.0.fits', smallscalescutoff=3
;    decompose_image_atrou, 'map_obslf_0000_0024_nh20.0.fits', smallscalescutoff=3
;
;    decompose_image_atrou, 'Iraf_mosA70PGim.fits', smallscalescutoff=3, shotnoise=1, norm=1.e5
;
;
;
;
; MODIFICATION HISTORY:
;    27-Feb-2002 Written Herve Dole University of Arizona
;    06-Mar-2002 Updated with small/large scales separation HD
;    23-Dec-2003 add shotnoise, norm HD
;    09-Sep-2004: add tests on fits image size HD
;
;-
PRO decompose_image_atrou, file_in, smallscalescutoff=smallscalescutoff, shotnoise=shotnoise, norm=norm

; Paths
;------
path_in = ''                    ;!MIPS_DATA_DIR + 'maps_simu/fits/'
path_out = ''                   ; !MIPS_DATA_DIR+ 'source_extraction/wavelet/fits/'

; FITS Filenames
;---------------
fits_in = path_in + file_in
pos = STRPOS(file_in, '.fits')
fileroot = STRMID(file_in, 0, pos)

file_out_cube = fileroot + '_wavelet.fits'
fits_out_cube = path_out + file_out_cube

file_out_small = fileroot + '_wsmall.fits'
fits_out_small = path_out + file_out_small

file_out_large = fileroot + '_wlarge.fits'
fits_out_large = path_out + file_out_large

; Read Input Image
;-----------------
data = MRDFITS(fits_in, 0, h)
sz = SIZE(data)
npixx = sz(1)
npixy = sz(2)

IF sz(0) EQ 3 THEN BEGIN        ; Case: 3D Cube with Planes
    im = REFORM(data(*,*,0))    ; Extract Image
ENDIF
IF sz(0) EQ 2 THEN BEGIN        ; Case: 2D image => weight is in extension
    im = REFORM(data(*,*,0))    ; Extract Image
ENDIF
IF sz(0) EQ 0 THEN BEGIN        ; Case: 0D image => map+weight are in extension
    extend = SXPAR(h,'EXTEND')
;    IF extend EQ 1  THEN BEGIN
        im = MRDFITS(fits_in, 1, h, ERROR_ACTION=error_mrdfits, status=status_mrdfits) ; Extract Image
;    ENDIF
ENDIF

; Sizes for Wavelet Decomposition
;---------------------------------
IF NOT KEYWORD_SET(smallscalescutoff) THEN nsmallscales = 5 ELSE nsmallscales = smallscalescutoff ; # of scales selection for small scales
IF NOT KEYWORD_SET(shotnoise) THEN shotnoise = 0 ELSE shotnoise=shotnoise ; # of small scales to remove to avoid noise

; Compute Wavelet Decomposition
;------------------------------
cube = atrou_hd(im, nsmallscales)

; Save Output Cube
;-----------------
WRITEFITS, fits_out_cube, cube, h

; Sizes
;------
sizecube = SIZE(cube)
nwscales = sizecube(3)
nx = sizecube(1)
ny = sizecube(2)

; Scales Separation: Create a Small Scales Only Cube
;---------------------------------------------------
small_cube = FLTARR(nx, ny, nsmallscales-shotnoise) ; Cube of small scales only
FOR iwscales = 0, nsmallscales-1-shotnoise DO BEGIN ; LOOP on Scales
    small_cube(*, *, iwscales) = cube(*, *, iwscales+shotnoise)
ENDFOR                          ; END LOOP on Scales

; Scales Separation
;------------------
small = TOTAL(small_cube, 3)    ; Array of Small Scales (High Frequency Signal)
large = im - small              ; Array of Large Scales (Low Frequency Signal)
IF KEYWORD_SET(shotnoise) THEN large = large - TOTAL(cube(*,*,0:shotnoise), 3)

; Save Output Small Scales
;-------------------------
IF KEYWORD_SET(norm) THEN  small =  small * norm
WRITEFITS, fits_out_small, small, h

; Save Output Large Scales
;-------------------------
IF KEYWORD_SET(norm) THEN  large =  large * norm
WRITEFITS, fits_out_large, large, h


END

