;+
; NAME:
;      check_decompose_image_atrou
;
; PURPOSE:
;      Read and displays the input & Output Images from decompose_image_atrou.pro
;
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;
; 
; INPUTS:
;   file_in: string of FITS file
;	
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;
; EXAMPLE:
;   check_decompose_image_atrou, 'map_obslf_0000_0160_nh20.0.fits'
;   check_decompose_image_atrou, 'map_obslf_0000_0070_nh20.0.fits'
;   check_decompose_image_atrou, 'map_obslf_0000_0024_nh20.0.fits'
;
;
; MODIFICATION HISTORY:
;    06-Mar-2002 Written Herve Dole, University of Arizona
;-
PRO check_decompose_image_atrou, file_in

; Paths
;------
path_in = !MIPS_DATA_DIR + 'maps_simu/fits/'
path_out =  !MIPS_DATA_DIR+ 'source_extraction/wavelet/fits/'

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
im = READFITS(fits_in, h)

; Read Cube
;----------
cube = READFITS(fits_out_cube, h)

; Read Small Scales
;------------------
small = READFITS(fits_out_small, h)

; Read Large Scales
;------------------
large = READFITS(fits_out_large, h)

; Statistics
;----------
PRINT, STATISTICS_HD(im),  STATISTICS_HD(small+large)
PRINT, STATISTICS_HD(im-(small+large))

; Plots
;------
LOADCT, 5
SLAFFI, im, small, large


END

