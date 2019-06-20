;#> srf_cp_fits_string.dc3
; Identifier   srf_cp_fits_string
;
; Purpose      Copy a set of strings to one string ensuring each is 80 chars 
;
; Synopsis     srf_cp_fits_string, head, base
;
; Arguments    Name      I/O Type    Description
;              -----------------------------------------------------
;              head       O  string  Output string header
;              base       I  string  Array of strings to add
;
; Returns      A string header
;
; Description  As the purpose says
;
; Comment      --
;
; Example      If base is a two element string array containing
;              'SIMPLE  =                    T /' &
;              'BITPIX  =                    8 /'
;              then after
;              srf_cp_fits_string, head, base
;              head will be a 160 byte long string, the first 80 of which 
;              will be 'SIMPLE...' padded out to 80 chars with spaces, the
;              last 80 being 'BITPIX...' similarly padded out
;
; Category     UTIL
;
; Filename     srf_cp_fits_string.pro
;
; Author       K. Leech ---> KL
;
; Version      1.12
;
; History      1.0   7-07-94  --> KL  First draft
;              1.1  14-07-94  --> KL  Modified and readied for CoCo
;              1.11 20-07-94  --> KL  Name changed to allow for
;                                     automatic compilation
;              1.12 09-03-95  --> HB  Unified Arguments
;#<
   pro srf_cp_fits_string, head, base

   for i = 0, n_elements( base ) - 1 do begin
;     Make sure at least 80 char long
      tst = base(i) + '                                         '
      tst = tst     + '                                         '

;     Now strip to 80 char
      tst = strmid( tst, 0, 80)

;     And add to header
      head = head + tst
   endfor
   return
end
