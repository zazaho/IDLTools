;#> sh_is_aar.dc2
; Identifier   sh_is_aar
;
; Purpose      test if variable is an AAR structure
;
; Synopsis     rc = sh_is_aar ( var )
;
; Arguments    Name       I/O  Type:  Decription:
;              ---------------------------------------
;              var         I   any    Input variable
;
; Returns      TRUE if var is an AAR structure, FALSE otherwise
;
; Description  This function checks the given variable for it's type.  The
;              function returns TRUE only if the input variable is a structure
;              of type AAR (which is made by the function define_aar).
;
; Comment      --
;
; Example      if sh_is_aar ( my_aar ) then plotaar, my_aar
;
; Category     UTIL
;
; Filename     sh_is_aar.pro
;
; Author       Rik Huygen (-->rh)
;
; Version      0.6
;
; History      0.1  14-07-94  -->RH  Documentation
;                   19-07-94  -->RH  First implementation (review by N.J.M. Sym)
;              0.2  31-03-95  -->HB  Unified header
;              0.4  04-10-95  -->NS  update to SAAR/YAAAR type /history
;              0.6  25-10-95  -->NS  Make compatible with old type AAR
;              0.7  01-12-95     EkW Correct for YAAAR
;#<

FUNCTION sh_is_aar, var

  N_TAGS_IN_AAR = 4                        ; SAAR.HISTORY added !!
  N_TAGS_MIN_AAR = 3                       ; Compatible with old type AAR

  if n_params() le 0 then return, 0
  if n_tags(var) lt N_TAGS_MIN_AAR then return, 0

  ; Check if the structure does contain a tag `TYPE' which has 'AAR'
  ; (so this can be 'SAAR' , 'LAAR' or 'YAAAR')

  names = tag_names(var)
  if MIN(WHERE(names eq 'TYPE')) ge 0 then begin
     if strpos(var.type,'AAR') ge 0 then begin
        if strpos(var.type,'YAAAR') ge 0 then begin
           if min(where(names eq 'DATA')) ge 0 then begin
              names = tag_names(var.data)
              if MIN(WHERE(names eq 'WAVE' )) ge 0 and $
                 MIN(WHERE(names eq 'FLAG' )) ge 0 and $ 
                 MIN(WHERE(names eq 'FLUX' )) ge 0 and $ 
                 MIN(WHERE(names eq 'STDEV'  )) ge 0 then return, 1
           endif 
        endif else begin
          if min(where(names eq 'DATA')) ge 0 then begin
             names = tag_names(var.data)
             if n_elements(names) eq 13 then return, 1   ;NEW AAR
             if n_elements(names) ge 6  then return, 1   ;OLD+NEW AAR
          endif
        endelse
     endif
  endif

  return, 0

END
