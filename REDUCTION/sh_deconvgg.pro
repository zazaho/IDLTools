; gaussische deconvolutie van deels opgeloste lijnen:

; Idee: sh_convgg heeft een tabel geproduceert van inratios van lijnen
; tov de instrumentele resolutie tegen waargenomen outratios tov van
; intrumentele resolutie

; Echter we weten nu na rijp beraad dat de FWHM is te berekenen uit de
; volgende simple formule: FWHM_out^2 = FWHM_in^2 + FHWM_res^2 

; input voor deconvolutie: 
; 1) waargenomen FWHM_out
; 2) instrumentele resolutie (tabel/sh_res/ncg7027): RP = l/dl
; 3) l

; output
; gaussische lijnbreedte FWHM

; methode:
; bereken FWHM_res uit res en l
; Result FWHM = sqrt(FWHM_out^2 + FWHM_res^2)

function sh_deconvgg,lambda,WObs,bnd,band=band,ngc7027=ngc7027, $
                     Werr=Werr,gausparam=gausparam,_extra=_extra
  
  if (n_params() lt 2) then begin
    print,'Please give wavelength,observed widths'
    return,0
  endif

  if (n_elements(lambda) lt 1) then begin
    print,'Warning please give an (array of) wavelengths as first parameter'
    return,0
  endif
    
  if (n_elements(WObs) lt 1) then begin
    print,'Warning please give an (array of) widths as second parameter'
    return,0
  endif
    
  if (n_elements(WObs) ne n_elements(lambda)) then begin
    print,'Lambda and width arrays are not of same size !!!'
    return,0
  endif
    
  If (n_params() eq 3) then begin
    band = bnd
  endif
  
  if not keyword_set(band) then begin
    band=sindgen(n_elements(lambda))
    for i=0,n_elements(band)-1 do begin
      band(i) = lambda_to_aot(lambda(i))
    endfor
  endif
  
  if ( n_elements(lambda) gt n_elements(band)) then begin
    temp = band
    band = sindgen(n_elements(lambda))
    band(0:n_elements(temp)-1) = temp
    band(n_elements(temp):n_elements(lambda)-1) = temp(n_elements(temp)-1)
  endif
  
  band = strtrim(strlowcase(band))
  
  if keyword_set(gausparam) then begin
    Wobs = 2.35842*WObs
  endif
  
  if (n_elements(ngc7027) ne 0 ) then begin
    if (ngc7027 eq 0) then begin
      rs = sh_res(lambda,band,_extra=_extra)
    endif else begin
      rs = sh_res(lambda,band,/ngc,_extra=_extra)
    endelse
  endif else begin
    rs = sh_res(lambda,band,/ngc,_extra=_extra)
  endelse
  
  WIn = sqrt(Wobs^2-(lambda/rs)^2)
  
; To get a feel for the uncertainty we 'variate' the rp by 10% and
; take the mean of the resulting variation in Win
  Err= ( sqrt(Wobs^2-(lambda/(rs*1.1))^2) - $
         sqrt(Wobs^2-(lambda/(rs/1.1))^2) )/2.0 
  
  if keyword_set(Werr) then WErr = Err
  return,WIn
end
