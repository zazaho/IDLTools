;; simple function to create aar from input (array)
function shc_array2aar,p,status=status

  ;; protect against aar as input
  if is_aar(p) then begin
     status=1
     return,p
  endif

  ;; protect against aar-like as as input
  if size(p,/tname) eq "STRUCT" then begin
     if where(tag_names(p) eq "DATA") ne -1 then begin
        if where(tag_names(p.data) eq "WAVE") ne -1 and where(tag_names(p.data) eq "FLUX") ne -1 then begin
           out=sh_define_aar(length=n_elements(p.data.wave))
           out.data.wave=p.data.wave
           out.data.flux=p.data.flux
           if where(tag_names(p.data) eq "STDEV") ne -1 then begin
              out.data.stdev=p.data.stdev
           endif
           status=1
           return,out
        endif
     endif
  endif
  
  ndims = size(p,/n_dim)
  case ndims of
     2: begin
        out = sh_define_aar(length=n_elements(p[0,*]))
        out.data.wave = reform(p[0,*])
        out.data.flux = reform(p[1,*])
        status=1
        return,out
     end
     3: begin
        out = sh_define_aar(length=n_elements(p[0,*]))
        out.data.wave = reform(p[0,*])
        out.data.flux = reform(p[1,*])
        out.data.stdev = reform(p[2,*])
        status=1
        return,out
     end
     else: begin
        status=0
        return,!values.d_nan
     end
  endcase
end

;do some mathemathics on aar
; usage:
; aarout = sh_calcaar,aarin,operation = value
; where operation means:
; offset: add value to fluxes
; factor: multiply fluxes with value
; log value = 1 return Alog10 of wave and flux
;     value =-1 return exp10  of wave and flux
; flambda value = 1 convert Janskies (flux) to F_lambda (Watt/m m s mu)
;         value =-1 convert F_lambda to F_nu
; l_fl value = 1 convert Janskies (flux) to lambda*F_lambda (Watt/m m s)
;      value =-1 convert lambda F_lambda to F_nu
; nu value = 1 convert mu to Hz
;    value =-1 convert Hz to mu
; cgs value = 1 convert Jy to cgs units
;     value =-1 convert cgs to Jy units
; watt value = 1 convert to watt/mm s hz
; watt value = -1 convert to Jy
; lwave value = 1 return Alog10 of wave
;       value =-1 return exp10  of wave
; lflux value = 1 return Alog10 of flux
;       value =-1 return exp10  of flux
; add   value = fluxes/aar add fluxes to aar.data.flux
; subt  value = fluxes/aar subtract fluxes from aa.data.fluxr
; multi value = fluxes/aar multiply fluxes with aar.data.flux
; divi  value = fluxes/aar divide aar.data.flux by fluxes
; smoo  boxcar smooth fluxes with value
; plambda resultant flux is multiplied by lambda^value
; shift move the wavelength by shift

;;(SH Jun 10 1999) changed shc_spline into shc_interpol because it
;;behaves less wild
;;(SH Mar 20 2000) Added shift function
;(SH Dec  3 2014) Allow input as arrays [[wave],[flux]] or [[wave],[flux],[stdev]]

function sh_calcaar,in,offset=offset,factor=factor,log=log,$
                    flambda=flambda,nu=nu,cgs=cgs,watt=watt,lwave=lwave,$
                    lflux=lflux,add=add,subt=subt,divi=divi,mult=mult,$
                    lafl=lafl,smooth=smooth,plambda=plambda,poly=cpoly, $
                    shift=shift,quiet=quiet,help=help
  
  if ((n_params() eq 0 ) or keyword_set(help)) then begin
    print,'do some logarithmics on aar'
    print,'usage:'
    print,'aarout = sh_calcaar(aarin,operation = value)'
    print,'where operation means:'
    print,'offset: add value to fluxes'
    print,'factor: multiply fluxes with value'
    print,'log value = 1 return Alog10 of wave and flux'
    print,'    value =-1 return exp10  of wave and flux'
    print,'flambda value = 1 convert F_nu to F_lambda'
    print,'  value =-1 convert F_lambda to F_nu'
    print,'lafl value = 1 convert Janskies to lambda*F_lambda'
    print,'     value =-1 convert lambda F_lambda to F_nu'
    print,'nu value = 1 convert mu to Hz'
    print,'   value =-1 convert Hz to mu'
    print,'cgs value = 1 convert Jy to cgs units'
    print,'    value =-1 convert cgs to Jy units'
    print,'watt value = 1 convert to watt/mm s hz'
    print,'watt value = -1 convert to Jy'
    print,'lwave value = 1 return Alog10 of wave'
    print,'      value =-1 return exp10  of wave'
    print,'lflux value = 1 return Alog10 of flux'
    print,'      value =-1 return exp10  of flux'
    print,'add   value = fluxes/aar add fluxes to aar.data.flux'
    print,'subt  value = fluxes/aar subtract fluxes from aar.data.flux'
    print,'multi value = fluxes/aar multiply fluxes with aar.data.flux'
    print,'divi  value = fluxes/aar divide aar.data.flux by fluxes'
    print,'plambda resultant flux is multiplied by lambda^value'
    print,'smoo  boxcar smooth fluxes with value'
    print,'poly  return polynome with coefs in value'
    print,'shift move the wavelength by shift'
    print,'quiet do not print message'
    return,0
  endif

  in_aar=shc_array2aar(in)
; Check for valid input
  if not is_aar(in_aar) then error,'F','No valid AAR structure specified!'
  
  ret = in_aar
  win = ret.data.wave
  wout = win
  fin = ret.data.flux
  fout = fin
  sin = ret.data.stdev
  sout = sin
  
  IF keyword_set(quiet) THEN BEGIN
    openw,lun,'/dev/null',/get_lun
  ENDIF ELSE BEGIN
    lun =-1
  ENDELSE
  
; Multiple operations are allowed, But the order might not make sense!
  
  if (n_elements(cpoly) gt 0) then begin
    fout = poly(wout,cpoly)
    printf,lun,'contructed polynome'
  endif
  
  if keyword_set(plambda) then begin
    fout = fout*wout^plambda
    sout = sout*wout^plambda
    printf,lun,'Multiplied all fluxes lambda^',plambda
  endif

  if keyword_set(factor) then begin
    fout = fout*factor
    sout = sout*factor
    printf,lun,'Multiplied all fluxes by: ',factor
  endif

  if keyword_set(offset) then begin
    fout = fout + offset
    printf,lun,'Offset all fluxes by: ',offset
  endif

  if keyword_set(smooth) then begin
    fout = smooth(fout,smooth)
    fout = smooth(fout,smooth)
    printf,lun,'Smooth fluxes by: ',smooth
  endif
  
  if keyword_set(flambda) then begin
    if (flambda eq 1) then begin
      fout = fout*3e-12/wout^2
      sout = sout*3e-12/wout^2
      printf,lun,'converted the fluxes to f_lambda'
    endif else begin
      fout = fout*wout^2/3e-12
      sout = sout*wout^2/3e-12
      printf,lun,'converted the fluxes to f_nu'
    endelse
  endif

  if keyword_set(lafl) then begin
    if (lafl eq 1) then begin
      fout = fout*3e-12/wout
      sout = sout*3e-12/wout
      printf,lun,'converted the fluxes to lambda*f_lambda'
    endif else begin
      fout = fout*wout/3e-12
      sout = sout*wout/3e-12
      printf,lun,'converted the fluxes to f_nu'
    endelse
  endif

  if keyword_set(nu) then begin
    wout = 3e14/wout
    printf,lun,'converted the waves to Hz or vice-versa'
  endif

  if keyword_set(cgs) then begin
    if (cgs eq 1) then begin
      fout = fout*1e-23
      sout = sout*1e-23
      printf,lun,'converted the fluxes to cgs units'
    endif else begin
      fout = fout*1e23
      sout = sout*1e23
      printf,lun,'converted the fluxes to Jy'
    endelse
  endif

  if keyword_set(watt) then begin
    if (watt eq 1) then begin
      fout = fout*1e-26
      sout = sout*1e-26
      printf,lun,'converted the fluxes to watts/m^s/s/hz'
    endif else begin
      fout = fout*1e26
      sout = sout*1e26
      printf,lun,'converted the fluxes to Jy'
    endelse
  endif

  if keyword_set(log) then begin
    if (log eq 1) then begin
      sout = abs(sout)/fout*alog(10)
      fout = alog10(fout)
      wout = alog10(wout)
      printf,lun,'Taken the logarithm of the wave,flux'
    endif else begin
      fout = 10^fout
      wout = 10^wout
      sout = sout*fout/alog(10)
      printf,lun,'Taken the exp10 of the wave,flux'
    endelse
  endif

  if keyword_set(lwave) then begin
    if (lwave eq 1) then begin
      wout = alog10(wout)
      printf,lun,'taken the alog10 of wave'
    endif else begin
      wout = 10^wout
      printf,lun,'taken the exp10 of wave'
    endelse
  endif

  if keyword_set(shift) then begin
    wout = wout+shift
    printf,lun,'Shifted the wavelength by: '+f2s(shift,2)
  endif

  if keyword_set(lflux) then begin
    if (lflux eq 1) then begin
      sout = abs(sout)/fout*alog(10)
      fout = alog10(fout)
      printf,lun,'taken the alog10 of flux'
    endif else begin
      fout = 10^fout
      sout = sout*fout/alog(10)
      printf,lun,'taken the exp10 of flux'
    endelse
  endif

  ;; operations between two aars
  if keyword_set(add) then begin
     add_aar=shc_array2aar(add)
     if (is_aar(add_aar)) then begin
        if (same_array(add_aar.data.wave,wout) eq 1) then begin
           fout = fout + add_aar.data.flux
           printf,lun,'added fluxes'
        endif else begin
           fout = fout + shc_interpol(add_aar.data.wave,add_aar.data.flux,wout)
           printf,lun,'added interpolated fluxes'
        endelse
     endif else begin
        if (n_elements(add) eq n_elements(wout)) then begin
           fout = fout + add
           printf,lun,'added flux array'
      endif else begin
         printf,lun,'cannot add different size structure'
      endelse
   endelse
  endif
  
  if keyword_set(subt) then begin
     subt_aar=shc_array2aar(subt)
     if (is_aar(subt_aar)) then begin
        if (same_array(subt_aar.data.wave,wout) eq 1) then begin
           fout = fout - subt_aar.data.flux
           printf,lun,'subtracted fluxes'
        endif else begin
           fout = fout - shc_interpol(subt_aar.data.wave,subt_aar.data.flux,wout)
           printf,lun,'subtracted interpolated fluxes'
        endelse
     endif else begin
        if (n_elements(subt) eq n_elements(wout)) then begin
           fout = fout - subt
           printf,lun,'subtracted flux array'
        endif else begin
           printf,lun,'cannot subtract different size structure'
        endelse
     endelse
  endif

  if keyword_set(mult) then begin
     mult_aar=shc_array2aar(mult)
     if (is_aar(mult_aar)) then begin
        if (same_array(mult_aar.data.wave,wout) eq 1) then begin
           fout = fout * mult_aar.data.flux
           sout = sout * mult_aar.data.flux
           printf,lun,'multiplied fluxes'
        endif else begin
           fout = fout * shc_interpol(mult_aar.data.wave,mult_aar.data.flux,wout)
           sout = sout * shc_interpol(mult_aar.data.wave,mult_aar.data.flux,wout)
        printf,lun,'multiplied interpolated fluxes'
      endelse
    endif else begin
      if (n_elements(mult) eq n_elements(wout)) then begin
        fout = fout * mult
        sout = sout * mult
        printf,lun,'multiplied flux array'
      endif else begin
        printf,lun,'cannot multiply different size structure'
      endelse
    endelse
  endif

  if keyword_set(divi) then begin
     divi_aar=shc_array2aar(divi)
     if (is_aar(divi_aar)) then begin
        if (same_array(divi_aar.data.wave,wout) eq 1) then begin
           fout = fout / divi_aar.data.flux
           sout = sout / divi_aar.data.flux
           printf,lun,'divided fluxes'
        endif else begin
           fout = fout / shc_interpol(divi_aar.data.wave,divi_aar.data.flux,wout)
           sout = sout / shc_interpol(divi_aar.data.wave,divi_aar.data.flux,wout)
           printf,lun,'divided interpolated fluxes'
        endelse
     endif else begin
        if (n_elements(divi) eq n_elements(wout)) then begin
           fout = fout / divi
           sout = sout / divi
           printf,lun,'divided flux array'
        endif else begin
           printf,lun,'cannot divide different size structure'
        endelse
     endelse
  endif
  
  IF keyword_set(quiet) THEN BEGIN
    close,lun
    free_lun,lun
  ENDIF

  ret.data.wave=wout
  ret.data.flux=fout
  ret.data.stdev=sout
  return,ret
end
