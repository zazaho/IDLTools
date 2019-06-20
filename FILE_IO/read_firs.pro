; Function to read an spitzer IRS tbl file into a sws/aar
; copies all data structures that are available
; also simulates the sws structure by moving some bits around 

FUNCTION read_firs,fname
  if file_test(fname) eq 1 then begin
     tmp = read_fipac(fname)

     wave = col_mr(tmp,'wavelength')
     nwave = n_elements(wave)
     
     if n_elements(wave) eq 0 then begin
        message,/info,'File does not seem to contain a spectrum'
        return,!values_d_nan
     endif
     
     ;; for now we just assume that since there is wavelength data the
     ;; other tags will exist as well.
     out = sh_define_aar(len=nwave)
     out.header  = tmp.header
     out.history = tmp.history

     out.data.wave = wave
     out.data.flux = col_mr(tmp,'flux_density')
     out.data.stdev = col_mr(tmp,'error')
     out.data.line = col_mr(tmp,'order')
     out.data.status = col_mr(tmp,'bit-flag')

     return,out
     ;;sh_calcaar(out,fl=-1,fact=1e4)
  endif else begin
     message,/info,'Could not open '+fname
     return,!values_d_nan
  endelse
END
