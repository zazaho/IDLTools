;; plot acf table from the give directory.
;; by default it will read the acf.data file but you can overrule
;; that by doing:
;; pl_acf,"./",prefix="acf_spl1"
;;
;; (SH Oct 16 2013) Added option to specify prefix

PRO pl_acf,dir,show_ngc=show_ngc,_extra=_extra
  ;; read global variables
  @grid_global_definitions.idl

  ref=transpose([ $
      [1.00,1.06,1.13,1.20,1.27,1.35,1.43,1.52,1.61,1.71,1.82,1.93,2.05,2.17,2.31,2.45,2.60,2.76,2.93,3.11,3.30,3.51,3.72,3.95,4.19,4.45,4.73,5.02,5.33,5.65,6.00,6.37,6.76,7.18,7.62,8.09,8.59,9.12,9.68,10.28,10.91,11.58,12.29,13.05,13.85,14.71,15.61,16.57,17.59,18.68,19.83,21.05,22.34,23.72,25.18,26.73,28.37,30.12,31.97,33.94,36.03,38.25,40.61,43.11,45.76,48.58,51.57,54.74,58.11,61.69,65.49,69.52,73.80,78.34,83.16,88.28,93.72,99.49,105.61,112.11,119.01,126.34,134.12,142.37,151.14], $
      [6.48,6.39,6.22,6.22,6.26,6.28,6.15,5.97,5.90,5.87,5.81,5.79,5.75,5.68,5.66,5.61,5.54,5.50,5.44,5.37,5.32,5.28,5.20,5.14,5.10,5.07,5.01,4.96,4.91,4.86,4.80,4.75,4.69,4.65,4.60,4.55,4.52,4.48,4.44,4.40,4.36,4.31,4.27,4.22,4.17,4.12,4.06,3.99,3.92,3.85,3.78,3.70,3.61,3.52,3.43,3.34,3.24,3.14,3.04,2.95,2.85,2.75,2.65,2.56,2.46,2.37,2.28,2.19,2.10,2.01,1.93,1.85,1.78,1.70,1.64,1.58,1.52,1.47,1.42,1.38,1.34,1.30,1.26,1.23,1.21], $
      [0.54,0.49,0.45,0.41,0.37,0.34,0.31,0.28,0.25,0.23,0.21,0.19,0.17,0.15,0.14,0.12,0.11,0.10,0.09,0.08,0.07,0.07,0.06,0.05,0.05,0.04,0.04,0.04,0.03,0.03,0.03,0.02,0.02,0.02,0.02,0.02,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00] $
                ])
  
  foo = execute('acf='+read_command+'(dir,_extra=_extra)') 
  if size(acf,/n_dimensions) eq 2 then begin
     pl,acf, $
        ps=0,thick=3,xlog=1,ylog=1,charsize=1.5, $
        xtitle='Separations ["]', $
        ytitle='Auto-correlation value', $
        _extra=_extra

     ;; this allows to put a reference acf (hint: n66) on top
     if n_elements(reference_file) eq 1 then begin
        if file_test(reference_file) ne 0 then begin
           ref=(read_ascii(reference_file)).(0)
           pl,ref,/oplot,thick=3,_extra=_extra
        endif
     endif
  
     if keyword_set(show_ngc) then begin
        pl,ref,ps=0,thick=3,_extra=_extra,/oplot
     endif
     
  endif else begin
     message,/info,'something went wrong reading the acf from: '+dir
  endelse
  
END
