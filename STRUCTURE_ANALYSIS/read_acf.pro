;; read a acf table from the give directory.
;; by default it will read the catalog.data file but you can overrule
;; that by doing:
;; acf = read_acf("./",prefix="catalog_spl1")
;;
;; (SH Oct 16 2013) deactivated file_decompress
;; (SH Oct 16 2013) Added option to specify prefix

function read_acf,dir,prefix=prefix

  ;; read global variables
  @grid_global_definitions.idl
  default,prefix,acffile_prefix
  
;  oldquiet = !quiet
;  !quiet = 1
;  tmp = file_decompress(dir+'/'+prefix+'.data*',status=status)
;  !quiet=oldquiet
;
;  if status eq 0 then begin
;     data=(read_ascii(tmp)).(0)
;  endif else begin
;     data = !values.d_nan
;  endelse
;  file_decompress,/delete

  full_path = dir+'/'+prefix+'.data*'
  if file_test(full_path) then begin
     data=(read_ascii(full_path)).(0)
  endif else begin
     data = !values.d_nan
  endelse
  
  return,data

end
