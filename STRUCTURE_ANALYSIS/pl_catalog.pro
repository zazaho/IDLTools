;; plot catalog table from the give directory.
;; by default it will read the catalog.data file but you can overrule
;; that by doing:
;; pl_catalog,"./",prefix="catalog_spl1"
;;
;; (SH Oct 16 2013) Added option to specify prefix

PRO pl_catalog,dir,_extra=_extra
  ;; read global variables
  @grid_global_definitions.idl

  catalog = read_catalog(dir,_extra=_extra)

  if size(catalog,/n_dimensions) eq 2 then begin
     pl,(catalog[0,*]-npixels/2d0)*pixelsize,(catalog[1,*]-npixels/2d0)*pixelsize, $
        ps=3, $
        xtitle='x ["]', $
        ytitle='y ["]', $
        _extra=_extra
  endif else begin
     message,/info,'something went wrong reading the catalog from: '+dir
  endelse
  
END
