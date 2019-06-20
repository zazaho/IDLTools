;; calculate the acf from a simulation in the given directory
;; by default it will read the catalog.data file but you can overrule
;; that by doing:
;; calc_acf,"./",prefix="catalog_spl1"
;;
;; (SH Oct 16 2013) Added option to specify prefix

pro calc_acf,dir,prefix=prefix,_extra=_extra

  ;; read global variables
  @grid_global_definitions.idl
  default,prefix,acffile_prefix

  pushd,dir
  if file_test(prefix+'.data*') ne 0 then begin
     print,'ACF output file already exists, not redoing the work'
  endif else begin
     ;; Use the whole field of view as the mask
     polygon = [[0.,0.],[0.,1.],[1.,1.],[1.,0.],[0.,0.]]*npixels
     tpcf = tpcf_stars_exact( $
            infofile=catalogfile_prefix+'.info', $
            output=acffile_prefix+'.data', $
            usemasking=1, $
            fov_polygon=polygon, $
            annulistep=acf_annulistep, $
            nannuli=acf_nannuli, $
            dolog=acf_dolog, $
            annuli_min=acf_annuli_min, $
            annuli_max=acf_annuli_max $
                            )
  endelse     
  popd
end
