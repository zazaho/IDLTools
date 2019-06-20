;+
; NAME: tpcf_stars_exact
;
; PURPOSE:
;  calculate a two point (autocorrelation function) from a stellar catalogue
;
; CATEGORY: Data Analysis
;
; CALLING SEQUENCE:
;  result = tpcf_stars_exact() $
;
; INPUTS:
;
; OPTIONAL INPUTS:
;   infofile=infofile: filename of info file ['tpcf_stars.info']
;   outputfile=outputfile: filename of output file [photofile+'_tpcf_stars.dat']
;   psfile=psfile: not used 
;   imagetitle=imagetitle: not used
;   imagesub=imagesub: not used
;   fov_polygon=fov_polygon: polygon of encompassing the observed FOV
;                            (in orignal pixels coordinates)
;   nannuli=nannuli: number of annuli to calculate [100]
;   
; KEYWORD PARAMETERS:
;   /usemasking: if set we use the mask and calculate the area covered
;   /dolog: if set do annuli in log space
;   /help: if set show this help
;
; MODIFICATION HISTORY:
;  (DG May 22 2013): Original implementation of the TPCF according to 
;                    Peebles 1980, Zahng et al. 2002, Scheepmaker et al. 2009
;  (SH May 31 2013): Initial rewrite from DG
;  (SH May 31 2013): fixed bugs and added mask_pixelsize option
;  (SH May 31 2013): error calculations corrected
;  (DG June 1 2013): added option for entering linear annuli radial steps in arcsec
;                    (overwrites 'dolog' option to 'false' and 'nannuli' to the appropriate number of annuli) 
;  (DG June 3 2013): Option '/NAN' was added in "total(surface_density_matrix,2)"
;                    and "stddev(surface_density_matrix,dimension=2)" so than no "-NaN"
;                    results appear in TPCF and its errors.
;  (SH Jun 18 2013) Added some options to simulate fields simulation=['no','field','cluster','complex''
;                   you can say simulation='cluster,nstars=1000,fwhm=300'
;                   to tune the parameters
;  (SH Jun 20 2013) Change the way the FWHM is specified (from
;                   percentage of FOV to arcsec
;                   Added option to calculate tpcf only for the stars
;                   inside a box while retaining all stars
;  (SH Oct  7 2013) Allow to read simulation tables
;- 

;; to parse a string like 'cluster,nstars=1000,fwhm=300' for first parameter
function tpcf_stars_exact_get_simulation_type,instr

  result = 'no'
  str = strlowcase(instr)
  ;; find parts separated by ,
  param_strings = strsplit(instr,',',/extract)
  value = strcompress(param_strings[0],/remove_all)
  if value ne '' then result=value
  return,result
end

;; to parse a string like 'cluster,nstars=1000,fwhm=300' for the
;; keyword values
function tpcf_stars_exact_get_simulation_keyword,instr,inkeyword, $
   default=default
  
  if n_elements(default) eq 1 then result=default else result = !values.d_nan

  ;; lowercase for comparing
  str = strlowcase(instr)
  keyword = strlowcase(inkeyword)

  ;; find parts separated by ,
  param_strings = strsplit(instr,',',/extract)
  idx = where(strpos(param_strings,'=') ne -1,cnt)
  if cnt eq 0 then return,result

  ;; find parts containing =
  param_strings = param_strings[idx]
  idx = where(strpos(param_strings,keyword) ne -1,cnt)
  if cnt eq 0 then return,result
  
  ;; take the first match
  param_string = param_strings[idx[0]]
  
  ;; extract the part after =
  value = stregex(param_string,'.*=([^ ]*)',/extract,/subexpr)
  
  ;; if it worked make a double
  if n_elements(value) eq 2 then result=double(value[1])

  return,result
end

function tpcf_stars_exact, $
   infofile=infofile, $
   outputfile=outputfile, $
   psfile=psfile, $
   imagetitle=imagetitle, $
   imagesub=imagesub, $
   usemasking=usemasking, $
   mask_pixelsize=mask_pixelsize, $
   fov_polygon=fov_polygon, $
   nannuli=nannuli, $
   annulistep=annulistep, $
   annuli_min=annuli_min, $
   annuli_max=annuli_max, $
   dolog=dolog, $
   simulation=simulation, $
   box=box, $
   mask_map=mask_map

  if keyword_set(help) then begin
     doc_library,'tpcf_stars_exact'
     return,!values.d_nan
  endif

  ;; set some default values if keywords are not given
  default,infofile,'tpcf_stars.info'
  default,psfile,'tpcf_stars.ps'
  default,fov_polygon,[[0.5,6291.6407], [4124.1371,6154.4551], [4196.7648,5161.8771], [4906.9019,5145.7376], [5011.8085,4249.9965], [6238.409,4290.3452], [6827.5,336.17258], [2607.026,45.661939], [2477.9102,1110.8676], [1186.7518,1078.5887], [1106.0544,2184.143], [178.03428,2200.2825]]
  default,nannuli,100L
  default,usemasking,1
  default,dolog,1
  default,annulistep,0
  default,imagetitle,''
  default,imagesub,''
  default,mask_pixelsize,1d0
  default,simulation,'no'

  ;; get the data from the infofile
  openr,lun_infofile,infofile,/get_lun
  photofile=''
  readf,lun_infofile,photofile
  readf,lun_infofile,pxsz
  readf,lun_infofile,toteffpx
  readf,lun_infofile,arcsec2pc
  readf,lun_infofile,oxpxmn,  oxpxmx,  oypxmn,  oypxmx
  readf,lun_infofile,ximpxmn, ximpxmx, yimpxmn, yimpxmx
  close,lun_infofile
  free_lun,lun_infofile

  ;; some handy variable to not have to test minmax all the time
  xmin = min([ximpxmn,ximpxmx],max=xmax)
  ymin = min([yimpxmn,yimpxmx],max=ymax)
  
  ;; default the output file based on the photofile name
  default,outputfile,'./'+photofile+'_tpcf_stars.dat' ;;; OUTPUT FILE FOR CLUSTER CATALOG INFO

  ;; allow a simulated field to benchmark the code
  case tpcf_stars_exact_get_simulation_type(simulation) of
     'field': begin
        nstars = tpcf_stars_exact_get_simulation_keyword(simulation,'nstars',default=nstars)
        xpx = xmin + (xmax-xmin)*randomu(seed,nstars)
        ypx = ymin + (ymax-ymin)*randomu(seed,nstars)
     end
     'cluster': begin
        nstars = tpcf_stars_exact_get_simulation_keyword(simulation,'nstars',default=nstars)
        fwhm = tpcf_stars_exact_get_simulation_keyword(simulation,'fwhm',default=0.2)
        ;; fwhm is now in arcsec
        sigma_parameter = fwhm/(2d0*sqrt(2d0*alog(2d0)))/pxsz
        xpx = (xmin+xmax)/2d0 + sigma_parameter*randomn(seed,nstars)
        ypx = (ymin+ymax)/2d0 + sigma_parameter*randomn(seed,nstars)
     end
     'complex': begin
        nstars = tpcf_stars_exact_get_simulation_keyword(simulation,'nstars',default=nstars)
        ;; fwhm compared to the size of the field
        fwhm = tpcf_stars_exact_get_simulation_keyword(simulation,'fwhm',default=0.2)
        ;; fraction of stars in the cluster
        fraction = tpcf_stars_exact_get_simulation_keyword(simulation,'fraction',default=0.5)
        ;; fwhm is now in arcsec
        sigma_parameter = fwhm/(2d0*sqrt(2d0*alog(2d0)))/pxsz
        xpx1 = (xmin+xmax)/2d0 + sigma_parameter*randomn(seed,nstars)
        ypx1 = (ymin+ymax)/2d0 + sigma_parameter*randomn(seed,nstars)
        xpx2 = xmin + (xmax-xmin)*randomu(seed,(1.0-fraction)*nstars)
        ypx2 = ymin + (ymax-ymin)*randomu(seed,(1.0-fraction)*nstars)
        xpx = [xpx1,xpx2]
        ypx = [ypx1,ypx2]
     end
     else: begin
        ;; read in all the data from the photofile
        foo = (read_ascii(photofile + '.data')).(0)

        ;; allow to read output from simulations which contain simply
        ;; x,y (,z) data in pixels coordinates and the original
        ;; photometry tables
        foo_dimensions = size(foo,/dimensions)
        if foo_dimensions[0] gt 11 then begin
           ;; these are the columns in the photo files
           ;; id, rah, ram, ras, decd, decm, decs, radeg, decdeg, xpx, ypx, mv, dmv, mi, dmi
           xpx = reform(foo[ 9,*])
           ypx = reform(foo[10,*])
        endif else begin
           xpx = reform(foo[0,*])
           ypx = reform(foo[1,*])
        endelse           
        ;; save some memory
        nstars = n_elements(xpx)
;;        delvar,foo
     end
  endcase
  
  if usemasking then begin
     ;; to be able to calculate the area that falls inside the mask we
     ;; define a coordinate grid (all in pixel units)
     ;; the grid covers the full  original data at the pixel size 
     mask_rebin_factor = mask_pixelsize/pxsz
     
     nx = long(CEIL((oxpxmx-oxpxmn)/mask_rebin_factor))
     ny = long(CEIL((oypxmx-oypxmn)/mask_rebin_factor))
     foo = lindgen(nx, ny)
     x_pix_arcsec = (foo mod nx)*mask_pixelsize
     y_pix_arcsec = (foo / nx)  *mask_pixelsize
     
     fov_polygon_arcsec = fov_polygon*pxsz
     ;; apply the polygon to the coordinate grid to make a mask image
     o = obj_new('IDLanROI', fov_polygon_arcsec)
     test_points = o->containspoints(x_pix_arcsec, y_pix_arcsec) ; 1: inside polygon, 0: outside polygon
     mask = reform(test_points, nx, ny)

     ;; allow for an additions pixel mask given by the caller
     if n_elements(mask_map) eq n_elements(mask) then begin
        mask=(mask and mask_map)
     endif

     ;; make sure we don't uses stars outside of the area/mask
     idx_inside = where(mask[xpx*pxsz/mask_pixelsize,ypx*pxsz/mask_pixelsize] eq 1,cnt)
  endif else begin
     ;; make sure we don't uses stars outside of the area/mask
     idx_inside = where(xpx ge xmin and xpx le xmax and ypx ge ymin and ypx le ymax,cnt)
  endelse

  xpx = xpx[idx_inside]
  ypx = ypx[idx_inside]
  nstars=cnt
  
  ;; we allow a box to be defined. The tpcf will be calculated only
  ;; based on the stars inside the box. This differs from masking in
  ;; that the stars outside the box are still used to calculated
  ;; stellar density around the stars in the box. Basically this is
  ;; implemented so that we can calculate a CF which is not affected
  ;; by edge effects
  
  ;; x0,x1,y0,y1 in pixel coords
  if n_elements(box) eq 4 then begin
     box_xmin = min(box[0:1],max=box_xmax)
     box_ymin = min(box[2:3],max=box_ymax)
     idx_inside_box = where(xpx ge box_xmin and xpx le box_xmax and ypx ge box_ymin and ypx le box_ymax,cnt)
     if cnt eq 0 then begin
        message,/info,'The box you specified does not contain any stars'
        return,!values.d_nan
     endif
     xpx_to_use = xpx[idx_inside_box]
     ypx_to_use = ypx[idx_inside_box]
     nstars_to_use = cnt
  endif else begin
     xpx_to_use = xpx
     ypx_to_use = ypx
     nstars_to_use = n_elements(xpx)
  endelse

  ;; here we put in some logic to calculate the right annuli
  ;; based on the smallest and largest distances and
  ;; also whether or not use logarithmic spacing ...
  ;; track the units! (pixel or arcsec)

  ;; in arcsec !!
  default,annuli_min,1.0                                ;; 1 arcsec in pixels
  min_xpx_to_use = min(xpx_to_use,max=max_xpx_to_use)
  min_ypx_to_use = min(ypx_to_use,max=max_ypx_to_use)
  default,annuli_max,sqrt((max_xpx_to_use-min_xpx_to_use)^2+(max_ypx_to_use-min_ypx_to_use)^2)*pxsz
  
  if annulistep ne 0 then begin
     nannuli = ROUND( (annuli_max - annuli_min) / (annulistep) ) ;; number of annuli based on required radial step size
     dolog = 0
  endif

  if dolog then begin
     annuli = annuli_min*10.^(alog10(annuli_max/annuli_min)*dindgen(nannuli)/(nannuli-1.))
  endif else begin
     annuli = annuli_min+(annuli_max - annuli_min)*dindgen(nannuli)/(nannuli-1.)
  endelse

  annuli_arcsec = annuli

  ;; in px
  annuli = annuli/pxsz

  ;; make a array to hold the results
  surface_density_matrix = make_array(nannuli,nstars_to_use,value=0d0)
  stellar_counts_matrix  = make_array(nannuli,nstars_to_use,value=0d0)

  ;; loop over each star
  for idx_this_star=0,nstars_to_use-1L do begin
     print,'treating star: '+string(idx_this_star)
     x_this_star = xpx_to_use[idx_this_star]
     y_this_star = ypx_to_use[idx_this_star]

     ;; calculated the distance of the stars and in the coordinate grid
     ;; to this star
     r_all_stars_to_this_star=sqrt((xpx-x_this_star)^2+(ypx-y_this_star)^2) ;; in pixels
     
     if usemasking then begin
        r_grid_to_this_star_arcsec= sqrt((x_pix_arcsec-x_this_star*pxsz)^2+(y_pix_arcsec-y_this_star*pxsz)^2) ;; in arcsec
     endif

     ;; in principe we could calculate a minimum annulus radius here where
     ;; the annuli start to touch the edge of the mask (TBD)

     ;; loop over the annuli
     for idx_this_annulus=0,nannuli-1 do begin
        idx_inside = where(r_all_stars_to_this_star le annuli[idx_this_annulus],cnt_stars)
        
        if usemasking then begin
           idx_inside_grid_and_mask = where( $
                                      (r_grid_to_this_star_arcsec le annuli_arcsec[idx_this_annulus]) and $
                                      mask eq 1,cnt_in_grid_and_mask)
           effective_area = cnt_in_grid_and_mask*mask_pixelsize^2 ;; arcsec^2
        endif else begin
           effective_area = (!dpi*annuli[idx_this_annulus]^2)*pxsz^2 ;;arcsec^2
        endelse
        
        stellar_counts_matrix [idx_this_annulus,idx_this_star] = cnt_stars
        ;; -1L because we don't count the star itself
        surface_density_matrix[idx_this_annulus,idx_this_star] = (cnt_stars-1L)/(effective_area) ;; stars/arcsec^2
        
     endfor
  endfor

  total_surface_density_array = total(surface_density_matrix,2, /NAN)
  
  if usemasking then begin
     total_effective_surface = total(mask)*mask_pixelsize^2
  endif else begin
     total_effective_surface = abs(ximpxmx-ximpxmn)*abs(yimpxmx-yimpxmn)*pxsz^2
  endelse

  ;; if we have a boxed calcuation we take the size of the box as the
  ;; effective area
  if n_elements(box) eq 4 then begin
     if usemasking then begin
        message,/info,'Warning the the calculation of the effective area for masking and an box is not implemented!'
     endif
     total_effective_surface = (box_xmax-box_xmin)*(box_ymax-box_ymin)*pxsz^2.
  endif
     
  average_stellar_density = (nstars_to_use/total_effective_surface)
  tpcf = total_surface_density_array/(nstars_to_use*average_stellar_density)

  ;; this is the formal uncertainy as given by Scheepmaker 2009)
  delta_tpcf = sqrt(nstars_to_use)*(0.5*total(stellar_counts_matrix*(stellar_counts_matrix-1L),2))^(-0.5)

  ;; but we prefer the dispersion as measured
  ;; the sqrt(nstars) come from error combination of each stddev on
  ;; surface density propagation of Nstars
  dispersion_tpcf = sqrt(nstars_to_use)*stddev(surface_density_matrix,dimension=2, /NAN)/(nstars_to_use*average_stellar_density)

  results = transpose([[annuli_arcsec],[tpcf],[delta_tpcf],[dispersion_tpcf]])

  ;; dump the results in a fine
  openw, lun_outputfile, outputfile,/get_lun
  printf, lun_outputfile, results, FORMAT = '(4E18.2)'
  close,lun_outputfile
  free_lun,lun_outputfile
  
  return,results

end

;AS = FINDGEN(16) * (!!DPI*2/15.)
;USERSYM, COS(AS), SIN(AS), /FILL
;
;answ='y'
;;READ,' Save output to a Postscript file ? [y/n] : ', answ
;IF answ EQ 'y' OR answ EQ 'Y' THEN BEGIN
;  SET_PLOT, 'ps'
;  DEVICE,  FILENAME=psfile, /color
;ENDIF
;
;;if answ eq 'y' or answ eq 'Y' then begin
;;	loadct, 8 ;;; GREEN
;;	loadct, 1 ;;; BLUE
;;	loadct, 7 ;;; RED
;;endif else begin
;;	set_plot, 'x'
;;	device,decomposed=0
;;	window,0,retain=2,xsize=750,ysize=1000
;;	loadct,39
;;endelse
;
;plot, tpcf(0,*), tpcf(1,*),  /XLOG, /YLOG, /NODATA, CHARSIZE=1.55, CHARTHICK=4, SYMsize = 0.115, XTHICK = 4, YTHICK = 4, $
;$ ; PSYM=8, SYMsize = .65, $
; YTITLE=textoidl('1+\xi(r)'), $
; XTITLE=textoidl('r (arcsec)'), $
; XRANGE=[0.5,2*thmax], XSTYLE=1
;; YRANGE=[(min(a(6,*))-60.)*0.085,(max(a(6,*))+60.)*0.085], YSTYLE=1, $
;
;OPLOT, tpcf(0,*), tpcf(1,*), LINESTYLE=0, COLOR=55, THICK = 4
;ERRPLOT, tpcf(0,*), tpcf(1,*)-tpcf(2,*), tpcf(1,*)+tpcf(2,*), THICK = 4, COLOR=55
;
;;X = [30, 60]
;;Y1 = [0.4, 0.4]
;;OPLOT, X, Y1, LINESTYLE=2, COLOR=255, THICK = 4
;;Y2 = [0.7, 0.7]
;;OPLOT, X, Y2, LINESTYLE=0, COLOR=55, THICK = 4
;
;IF answ EQ 'y' OR answ EQ 'Y' THEN BEGIN
; device,/close
; set_plot,'x'
; PRINT, ' Finished! The file was saved as: ', psfile
;ENDIF ELSE print, ' Finished!'

