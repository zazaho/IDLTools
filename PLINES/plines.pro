;; put a cross defined in ra,dec on the current map
pro plines_plot_triangle,position,color=color
  
  default,color,kleur('salmon')

  ;; could add sex2deg option here XXX
  coord_device=convert_coord(position[0],position[1],/data,/to_device)
  
  ;; basic triangle
  x_triang=[-1,0,1,-1]
  y_triang=[-.5,1,-.5,-.5]
  
  ;; we want the mark in device coordinates and size so that
  ;; it always has the same size on each page
  tmp=convert_coord([0,1],[0,1],/normal,/to_device)
  diagonal_length_device=sqrt((tmp[0,1]-tmp[0,0])^2+(tmp[1,1]-tmp[1,0])^2)

  ;; use a cross size of 5% of the diagonal of the figure
  plots,0.01*x_triang*diagonal_length_device+coord_device[0], $
        0.01*y_triang*diagonal_length_device+coord_device[1], $
        /device,thick=6,color=color
  
end

;; put a cross defined in ra,dec on the current map
pro plines_plot_mark,position,color=color
  
  default,color,kleur('black')
  ;; could add sex2deg option here XXX
  coord_device=convert_coord(position[0],position[1],/data,/to_device)
  
  ;; basic cross
  x_cross=[-1,1,0,-1,1]
  y_cross=[-1,1,0,1,-1]
  
  ;; more fancy cross
  x_cross=[-1,+1,+0,+0,-1,+1,+0,+0,-5,-5,-5,-5,+5,+5,+5]/5.
  y_cross=[-5,-5,-5,+5,+5,+5,+5,+0,+0,-1,+1,+0,+0,-1,+1]/5.
  
  ;; we want the mark in device coordinates and size so that
  ;; it always has the same size on each page
  tmp=convert_coord([0,1],[0,1],/normal,/to_device)
  diagonal_length_device=sqrt((tmp[0,1]-tmp[0,0])^2+(tmp[1,1]-tmp[1,0])^2)
  ;; use a cross size of 5% of the diagonal of the figure
  plots,0.01*x_cross*diagonal_length_device+coord_device[0], $
        0.01*y_cross*diagonal_length_device+coord_device[1], $
        /device,thick=6,color=color
end

;; determine the box in ra,dec that fits around a image based on the
;; header info
function plines_header_coord_range,header
  if n_elements(header) gt 1 then begin
     nx = sxpar(header,'NAXIS1')
     ny = sxpar(header,'NAXIS2')
     ;; the coordinates of the outer edges of the corner pixels
     xyad,header,[-0.5,nx-1+0.5,nx-1+0.5,-0.5],[-0.5,-0.5,ny-1+0.5,ny-1+0.5],ra,dec
     ra_min=min(ra,max=ra_max)
     dec_min=min(dec,max=dec_max)
  endif else begin
     ;; bogus values
     ra_min=400d0
     ra_max=-400d0
     dec_min=900d0
     dec_max=-900d0
  endelse
  return,[ra_min,dec_min,ra_max,dec_max]
end

;; find the coordinates for the pixel with the maximum flux in a map
;; restored from a PACSMAN sav structure
function plines_radec_maxraster,savefilename,dirname=dirname
  
  if n_elements(savefilename) eq 0 then begin
     if n_elements(dirname) eq 1 then begin
        savefilename=dirname+path_sep()+'cubeAcubeB.sav'
     endif
  endif

  files_found=file_search(savefilename,count=n_files_found)
  case n_files_found of
     0: begin
        message,/info,'The .sav file containing the PACSMAN results cannot be found'
        message,/info,'I tried: '+savefilename
        return,""
     end
     1:begin
        ;; this is the default behaviour
        restore,files_found
     end
     else: begin
        ;; here we need some logic to decide which file to use
        message,/info,'multiple save files found, restoring the first one: '+files_found[0]
        restore,files_found[0]
     end
  endcase
  
  flux_brightest = max(cube_lres.flux,idx_brightest)
  ra=cube_lres[idx_brightest].ra
  dec=cube_lres[idx_brightest].dec
  return,[ra,dec]
end

;; function that determines an optimal size for a subplot based on the
;; requested ra,dec size and the page layout
function plines_subplot_size, $
   layout=layout, $
   ra_range=ra_range, $
   dec_range=dec_range, $
   pagebox=pagebox

;; This is for calculating the position of the subfigure on the page.
;; We want the total figure to be contained within
;; pagebox=[0.1,0.1,0.95,0.95] in normalised coordinates

;; We want each sub figure to be undistorted we convert the normalised
;; total plotting area to device coordinates

  default,pagebox,[0.05,0.07,0.99,0.95]

  pagebox_device = convert_coord([pagebox[0],pagebox[2]],[pagebox[1],pagebox[3]],/normal,/to_device)

  ;; calculate the maximum length available per subplot
  subplot_max_xsize_device = abs(pagebox_device[0,1]-pagebox_device[0,0])/layout[0]
  subplot_max_ysize_device = abs(pagebox_device[1,1]-pagebox_device[1,0])/layout[1]

  ;; check inputs
  if (n_elements(ra_range) ne 2) or (n_elements(dec_range) ne 2) then begin
     message,/info,'no valid coordinate ranges supplied'
     ;; return the full box
     return,[subplot_max_xsize_device,subplot_max_ysize_device]
  endif

  ;; we determine the desired axes length of the subplot in degree
  ra_size  = (ra_range[0] -ra_range[1] )*cos(mean(dec_range)*!dpi/180.) ;; angular degree
  dec_size = (dec_range[1]-dec_range[0])                                ;; angular degree
        
  ;; take the smaller of the two value when we divide the available
  ;; length by the requested size in angular degree
  optimal_scaling_device = (subplot_max_xsize_device/ra_size) < (subplot_max_ysize_device/dec_size)

  ;; use the optimal scaling to make a best fit subplot box
  subplot_xsize_device = optimal_scaling_device*ra_size
  subplot_ysize_device = optimal_scaling_device*dec_size

  ;; convert the subplot box to normalised coordinates
  subplot_normal = convert_coord([0,subplot_xsize_device],[0,subplot_ysize_device],/device,/to_normal)

  ;; and measure its size in normalised coordinates
  subplot_xsize_normal = abs(subplot_normal[0,1]-subplot_normal[0,0])
  subplot_ysize_normal = abs(subplot_normal[1,1]-subplot_normal[1,0])

  ;; recalculate the box to be centered and against the top margin
  x_center = (pagebox[0]+pagebox[2])/2.
  pagebox = [x_center-subplot_xsize_normal*layout[0]/2.0, $
             pagebox[3]-subplot_ysize_normal*layout[1], $
             x_center+subplot_xsize_normal*layout[0]/2.0, $
             pagebox[3]]

  return,[subplot_xsize_normal,subplot_ysize_normal]
end

;; routine to make a plot of a spectrum based on the data contained in
;; sav file from PACSMAN
;; steps to do:

;;   determine peak position in map
;;   determine RA,DEC from the center of this pixel
;;   extract rebinned data from this pixel
;;   construct fitted line from this pixel
;;   plot the rebinned spectrum
;;   make second xaxis in velocity
;;   plot the fit on top
;;   annotate figure

pro plines_plotspectrum,savefilename, $
                        dirname=dirname, $
                        position=position, $
                        fitcolor=fitcolor, $
                        continuumcolor=continuumcolor, $
                        vrange=vrange, $
                        continuumfitrange=continuumfitrange, $
                        mark=mark, $
                        have_plotted=have_plotted, $
                        _extra=_extra
  
  have_plotted=0

  if n_elements(savefilename) eq 0 then begin
     if n_elements(dirname) eq 1 then begin
        savefilename=dirname+path_sep()+'cubeAcubeB.sav'
     endif
  endif
  
  files_found=file_search(savefilename,count=n_files_found)
  case n_files_found of
     0: begin
        message,/info,'The .sav file containing the PACSMAN results cannot be found'
        message,/info,'I tried: '+savefilename
        return
     end
     1:begin
        ;; this is the default behaviour
        restore,files_found
     end
     else: begin
        ;; here we need some logic to decide which file to use
        message,/info,'multiple save files found, restoring the first one: '+files_found[0]
        restore,files_found[0]
     end
  endcase

  default,fitcolor,"red"
  default,continuumcolor,"green"
  default,position,[0.1,0.1,0.95,0.95]
  default,vrange,[-800.,800] ;; kms
  ;; keep a bit of margin around the plot for the labels etc
  local_position = position+[0.10,0.05,0,-0.05]
  box_width=abs(local_position[2]-local_position[0])
  box_height=abs(local_position[3]-local_position[1])

  c = 2.99792458d8                        ;; Speed of light [SI]
  c_kms = c/1d3
  c_micron = c*1d6

  ;; calculate the pixel index of the mark (ra,dec)
  if n_elements(mark) eq 2 then begin
     mark_ra = mark[0]
     mark_dec = mark[1]
     distance = sqrt(((cube_lres.ra-mark_ra)*cos(mark_dec*!dpi/180.))^2 + (cube_lres.dec-mark_dec)^2) ;; deg
     foo = min(distance,idx_spectral_pixel)
  endif else begin ;; use the brightest pixel instead if no coord is given
     flux_brightest = max(cube_lres.flux,idx_spectral_pixel)
  endelse
  
  ra=cube_lres[idx_spectral_pixel].ra
  dec=cube_lres[idx_spectral_pixel].dec

  ;; from the spectra
  wavelength=cube_spectra[idx_spectral_pixel].lambda
  fluxdensity=cube_spectra[idx_spectral_pixel].flux
  fluxdensity_error=cube_spectra[idx_spectral_pixel].error

  ;; convert from MJY/sr to JY/pix if the version is newer then 3.5
  ;; (in fact the coding of the version went from cube_params.VERSION
  ;; to cube_params.PACMAN_VERSION
  tags_cube_params = tag_names(cube_params)
  if total(tags_cube_params eq "VERSION") ne 0 then begin
     cube_version=float(cube_params.version)
  endif else begin
     if total(tags_cube_params eq "PACSMAN_VERSION") ne 0 then begin
        cube_version=float(cube_params.pacsman_version)
     endif else begin
        cube_version=0
     endelse
  endelse
  
  if cube_version gt 3.5 then begin
     fact=1d-6/(9.4*!dpi/180d0/3600d0)^2
     fluxdensity=cube_spectra[idx_spectral_pixel].flux/fact
     fluxdensity_error=cube_spectra[idx_spectral_pixel].error/fact
  endif

  ;; cleaning
  idx_good = where(finite(wavelength) and finite(fluxdensity),cnt)
  wavelength=wavelength[idx_good]
  fluxdensity=fluxdensity[idx_good]
  fluxdensity_error=fluxdensity_error[idx_good]

  ;; and sorting
  idx_sort=sort(wavelength)
  wavelength=wavelength[idx_sort]
  fluxdensity=fluxdensity[idx_sort]
  fluxdensity_error=fluxdensity_error[idx_sort]

  min_wavelength=min(wavelength,max=max_wavelength)
  min_fluxdensity=min(fluxdensity,max=max_fluxdensity)

  ;; put the fitted line over the figure
  ;; fit parameters (watch for units!)
  fit_flux=cube_lres[idx_spectral_pixel].flux ;; W/m^2
  fit_fwhm=cube_lres[idx_spectral_pixel].fwhm ;; km/s
  fit_wavelength0=cube_lres[idx_spectral_pixel].wave ;; micron
  fit_continuum=cube_lres[idx_spectral_pixel].continuum ;; Jy

;; fix the velocity range to something nice and useful [vrange]
  fixed_xrange=[fit_wavelength0+vrange[0]/c_kms*fit_wavelength0,fit_wavelength0+vrange[1]/c_kms*fit_wavelength0]

  ;; scale the yrange to the data that will actually be shown
  idx_fixed_xrange=where(wavelength ge min(fixed_xrange) and wavelength le max(fixed_xrange),cnt)
  if cnt ne 0 then begin
     min_fluxdensity=min(fluxdensity[idx_fixed_xrange],max=max_fluxdensity)
  endif else begin
     ;; ok take all data
     min_fluxdensity=min(fluxdensity,max=max_fluxdensity)
  endelse

  pl,transpose([[wavelength],[fluxdensity],[fluxdensity_error]]), $
     /err, $
     ystyle=1, $
     yrange=[min_fluxdensity-0.05*(max_fluxdensity-min_fluxdensity),max_fluxdensity+0.05*(max_fluxdensity-min_fluxdensity)], $
     ytitle='Flux density [Jy]', $
     xstyle=9, $
     xrange=fixed_xrange, $
     xtitle=textoidl('Wavelength [\mum]'), $
     position=local_position, $
     _extra=_extra

  have_plotted=1
  
  ;; for the velocity axis on top
  ;; basic equation is as usual (l-l0)/l0 = dv/c

  ;; we want N+1 labels at [-Nx,..,0,..,Nx] that fit nicely on the
  ;; current plot
  N = 3
  full_dv_range = c_kms*(!x.crange-fit_wavelength0)/fit_wavelength0
  
  rough_dv_step = (max(full_dv_range)-min(full_dv_range))/(2.0*N)
  dv_step_order = floor(alog10(rough_dv_step))
  dv_step = floor(rough_dv_step/10d0^dv_step_order)*10d0^dv_step_order

  dv_steps=(indgen(2*N+1)-N)*dv_step
  
  dv_steps_micron=dv_steps/c_kms*fit_wavelength0+fit_wavelength0

  axis, $
     /xaxis, $
     xstyle=9, $
     xticks=n_elements(dv_steps)-1, $
     xtickv=dv_steps_micron, $
     xtickname=f2s(dv_steps,0), $
     xtitle='Velocity [kms!U-1!N]'
  
  ;; write the coordinate of the pixel on the figure
  ra_h = floor(ra/15.)
  ra_m = floor((ra/15.-ra_h)*60.)
  ra_s = round((ra/15.-ra_h-ra_m/60.)*3600.)
  IF ra_s EQ 60 THEN BEGIN
     ra_s=0
     ra_m=ra_m+1
  ENDIF 
  str_ra = n2s(ra_h)+':'+n2s(ra_m)+':'+n2s(ra_s)

  sign = dec GE 0
  dec = abs(dec)
  dec_d = floor(dec)
  dec_m = floor((dec-dec_d)*60.)
  dec_s = round((dec-dec_d-dec_m/60.)*3600.)
  IF dec_s EQ 60 THEN BEGIN
      dec_s=0
      dec_m=dec_m+1
  ENDIF 
  str_dec = (['-','+'])[sign]+n2s(dec_d)+':'+n2s(dec_m)+':'+n2s(dec_s)

  label=str_ra+' '+str_dec

  xyouts, $
     local_position[0]+0.05*box_width, $
     local_position[1]+0.90*box_height, $
     label,/normal,charsize=!p.charsize/2.5

  ;; convert fit_XXX into something we can use to construct the fitted
  ;; line profile:
  ;; gauss_sigma = fit_fwhm_micron/(2*sqrt{2*ln(2)})
  fit_fwhm_micron = fit_wavelength0*fit_fwhm/c_kms
  fit_sigma = fit_fwhm_micron/(2d0*sqrt(2d0*alog(2d0)))

  ;; fit_flux = fit_sigma*fit_height*sqrt(!dpi*2)*c_micron/(fit_wave)^2
  fit_height = 1d26*(fit_flux/fit_sigma/sqrt(!dpi*2)/c_micron*(fit_wavelength0)^2)

  ;; lets look at the residu
  residue = fluxdensity -(fit_continuum + fit_height * exp(-1d0*(((wavelength-fit_wavelength0)/fit_sigma)^2d0)/2d0))
  residue_wavelength = wavelength

  ;; use a subset of the residue to fit the continuum if
  ;; 1) continuumfitrange is given
  if n_elements(continuumfitrange) eq 2 then begin
     cfr = continuumfitrange[sort(continuumfitrange)]
     idx = where((residue_wavelength ge cfr[0]) and (residue_wavelength le cfr[1]),cnt)
     ;; 2) enough points are left after selecting
     if cnt gt 3 then begin
        residue_wavelength = residue_wavelength[idx]
        residue = residue[idx]
     endif
  endif

  residue_parameters=poly_fit(residue_wavelength-fit_wavelength0,residue,2)

  ;; nicely samples wavelengths for the fit
  fit_wavelength = dindgen(1001)/1000.*(max_wavelength-min_wavelength)+min_wavelength

  total_continuum=fit_continuum+$
                  residue_parameters[0]+ $
                  residue_parameters[1]*(fit_wavelength-fit_wavelength0)+ $
                  residue_parameters[2]*(fit_wavelength-fit_wavelength0)^2

  ;; the nicely sample curve thus becomes
  fit_fluxdensity = total_continuum+fit_height*exp(-1d0*(((fit_wavelength-fit_wavelength0)/fit_sigma)^2d0)/2d0)

  pl,fit_wavelength,fit_fluxdensity,/opl,ps=0,color=kleur(fitcolor),_extra=_extra
  pl,fit_wavelength,total_continuum,/opl,ps=0,color=kleur(continuumcolor),_extra=_extra

end

pro plines_plotmap,map_filename, $
                   ra_range=ra_range, $
                   dec_range=dec_range, $
                   limits=limits_in, $
                   scaling=scaling_in, $
                   mark=mark, $
                   linename=linename, $
                   startcolor=startcolor, $
                   ncolors=ncolors, $
                   backgroundcolor=backgroundcolor, $
                   boxcolor=boxcolor, $
                   textcolor=textcolor, $
                   position=position, $
                   postplottingprocedure=postplottingprocedure, $
                    have_plotted=have_plotted, $
                   _extra=_extra
  
  have_plotted=0

  ;; check basic input
  if n_elements(map_filename) ne 1 then return

  files_found=file_search(map_filename,count=n_files_found)
  case n_files_found of
     0: begin
        ;; simple return
        return
     end
     1:begin
        ;; this is the default behaviour
        map=readfits(files_found,map_header)
     end
     else: begin
        ;; here we need some logic to decide which file to use
        message,/info,'multiple maps (fits) files found, restoring the first one: '+files_found[0]
        map=readfits(files_found[0],map_header)
     end
  endcase
  
  if (size(map,/n_dimensions) ne 3) or (n_elements(map_header) le 1) then return
  
  map=reform(map[*,*,0])
  
  ;; some defaults
  default,limits_in,"none"
  default,scaling_in,"none"
  default,linename,""
  default,ncolors,250
  default,startcolor,5
  default,backgroundcolor,"black"
  default,boxcolor,"white"
  default,textcolor,"white"
  default,position,[0.1,0.1,0.95,0.95]
  box_width=abs(position[2]-position[0])
  box_height=abs(position[3]-position[1])

  black = kleur("black")
  white = kleur("white")
  
  case strlowcase(linename) of
     "cii": begin
        label=textoidl("[CII] 157\mum")
        beamsize=11.5 ;; arcsec
     end
     "oi63": begin
        label=textoidl("[OI] 63\mum")
        beamsize=9.5 ;; arcsec
     end
     "oi145": begin
        label=textoidl("[OI] 145\mum")
        beamsize=11.0 ;; arcsec
     end
     "oiii": begin
        label=textoidl("[OIII] 88\mum")
        beamsize=9.5 ;; arcsec
     end
     "nii": begin
        label=textoidl("[NII] 122\mum")
        beamsize=10.0 ;; arcsec
	 end
     "niii": begin
        label=textoidl("[NIII] 57\mum")
        beamsize=9.5 ;; arcsec
     end
     else: begin
        label=""
        beamsize=0. ;; arcsec
     end
  endcase
  
  idx_finite=where(finite(map))
  finite_map=map[idx_finite]
  
  ;; first apply limits to the map if requested
  limits=strlowcase(limits_in)
  case 1 of 
     limits eq "none": begin
        ;; do nothing
     end
     limits eq "positive": begin
        map=(map>0d0)
     end
     strpos(limits,"min") eq 0: begin
        value=double((strsplit(limits,/extract))[1])
        map=(map>value)
     end
     strpos(limits,"max") eq 0: begin
        value=double((strsplit(limits,/extract))[1])
        map=(map<value)
     end
     strpos(limits,"percentile") eq 0: begin
        value=double((strsplit(limits,/extract))[1])
        lower_fractile = value/1d2
        upper_fractile = 1d0-lower_fractile
        nmap=n_elements(finite_map)
        smap=finite_map(sort(finite_map))
        map=(map>smap[round(nmap*lower_fractile)])<smap[round(nmap*upper_fractile)]
     end
     strpos(limits,"positivepercentile") eq 0: begin
        value=double((strsplit(limits,/extract))[1])
        upper_fractile = 1d0-value/1d2
        nmap=n_elements(finite_map)
        smap=finite_map(sort(finite_map))
        map=(map>0d0)<smap[round(nmap*upper_fractile)]
     end
     strpos(limits,"user") eq 0: begin
        values=double((strsplit(limits,/extract))[1:2])
        map=(map>values[0])<values[1]
     end
     else: begin
        message,/info,'Unknown limits method, using all data'
     end
  endcase

  ;; next possible ways of stretching the map (not quite useful here
  scaling = strlowcase(scaling_in)
  ;; needed for all scaling methods
  maxi=max(finite_map,min=mini)
  norm_map = (map-mini)/(maxi-mini)
  
  case 1 of 
     scaling eq "none": begin
        ;; do nothing
     end
     scaling eq "log": begin
        map = alog(1d3*norm_map+1d0)/alog(1d3)
     end
     scaling eq "sqrt": begin
        map = sqrt(norm_map)
     end
     scaling eq "square": begin
        map = norm_map^2
     end
     scaling eq "equalise": begin
        map[idx_finite] = hist_equal(norm_map[idx_finite])
     end
     scaling eq "normalise": begin
        map = norm_map
     end
     strpos(scaling,"power") eq 0: begin
        value=strsplit(scaling,/extract)
        if n_elements(power) eq 2 then a=double(value[1]) else a=1d3
        map = (a^norm_map-1d0)/a
     end
     else: begin
        message,/info,'Unknown scaling method'
     end
  endcase
     
  ;; keep the annotation away from the edges
  ra_annotation_values =[1,2,3]/4.*(ra_range[1] -ra_range[0] )+ra_range[0]
  dec_annotation_values=[1,2,3]/4.*(dec_range[1]-dec_range[0])+dec_range[0]
  
  pli,map,map_header, $
      position=position, $
      xrange=ra_range, $
      yrange=dec_range, $
      xticks=n_elements(ra_annotation_values)-1, $
      yticks=n_elements(dec_annotation_values)-1, $
      xtickv=ra_annotation_values, $
      ytickv=dec_annotation_values, $
      nlevels=ncolors, $
      c_colors=indgen(ncolors)+startcolor, $
      background=kleur(backgroundcolor), $
      boxcolor=kleur(boxcolor), $
      _extra=_extra

  have_plotted=1
  
  ;; Mark a positon on the map
  if n_elements(mark) eq 2 then begin
     plines_plot_mark,mark,color=kleur(backgroundcolor)
  endif
  
  if postplottingprocedure ne "" then begin
     foo=execute(postplottingprocedure)
  endif
  
  ;; make a small 5% colorbar along the bottom of the map
  colorbar_position = position+[0.05*box_width,0.01*box_height,-0.05*box_width,-0.95*box_height]
  
  ;; the range of the map as shown for setting the correct values on
  ;; the colorbar
  min_range=min(map[idx_finite],max=max_range)
  
  plbar, $
     range=[min_range,max_range], $
     position=colorbar_position, $
     indexrange=[startcolor,startcolor+ncolors-1], $
     /top, $
     boxcolor=kleur(boxcolor), $
     smartlabels=3
  
  ;; add the line label to the top left of the figure
  xyouts, $
     position[0]+0.05*box_width, $
     position[1]+0.90*box_height, $
     label, $
     color=kleur(textcolor),/normal
  
  ;; show the beam size on the top right
  xcirc = beamsize/2./3600.*cos(2*!dpi*dindgen(101)/100.) ;; beam coordinates in degree
  ycirc = beamsize/2./3600.*sin(2*!dpi*dindgen(101)/100.)
  
  ;; calculate the device pixels per degree
  tmp=convert_coord([0.,0.],[0.,box_height],/normal,/to_device)

  device_pixels_per_degree = abs((tmp[1,1]-tmp[0,0])/(dec_range[1]-dec_range[0]))

  xcirc_device = xcirc*device_pixels_per_degree
  ycirc_device = ycirc*device_pixels_per_degree
  
  ;; determine the center of where we want the beam in device coordinates
  beam_center_device = convert_coord( $
                       position[0]+box_width*0.85, $
                       position[1]+box_height*0.85, $
                       /normal,/to_device)
  
  plots,/device, $
        xcirc_device+beam_center_device[0], $
        ycirc_device+beam_center_device[1],color=kleur(textcolor)

  polyfill,/device, $
           xcirc_device+beam_center_device[0], $
           ycirc_device+beam_center_device[1], $
           /line_fill, $
           color=kleur(textcolor), $
           spacing=0.1, $
           orient=40, $
           _extra=_extra
  
end

;; routine that draws one map and one spectrum on the page. Very
;; specific for plines
pro plines_plotduo,map_filename,sav_filename, $
                   layout=layout, $
                   panelindex=panelindex, $
                   subplot_size=subplot_size, $
                   pagebox=pagebox, $
                   have_plotted=have_plotted, $
                   show_max_mark=show_max_mark, $
                   _extra=_extra
  
  have_plotted=0

  ;; some defaults
  default,layout,[1,1]
  default,panelindex,0
  default,pagebox,[0.05,0.07,0.99,0.95]
  default,show_max_mark,1
  
  ;; where the map plot should go based on the panelindex value.
  panelindex_map = panelindex
  x_index_map = panelindex_map mod layout[0]
  y_index_map = panelindex_map  /  layout[0]
  ;; this is to make the y start at the top
  y_index_map = layout[1] - y_index_map -1
  
  position_map = [ $
                 pagebox[0]+subplot_size[0]*x_index_map, $
                 pagebox[1]+subplot_size[1]*y_index_map, $
                 pagebox[0]+subplot_size[0]*(x_index_map+1), $
                 pagebox[1]+subplot_size[1]*(y_index_map+1) $
                 ]
  

  plines_plotmap, map_filename, $
                  ra_range=ra_range, $
                  dec_range=dec_range, $
                  position=position_map, $
                  _extra=_extra, $
                  have_plotted=have_plotted_map

  panelindex_spectrum = panelindex+1
  x_index_spectrum = panelindex_spectrum mod layout[0]
  y_index_spectrum = panelindex_spectrum  /  layout[0]
  ;; this is to make the y start at the top
  y_index_spectrum = layout[1] - y_index_spectrum -1
  
  position_spectrum = [ $
                 pagebox[0]+subplot_size[0]*x_index_spectrum, $
                 pagebox[1]+subplot_size[1]*y_index_spectrum, $
                 pagebox[0]+subplot_size[0]*(x_index_spectrum+1), $
                 pagebox[1]+subplot_size[1]*(y_index_spectrum+1) $
                 ]

  plines_plotspectrum,sav_filename, $
                      position=position_spectrum, $
                      _extra=_extra, $
                      have_plotted=have_plotted_spectrum
 
  have_plotted=(have_plotted_map ne 0) or (have_plotted_spectrum ne 0)

end

pro plines_help
  doc_library,'plines'
end

;+
; NAME: plines
;
; PURPOSE: Make multi-panel plots of (line) maps and spectra with
; matching ra,dec coordinates in a relatively systematic way
;
; CATEGORY: plotting
;
; CALLING SEQUENCE: plines,sourenames
;
; INPUTS:
;  sourcenames: STRING (array) of the sources to make plots for
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
;   parfiles: STRING (array) of the parameter files for each source
;     if a single value is given for an array of sources the value is
;     used as a prefix, e.g. parfiles="plines_parameters_"
;
;   fignames: STRING (array) of the figure names to create for each source
;     if a single value is given for an array of sources the value is
;     used as a prefix on the output figure, e.g., figname="plot_v3_"
;
;   outdir: STRING directory where the output figures will be stored
;   default './figures'
;
;   pardir: STRING directory where the parameters files for each
;   source are stored
;   default './plines_parameters'
;
;   colortable: INT which colortable to use for the plots, default 39
;
;   startcolor: INT lowest color index to use for the figure (5)
;
;   ncolors: INT number of colors to use in the plot (250)
;
;   pagebox: FLOAT(4) indicated the maximum position (in normal coordinates)
;            where all the subpanels have to fit within ([0.07,0.03,0.99,0.95])
;
;   plotorder: STRING (array) which control the sequence of the line plots
;              default ["cii","oiii","oi63","oi145","nii"]
;
;   lines: STRING (ARRAY) of which lines to plot, default ["cii","oi145","oi63","oiii","nii"]
;
;   /fixpositions: if set each line will be put at the same place on
;   each figure, default no
;
;   layout: INT [N,M] number of columns and row on the page, default [2,3]
;
;   limits: STRING which controls the ranges of the pixel values, accepted forms are
;     limits="none"
;     limits="positive"
;     limits="percentile F" (default percentile 1.0)
;     limits="positivepercentile F"
;     limits="user F F"
;     limits="min F"
;     limits="max F"
;
;   scaling: STRING which controls the streching of the colors
;     scaling="none" default
;     scaling="equalise"
;     scaling="log"
;     scaling="normalise"
;     scaling="sqrt"
;     scaling="square"
;     scaling="power F" F=1000 if omitted
;
;   backgroundcolor: STRING color to put behind the map ["black"]
;   boxcolor: STRING color to draw the box around the map ["white"]
;   textcolor: STRING color to annotate the map ["white"]
;   fitcolor: STRING color to use for plotting the fitted line on the spectrum ["red"]
;   continuumcolor: STRING color to use for plotting the continuum on the spectrum ["green"]
;
;   /title: If set put the source name over the top of the figure
;
;   /help: show this help
;
; SPECIAL NOTE:
;  These global defaults can be overwritten in detail in each
;  parameter files if it exists. For example to show the full range of
;  the CII map of IIZw40 one would add: cii_limits="none" in its parameter-file.
;
;;;; an example parameter file could be the following without the first comment
;;;; symbol.
;; This is a comment
;cii="mycii.fits"
;cii_limits="percentile 5"
;nii_limits="user 0 100"
;cii_scaling="log"
;oi145_limits="none"
;lines=["cii","oi145"]
;
; OUTPUTS: eps figures in the output directory
;
; OPTIONAL OUTPUTS: pdf figures in the output directory
;
; MODIFICATION HISTORY:
; (SH Jul 22 2012) Initial framework and idea
;
;-
;; *** plines.pro ***

; accepted keywords:
; parfile
; figname
; outdir
; colortable
; which lines to show
; plotting order for lines
; fixpositions for lines even if some are not plotted
; layout (2x3)
; xsize,ysize
;-

pro plines,sourcenames, $
           parfiles=parfiles, $
           fignames=fignames, $
           outdir=outdir, $
           pardir=pardir, $
           colortable=colortable, $
           plotorder=plotorder, $
           lines=lines, $
           fixpositions=fixpositions, $
           layout=layout, $
           limits=limits, $
           scaling=scaling, $
           title=title, $
           pagebox=pagebox, $
           startcolor=startcolor, $
           ncolors=ncolors, $
           backgroundcolor=backgroundcolor, $
           boxcolor=boxcolor, $
           continuumcolor=continuumcolor, $
           fitcolor=fitcolor, $
           textcolor=textcolor, $
           vrange=vrange, $
           show_max_position=show_max_position, $
           pdf=pdf, $
           help=help, $
           postpro=postpro, $
           _extra=_extra

  default,outdir,'./figures'
  default,pardir,'./plines_parameters'
  default,colortable,39
  default,plotorder,["cii","oiii","oi63","oi145","nii","niii"]
  default,lines,["cii","oiii","oi63","oi145","nii","niii"]
  default,fixpositions,0
  default,layout,[2,3]
  default,limits,"percentile 1.0"
  default,scaling,"none"
  default,pagebox,[0.1,0.1,0.99,0.95]
  default,startcolor,5
  default,ncolors,250
  default,vrange,[-800.,800.]
  default,show_max_position,1
  default,title,1
  default,pdf,1
  default,postpro,""

  defsysv,"!plines_sourcename",""

  title_x_position = (pagebox[0]+pagebox[2])/2.
  title_y_position = pagebox[3]+0.01

  default,backgroundcolor,"black"
  default,boxcolor,"white"
  default,continuumcolor,"green"
  default,fitcolor,"red"
  default,textcolor,"white"

  if keyword_set(help) then begin
     plines_help
     return
  endif
  
  nsources = n_elements(sourcenames)
  if nsources eq 0 then begin
     message,/info,'no sourcenames defined'
     plines_help
     return
  endif
  
  if n_elements(fignames) ne nsources then begin
     if n_elements(fignames) eq 1 then begin
        ;; allow a prefix to be passed as the figname argument
       global_fignames=fignames[0]+sourcenames
     endif else begin
        global_fignames=sourcenames
     endelse 
  endif else begin
     global_fignames=fignames
  endelse
     
  if n_elements(parfiles) ne nsources then begin
     if n_elements(parfiles) eq 1 then begin
        ;; allow a prefix to be passed as the parfile argument
        global_parfiles=pardir+path_sep()+parfiles[0]+sourcenames+'.txt'
     endif else begin
        global_parfiles=pardir+path_sep()+''+sourcenames+'.txt'
     endelse 
  endif else begin
     global_parfiles=parfiles
  endelse

  ;; we define global values that can be overwritten in the individual
  ;; param files
  global_outdir = outdir
  global_colortable = colortable
  ;; downcase for easier comparison
  global_plotorder = strlowcase(plotorder)
  global_lines = strlowcase(lines)
  global_fixpositions = fixpositions
  global_layout = layout
  global_limits = limits
  global_scaling = scaling
  if (show_max_position ne 0) then global_mark="max" else global_mark=""
  global_backgroundcolor=backgroundcolor
  global_boxcolor=boxcolor
  global_continuumcolor=continuumcolor
  global_fitcolor=fitcolor
  global_textcolor=textcolor
  global_vrange=vrange
  global_postpro=postpro

  for isource=0,nsources-1 do begin

     ;; restore the global defaults
     outdir = global_outdir
     colortable = global_colortable
     plotorder = global_plotorder
     lines = global_lines
     fixpositions = global_fixpositions
     layout = global_layout

     limits = global_limits
     scaling = global_scaling

     ;; undefine
     cii_limits   = ""
     nii_limits   = ""
     oi145_limits = ""
     oi63_limits  = ""
     oiii_limits  = ""
     niii_limits  = ""

     cii_scaling   = ""
     nii_scaling   = ""
     oi145_scaling = ""
     oi63_scaling  = ""
     oiii_scaling  = ""
     niii_scaling  = ""

     mark       = global_mark
     cii_mark   = global_mark
     nii_mark   = global_mark
     oi145_mark = global_mark
     oi63_mark  = global_mark
     oiii_mark  = global_mark
     niii_mark  = global_mark
     
     vrange       = global_vrange
     cii_vrange   = global_vrange
     nii_vrange   = global_vrange
     oi145_vrange = global_vrange
     oi63_vrange  = global_vrange
     oiii_vrange  = global_vrange
     niii_vrange  = global_vrange
     
     postpro       = global_postpro
     cii_postpro   = global_postpro
     nii_postpro   = global_postpro
     oi145_postpro = global_postpro
     oi63_postpro  = global_postpro
     oiii_postpro  = global_postpro
     niii_postpro  = global_postpro
     
     ;; allow ranges to be set in parameter file
     ra_range = 0
     dec_range = 0

     sourcename=sourcenames[isource]
     !plines_sourcename=sourcename

     figname=global_fignames[isource]
     parfile=global_parfiles[isource]
     
     ;;default naming scheme
     cii_map_filename   = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'CII157mic'+path_sep()+sourcename+'_LINECII157*_Flux.fits'
     nii_map_filename   = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'NII122mic'+path_sep()+sourcename+'_LINENII122*_Flux.fits'
     oi145_map_filename = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'OI145mic' +path_sep()+sourcename+'_LINEOI145*_Flux.fits'
     oi63_map_filename  = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'OI63mic'  +path_sep()+sourcename+'_LINEOI63*_Flux.fits'
     oiii_map_filename  = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'OIII88mic'+path_sep()+sourcename+'_LINEOIII88*_Flux.fits'
     niii_map_filename  = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'NIII57mic'+path_sep()+sourcename+'_LINENIII57*_Flux.fits'

     cii_sav_filename   = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'CII157mic'+path_sep()+'cube*.sav' ;cubeAcubeB.sav'
     nii_sav_filename   = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'NII122mic'+path_sep()+'cube*.sav'
     oi145_sav_filename = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'OI145mic' +path_sep()+'cube*.sav'
     oi63_sav_filename  = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'OI63mic'  +path_sep()+'cube*.sav'
     oiii_sav_filename  = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'OIII88mic'+path_sep()+'cube*.sav'
     niii_sav_filename  = sourcename+path_sep()+'PACS_spec'+path_sep()+'fits'+path_sep()+'NIII57mic'+path_sep()+'cube*.sav'

     backgroundcolor=global_backgroundcolor
     boxcolor=global_boxcolor
     continuumcolor=global_continuumcolor
     fitcolor=global_fitcolor
     textcolor=global_textcolor

     ;; by default use all data to fit the continuum of each line
     cii_continuumfitrange   = 0
     nii_continuumfitrange   = 0
     oi145_continuumfitrange = 0
     oi63_continuumfitrange  = 0
     oiii_continuumfitrange  = 0
     niii_continuumfitrange  = 0

     files_found=file_search(parfile,count=n_files_found)
     case n_files_found of
        0: begin
           ;; do nothing
           parlist=""
        end
        1:begin
           ;; this is the default behaviour
           parlist = strsplit(string(read_binary(files_found)),10B,/extract)
        end
        else: begin
           ;; here we need some logic to decide which file to use
           message,/info,'multiple parameter files, using the first one: '+files_found[0]
           parlist = strsplit(string(read_binary(files_found[0])),10B,/extract)
        end
     endcase

     ;; execute each line in the partlist
     for i=0,n_elements(parlist)-1 do begin
        foo = execute(parlist[i])
     endfor
     
     ;; read the fits files
     for i=0,n_elements(lines)-1 do begin
        line=lines[i]
        foo=execute("fitsname="+line+"_map_filename")

        files_found=file_search(fitsname,count=n_files_found)
        case n_files_found of
           0: begin
              ;; dummy values
              foo=execute(line+'_map=0')
              foo=execute(line+'_header=0')
           end
           1:begin
              ;; this is the default behaviour
              foo=execute(line+'_map=reform((readfits(files_found,'+line+'_header))[*,*,0])')
           end
           else: begin
              ;; here we need some logic to decide which file to use
              message,/info,'multiple maps (fits) files found, restoring the first one: '+files_found[0]
              foo=execute("dummy=reform((readfits(files_found[0],"+line+"_header))[*,*,0])")
           end
        endcase
     endfor
     
     ;; determine automatically the required ranges if not given
     if (ra_range[0] eq 0) and (dec_range[0] eq 0) then begin
        ;; more extreme than any real value therefore these will be
        ;; replaced in the first interation by real values
        min_ra=400d0
        max_ra=-400d0
        min_dec=900d0
        max_dec=-900d0
        for i=0,n_elements(lines)-1 do begin
           line=lines[i]
           ;;ra0,dec0,ra1,rec1
           foo=execute("box=plines_header_coord_range("+line+"_header)")
           min_ra  = (min_ra  < box[0])
           max_ra  = (max_ra  > box[2])
           min_dec = (min_dec < box[1])
           max_dec = (max_dec > box[3])
        endfor
        ;; inversed for astro convention
        ra_range=[max_ra,min_ra]
        dec_range=[min_dec,max_dec]
     endif
     
     ;; define if not defined in par file the funny logic allows for
     ;; values that are not overruled to for a given line to still be
     ;; overruled for the whole galaxy
     if cii_limits    eq "" then cii_limits    = limits
     if nii_limits    eq "" then nii_limits    = limits
     if oi145_limits  eq "" then oi145_limits  = limits
     if oi63_limits   eq "" then oi63_limits   = limits
     if oiii_limits   eq "" then oiii_limits   = limits
     if niii_limits   eq "" then niii_limits   = limits

     if cii_scaling   eq "" then cii_scaling   = scaling
     if nii_scaling   eq "" then nii_scaling   = scaling
     if oi145_scaling eq "" then oi145_scaling = scaling
     if oi63_scaling  eq "" then oi63_scaling  = scaling
     if oiii_scaling  eq "" then oiii_scaling  = scaling
     if niii_scaling  eq "" then niii_scaling  = scaling

     if array_equal(cii_mark   ,global_mark) then cii_mark   = mark
     if array_equal(nii_mark   ,global_mark) then nii_mark   = mark
     if array_equal(oi145_mark ,global_mark) then oi145_mark = mark
     if array_equal(oi63_mark  ,global_mark) then oi63_mark  = mark
     if array_equal(oiii_mark  ,global_mark) then oiii_mark  = mark
     if array_equal(niii_mark  ,global_mark) then niii_mark  = mark

     if cii_postpro   eq global_postpro then cii_postpro   = postpro
     if nii_postpro   eq global_postpro then nii_postpro   = postpro
     if oi145_postpro eq global_postpro then oi145_postpro = postpro
     if oi63_postpro  eq global_postpro then oi63_postpro  = postpro
     if oiii_postpro  eq global_postpro then oiii_postpro  = postpro
     if niii_postpro  eq global_postpro then niii_postpro  = postpro

     if array_equal(cii_vrange   ,global_vrange) then cii_vrange   = vrange
     if array_equal(nii_vrange   ,global_vrange) then nii_vrange   = vrange
     if array_equal(oi145_vrange ,global_vrange) then oi145_vrange = vrange
     if array_equal(oi63_vrange  ,global_vrange) then oi63_vrange  = vrange
     if array_equal(oiii_vrange  ,global_vrange) then oiii_vrange  = vrange
     if array_equal(niii_vrange  ,global_vrange) then niii_vrange  = vrange

     ;; create the output directory
     file_mkdir,outdir

     ;; plot each line-map
     figureindex=1
     Npanels=layout[0]*layout[1]

     ;; start the first plot
     sh_ps,outdir+path_sep()+figname+'_1.eps', $
           /a4port,thick=3,charsize=2.5, $
           bits_per_pixel=8,/color,loadtable=colortable, $
           _extra=_extra

     !p.charsize=2.5
     !p.multi=[0,layout[0],layout[1]]
     panelsleftonpage=Npanels
     subplot_size=plines_subplot_size(layout=layout,ra_range=ra_range,dec_range=dec_range,pagebox=pagebox)
     
     for i=0,n_elements(plotorder)-1 do begin
        
        ;; determine if we need a new figure either because there are
        ;; less then 2 subplots left on the page
        if panelsleftonpage lt 2 then begin
           
           ;; annotations of the current full figure (title) xtitle,ytitle
           if title ne 0 then xyouts,title_x_position,title_y_position,sourcename, $
                                     charsize=2.5,align=0.5,/normal
           ;; close the current figure
           sh_ps,/fixbb,pdf=pdf,_extra=_extra
           
           ;; open a new figure
           figureindex+=1
           sh_ps,outdir+path_sep()+figname+'_'+n2s(figureindex)+'.eps', $
                 /a4port,thick=3,charsize=2.5, $
                 bits_per_pixel=8,/color,loadtable=colortable, $
                 _extra=_extra
           !p.multi=[0,layout[0],layout[1]]
           panelsleftonpage=Npanels
        endif
        
        this_line=plotorder[i]
        
        ;; is this line in the requested lines list?
        if where(this_line eq lines) ne -1 then begin
           foo=execute("this_map_filename="+this_line+"_map_filename")
           foo=execute("this_sav_filename="+this_line+"_sav_filename")
           foo=execute("this_limits="+this_line+"_limits")
           foo=execute("this_scaling="+this_line+"_scaling")
           foo=execute("this_mark="+this_line+"_mark")
           foo=execute("this_vrange="+this_line+"_vrange")
           foo=execute("this_postpro="+this_line+"_postpro")
           foo=execute("this_continuumfitrange="+this_line+"_continuumfitrange")
           this_linename=this_line

           if this_mark[0] eq "max" then this_mark=plines_radec_maxraster(this_sav_filename)

           plines_plotduo, $
              this_map_filename, $
              this_sav_filename, $
              ra_range=ra_range, $
              dec_range=dec_range, $
              limits=this_limits, $
              scaling=this_scaling, $
              mark=this_mark, $
              layout=layout, $
              panelindex=Npanels-panelsleftonpage, $
              linename=this_linename, $
              subplot_size=subplot_size, $
              pagebox=pagebox, $
              backgroundcolor=backgroundcolor, $
              boxcolor=boxcolor, $
              fitcolor=fitcolor, $
              textcolor=textcolor, $
              continuumcolor=continuumcolor, $
              vrange=this_vrange, $
              have_plotted=have_plotted, $
              postplottingprocedure=this_postpro, $
              continuumfitrange=this_continuumfitrange, $
              _extra=_extra
        endif
        
        ;; see of we should reduce the number of available panels
        if (have_plotted eq 1) or (fixpositions ne 0) then panelsleftonpage -= 2
        
     endfor

     ;; annotations of the current full figure (title) xtitle,ytitle
     if title ne 0 then xyouts,title_x_position,title_y_position,sourcename, $
                               charsize=2.5,align=0.5,/normal
     ;; close the current figure before moving to the next figure
     sh_ps,/fixbb,pdf=pdf,_extra=_extra
  
  endfor
  
end
