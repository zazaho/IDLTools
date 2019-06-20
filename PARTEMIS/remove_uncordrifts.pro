;+
; NAME:
;	REMOVE_UNCORDRIFTS
;
; PURPOSE:
;
;	Remove uncorrelated 1/f noise from data.
;	
; CALLING SEQUENCE:
;
;	REMOVE_UNCORDRIFTS, Visu=Visu, Dir_in=Dir_in, Num_scans=Num_scans, Nsubscans=Nsubscans, Dir_rcp=Dir_rcp
;
; INPUTS:
;
;	Visu:		Visualization mode.
;	Dir_in:		Data path.
;	Num_scans:	List of scans.
;	Nsubscans:	Maximum number of subscans. 
;	Dir_rcp:	Distorsion files path.	
;
; OPTIONAL INPUTS:
;
;	Pfov:	Pixel field of view.
;		
; EXAMPLE:
;
;		REMOVE_UNCORDRIFTS, Visu=1, Dir_in=Dir_in, Num_scans=[47527,47528], Nsubscans=150, Pfov=5.2, dir_rcp=dir_rcp
;
; MODIFICATION HISTORY:
; 	
;-

pro remove_uncordrifts, visu=visu, dir_in=dir_in, num_scans=num_scans, nsubscans=nsubscans, $
                        pfov=pfov, dir_rcp=dir_rcp


;; visu : set to 1 to visualize intermediate results
;; dir_in : directory where all data structures are stored
;; num_scans : array containing the scan numbers
;; nsubscans : maximum number of subscans per scan
;; pfov : average size of a matrix pixel in arcseconds
;; dir_rcp : directory where distortion files are stored


;; note: The mask goodpix_ima used by this algorithm is the intersection of the
;;;      goodpix_imas of all the input scans.

;; important: Any misalignment between successive scans has to be corrected beforehand.



;; N3576 :
;; dir_in='/poubelles/broque1NS/tmp/roussel/PArtemis/n3576/map_otf_xdr/E-080.C-0722A-2007/'
;; dir_rcp='/poubelles/broque1NS/tmp/roussel/PArtemis/calib/'
;; N3576 center, mars 2007: num_scans=[4825, 4828], nsubscans=76, pfov=3.75
;; N3576 center, nov. 2007: num_scans=[47527, 47536], nsubscans=120, pfov=5.85
;; N3576 Smost:             num_scans=[49000, 49281], nsubscans=121, pfov=6.2
;; N3576 Smid:              num_scans=[49281, 48822], nsubscans=121, pfov=6.2



;;; common blocks:
common arraydef, half_bolo, npix_side, coord_ref, mask
common scanquant, detarr, signals, x_bolos, y_bolos, nbolo
common timekeeping, time_s, timestep, maxtimediff, nt, nobs, ind_scans, ind_subscans
common psfuse, psfs, dim_psf, xcmin_psf, nstep_psf
common mapuse, map_signal, map_weight, dimx, dimy, dimx_iter, dimy_iter, fact
common finegridproj, wproj_fine, revind_fine, nproj_fine, vectpos_startpsf, vectpos_psf
common algoquant, maxnoise, nt_cross, ntmin_pass, ind_aver
common coarsegridproj, wproj, revind, nproj
common driftmatrices, array_drifts, array_weight, array_npass

resolve_all, /continue_on_error, /quiet


;;;;;;;;;;;;;;;;
;;; definitions
;;;;;;;;;;;;;;;;


npix_side = 16
coord_ref = 7
boloref = coord_ref * npix_side + coord_ref

if not keyword_set(pfov) then read, pfov, prompt = crs + 'array pixel size in arcsec: '
pfov = double(pfov)

fwhm_as = [4.2D0, 9.4D0]
pix_as = fwhm_as / 4.
fwhm_as_proj = dblarr(2) + 2. * sqrt(pfov^2. / !dpi)

iter_max = 3
rmax_beam = 2.D0
nstep_psf = 10

;;; drift minimum timescale changed by the code if too small:
tc_drift_s = 0.4D0

arrays_default = ['P200', 'P450']
;arrays_default = ['P450']
orient_default = 'astro'

crs = string(13b) + string(10b)


nscans = n_elements(num_scans)
files_scans = strarr(nscans * nsubscans)
files_out = strarr(nscans * nsubscans)
for k = 0, nscans - 1 do begin
   files_scans(k*nsubscans:(k+1)*nsubscans-1) = $
      'otf_subscan_' + chain(num_scans(k)) + '_' + chain(indgen(nsubscans)+1) + '.xdr'
   files_out(k*nsubscans:(k+1)*nsubscans-1) = $
      'otf_subscan_nodrifts_' + chain(num_scans(k)) + '_' + chain(indgen(nsubscans)+1) + '.xdr'
endfor

print, crs + chain(nscans) + ' observation(s) to be combined'



;arrays = ''
arrays = 'P450'
;read, arrays, prompt = crs + 'detector arrays:' $
;   + crs + '   default = ' + strjoin(arrays_default, ', ') + ' [] ;' $
;   + crs + '      choice = '
if not keyword_set(arrays) then arrays = arrays_default $
   else arrays = find_all_regex(strupcase(arrays), 'P(200|450)')
ind_arrays = intarr(2)
;for detarr = 0, n_elements(arrays_default)-1 do begin
for detarr = 0, 1 do begin
   w = where(arrays eq arrays_default(detarr), cw)
   if cw ge 1 then ind_arrays(detarr) = 1
endfor

;orient = ''
orient = 'astro'
;read, orient, prompt = crs + 'map orientation:' + crs + $
;   'either along the scan (direction at t=0)' + crs $
;   + ' or in the standard astronomical frame (N up, E left)' $
;   + crs + '   default = ' + orient_default + ' [] ;' $
;   + crs + '      choice = [ /scan/astro] '
if not keyword_set(orient) then orient = orient_default $
   else if strlowcase(strmid(orient, 0, 1)) eq 's' then orient = 'scan' $
   else orient = 'astro'
print, ''



;;;;;;;;;;;;;;;;;
;;; data readout
;;;;;;;;;;;;;;;;;


i = long(-1)
repeat begin
   i += 1
   test = file_test(dir_in + files_scans(i), /read)
endrep until test gt 0
restore, dir_in + files_scans(i)
subs_next = i + 1
subscan_to_file = [i]

time_s = donnees_red.datapar.lst
nt = n_elements(time_s)

nbolo = lonarr(2) + n_elements(donnees_red.cube(*,*,0))
signals = ptrarr(2, /allocate_heap)
for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then $
   *signals(detarr) = transpose(reform(double(donnees_red.cube), nbolo(detarr), nt))

mask = donnees_red.goodpix_ima


;;; coordinate vectors of array center in degrees (Xra = -RA, Ydec = DEC) :
Ydec = donnees_red.datapar.baslat
ra_ref = median(donnees_red.datapar.baslong)
Xra = -(donnees_red.datapar.baslong - ra_ref) * cos(Ydec * !dpi / 180.)


;;; scan rotation angle with respect to Xra axis:
scanrot_deg =  median(donnees_red.rota - donnees_red.datapar.elevatio)
cos_scanrot = cos(scanrot_deg * !dpi / 180.)
sin_scanrot = sin(scanrot_deg * !dpi / 180.)

;;; array orientation in (N, E) frame varies with time:
pa_array = donnees_red.rota
cos_pa_array = cos(pa_array * !dpi / 180.)
sin_pa_array = sin(pa_array * !dpi / 180.)



;;; Xra and Ydec offset vectors of bolometers with respect to array center in arcseconds:

if abs(pfov - 3.75) le 0.1 then begin
   test = file_test(dir_rcp + 'rcp_jupiter_4792.xdr', /read)
   if test gt 0 then begin
      restore, dir_rcp + 'rcp_jupiter_4792.xdr'
      dx_as_matrix = reform(dx_jupiter_best, npix_side^2)
      dy_as_matrix = reform(dy_jupiter_best, npix_side^2)
   endif
endif else begin
   if abs(pfov - 5.85) le 0.1 then begin
      test = file_test(dir_rcp + 'rcp_mars_47525.xdr', /read)
      if test gt 0 then restore, dir_rcp + 'rcp_mars_47525.xdr'
   endif
   if abs(pfov - 6.2) le 0.1 then begin
      test = file_test(dir_rcp + 'rcp_mars_50010.xdr', /read)
      if test gt 0 then restore, dir_rcp + 'rcp_mars_50010.xdr'
   endif
   if keyword_set(dx_mars_best) then begin
      dx_as_matrix = reform(dx_mars_best, npix_side^2)
      dy_as_matrix = reform(dy_mars_best, npix_side^2)
   endif
endelse

if not keyword_set(dx_as_matrix) then begin
   dx_as_matrix = rebin(dindgen(npix_side), npix_side, npix_side) * pfov
   dy_as_matrix = dindgen(npix_side) ## (dblarr(npix_side) + 1.) * pfov
   dx_as_matrix = reform(dx_as_matrix, npix_side^2)
   dy_as_matrix = reform(dy_as_matrix, npix_side^2)
   dx_as_matrix = dx_as_matrix - dx_as_matrix(boloref)
   dy_as_matrix = dy_as_matrix - dy_as_matrix(boloref)
endif



dXra = ptrarr(2, /allocate_heap)
dYdec = ptrarr(2, /allocate_heap)
for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
   dXra_temp = dx_as_matrix ## (dblarr(nt) + 1.)
   dYdec_temp = dy_as_matrix ## (dblarr(nt) + 1.)
   cos_pa_array = rebin(temporary(cos_pa_array), nt, nbolo(detarr))
   sin_pa_array = rebin(temporary(sin_pa_array), nt, nbolo(detarr))
   *dXra(detarr) = dXra_temp * cos_pa_array - dYdec_temp * sin_pa_array
   *dYdec(detarr) = dYdec_temp * cos_pa_array + dXra_temp * sin_pa_array
endif


;;; bolometer coordinate arrays in degrees:
Xra_bolos = ptrarr(2, /allocate_heap)
Ydec_bolos = ptrarr(2, /allocate_heap)
for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
   *Ydec_bolos(detarr) = rebin(Ydec, nt, nbolo(detarr)) + *dYdec(detarr) / 3600.
   *Xra_bolos(detarr) = rebin(Xra, nt, nbolo(detarr)) + *dXra(detarr) / 3600.
endif


;;; read all other subscans:
ind_scans = long([0])
ind_subscans = long([0, nt])
nobs = nscans * nsubscans
nobs_reject = subs_next - 1
mask_save_subscan = intarr(nobs)
mask_save_subscan(subs_next-1:*) += 1
for i = subs_next, nobs - 1 do begin
   test = file_test(dir_in + files_scans(i), /read)
   if test eq 0 then begin
      print, 'input file ' + files_scans(i) + ' inexistent or unreadable'
      mask_save_subscan(i) = 0
      if i mod nsubscans eq nsubscans - 1 then ind_scans = [ind_scans, nt]
      nobs_reject += 1
   endif else begin
      restore, dir_in + files_scans(i)
      carte_scan = 0
      donnees_uncal = 0
      donnees = 0
      time_b = donnees_red.datapar.lst
      nt_b = n_elements(time_b)
      if nt_b le 10 then begin
         mask_save_subscan(i) = 0
         if i mod nsubscans eq nsubscans - 1 then ind_scans = [ind_scans, nt]
         nobs_reject += 1
         print, 'input file ' + files_scans(i) + ' too few data: discarded'
      endif else begin
         subscan_to_file = [subscan_to_file, i]
         mask *= donnees_red.goodpix_ima
         if time_b(0) le max(time_s) then time_b += max(time_s) - 1.1 * time_b(0) + 0.1 * max(time_b)
         time_s = [time_s, time_b]
         nt = nt + nt_b
         ind_subscans = [ind_subscans, nt]
         if i mod nsubscans eq nsubscans - 1 then ind_scans = [ind_scans, nt]
         Ydec = donnees_red.datapar.baslat
         Xra = -(donnees_red.datapar.baslong - ra_ref) * cos(Ydec * !dpi / 180.)
         pa_array_b = donnees_red.rota(0:nt_b-1)
         cos_pa_array = cos(pa_array_b * !dpi / 180.)
         sin_pa_array = sin(pa_array_b * !dpi / 180.)
         for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
            *signals(detarr) = [*signals(detarr), $
               transpose(reform(double(donnees_red.cube(*, *, 0:nt_b-1)), nbolo(detarr), nt_b))]
            dXra_temp = dx_as_matrix ## (dblarr(nt_b) + 1.)
            dYdec_temp = dy_as_matrix ## (dblarr(nt_b) + 1.)
            cos_pa_array = rebin(temporary(cos_pa_array), nt_b, nbolo(detarr))
            sin_pa_array = rebin(temporary(sin_pa_array), nt_b, nbolo(detarr))
            *dXra(detarr) = dXra_temp * cos_pa_array - dYdec_temp * sin_pa_array
            *dYdec(detarr) = dYdec_temp * cos_pa_array + dXra_temp * sin_pa_array
            *Ydec_bolos(detarr) = [*Ydec_bolos(detarr), rebin(Ydec, nt_b, nbolo(detarr)) $
               + *dYdec(detarr) / 3600.]
            *Xra_bolos(detarr) = [*Xra_bolos(detarr), rebin(Xra, nt_b, nbolo(detarr)) $
               + *dXra(detarr) / 3600.]
         endif
      endelse
   endelse
endfor

nobs -= nobs_reject

;print, "nobs = ", nobs

ptr_free, dXra, dYdec
donnees_red = 0
Xra = 0
Ydec = 0
dXra_temp = 0
dYdec_temp = 0
cos_pa_array = 0
sin_pa_array = 0
dx_as_matrix = 0
dy_as_matrix = 0
time_b = 0



;;;;;;;;;;;;;;;;;;;;;
;;; drift correction
;;;;;;;;;;;;;;;;;;;;;


;;; map and time grids:

crval1 = -min(*Xra_bolos(1) / cos(*Ydec_bolos(1) * !dpi / 180.)) + ra_ref
crval2 = min(*Ydec_bolos(1))

diff_t = time_s(1:*) - time_s(0:nt-2)
diff_t = diff_t(sort(diff_t))
timestep = median(diff_t(10:nt-12))
diff_t = 0

xas_bolos = ptrarr(2, /allocate_heap)
yas_bolos = ptrarr(2, /allocate_heap)
case orient of
   'scan': for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
      *xas_bolos(detarr) = *Xra_bolos(detarr) * cos_scanrot + *Ydec_bolos(detarr) * sin_scanrot
      *yas_bolos(detarr) = *Ydec_bolos(detarr) * cos_scanrot - *Xra_bolos(detarr) * sin_scanrot
      endif
   'astro': for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
      *xas_bolos(detarr) = *Xra_bolos(detarr)
      *yas_bolos(detarr) = *Ydec_bolos(detarr)
      endif
endcase
ptr_free, Xra_bolos, Ydec_bolos

for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
   *xas_bolos(detarr) -= min(*xas_bolos(detarr))
   *yas_bolos(detarr) -= min(*yas_bolos(detarr))
   *xas_bolos(detarr) *= 3600.
   *yas_bolos(detarr) *= 3600.
endif


;;; exclude subscans with outlier coordinates:
for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
   dimx_cov = ceil(max(*xas_bolos(detarr)))
   dimy_cov = ceil(max(*yas_bolos(detarr)))
   map_cov = fltarr(dimx_cov, dimy_cov)
   for it = long(0), nt - 1 do $
      map_cov((*xas_bolos(detarr))(it, *), (*yas_bolos(detarr))(it, *)) += 1.
   w = where(map_cov gt 0. and map_cov lt max(map_cov) / 3.)
   med_cov = median(map_cov(w))
   nobs_reject = 0
   for i = long(1), nobs - 1 do begin
      nsubs = ind_subscans(i+1-nobs_reject) - ind_subscans(i-nobs_reject)
      ind = ind_subscans(i-nobs_reject) + nsubs / 4 + lindgen(nsubs / 2)
      cov_ref = map_cov((*xas_bolos(detarr))(ind, boloref), (*yas_bolos(detarr))(ind, boloref))
      if mean(cov_ref) lt med_cov then begin
         mask_save_subscan(subscan_to_file(i)) = 0
         print, crs + 'input file ' + files_scans(i) + ' : outlier coordinates: discarded'
         wkeep = lindgen(ind_subscans(i-nobs_reject))
         if i lt nobs - 1 then wkeep = [wkeep, $
            ind_subscans(i+1-nobs_reject) + lindgen(nt-ind_subscans(i+1-nobs_reject))]
         *xas_bolos(detarr) = temporary((*xas_bolos(detarr))(wkeep, *))
         *yas_bolos(detarr) = temporary((*yas_bolos(detarr))(wkeep, *))
         *signals(detarr) = temporary((*signals(detarr))(wkeep, *))
         time_s = temporary(time_s(wkeep))
         nt = n_elements(wkeep)
         nsubs = ind_subscans(i+1-nobs_reject) - ind_subscans(i-nobs_reject)
         flag_scans = [1, ind_subscans(i-nobs_reject) / ind_scans(1:*)]
         ws = where(flag_scans gt 0)
         ws = max(ws)
         ind_scans(ws+1:*) -= nsubs
         ind_subscans = [ind_subscans(0:i-1-nobs_reject), ind_subscans(i+1-nobs_reject:*) - nsubs]
         nobs_reject += 1
      endif
   endfor
   nobs -= nobs_reject
endif



w = where(ind_arrays eq 1)
if max(abs(pix_as(w) - pix_as(w(0)))) eq 0 then begin
   common_grid = 1
   ind = max(w)
   dimxas = max(*xas_bolos(ind)) - min(*xas_bolos(ind)) + 2.4 * rmax_beam * fwhm_as(ind)
   dimyas = max(*yas_bolos(ind)) - min(*yas_bolos(ind)) + 2.4 * rmax_beam * fwhm_as(ind)
   dimx = ceil(dimxas / pix_as(ind))
   dimy = ceil(dimyas / pix_as(ind))
   crpix1 = -(min(*xas_bolos(ind)) - 1.2 * rmax_beam * fwhm_as(ind)) / pix_as(ind)
   crpix2 = -(min(*yas_bolos(ind)) - 1.2 * rmax_beam * fwhm_as(ind)) / pix_as(ind)
endif



;; array loop:

for detarr = 0, 1 do if ind_arrays(detarr) eq 1 then begin
   print, crs + 'building ' + arrays_default(detarr) + ' array map'

   if not keyword_set(common_grid) then begin
      dimxas = max(*xas_bolos(detarr)) - min(*xas_bolos(detarr)) + 2.4 * rmax_beam * fwhm_as(detarr)
      dimyas = max(*yas_bolos(detarr)) - min(*yas_bolos(detarr)) + 2.4 * rmax_beam * fwhm_as(detarr)
      dimx = ceil(dimxas / pix_as(detarr))
      dimy = ceil(dimyas / pix_as(detarr))
      crpix1 = -(min(*xas_bolos(detarr)) - 1.2 * rmax_beam * fwhm_as(detarr)) / pix_as(detarr)
      crpix2 = -(min(*yas_bolos(detarr)) - 1.2 * rmax_beam * fwhm_as(detarr)) / pix_as(detarr)
   endif
   x_bolos = crpix1 + *xas_bolos(detarr) / pix_as(detarr)
   y_bolos = crpix2 + *yas_bolos(detarr) / pix_as(detarr)
   w = where(ind_arrays eq 1)
   if detarr eq max(w) then ptr_free, xas_bolos, yas_bolos


   ;;; scale factor between coarse and fine grids:
   fact = ceil(fwhm_as(detarr) / pix_as(detarr))
   pix_as_coarse = pix_as(detarr) * fact

   ;;; dimensions for the coarse grid used in iterations:
   dimx_iter = dimx / fact
   if dimx mod fact gt 0 then dimx_iter += 1
   dimy_iter = dimy / fact
   if dimy mod fact gt 0 then dimy_iter += 1


   ;;; "PSF" projection:

   model_r_as = dindgen(10000)/ 100.
   model_psf = dblarr(10000)
   w = where(model_r_as le fwhm_as_proj(detarr) / 2.)
   model_psf(w) = 1.

   print, crs + 'projecting "PSF" on ' + chain(pix_as(detarr), 2) + ' arcsec pixels with ' $
      + chain(nstep_psf) + ' steps per pixel'
   compute_psfs, pixsize=pix_as(detarr), fwhm_as_psf=fwhm_as_proj(detarr), $
      rmax_beam=rmax_beam, model_r_as, model_psf
   model_r_as = 0
   model_psf = 0
   npos_psf = nstep_psf^2


   ;;; time-dependent scan speed:
   for i = 0, nobs - 1 do begin
      dd = sqrt((x_bolos(ind_subscans(i)+1:ind_subscans(i+1)-1, boloref) $
          - x_bolos(ind_subscans(i):ind_subscans(i+1)-2, boloref))^2. + $
         (y_bolos(ind_subscans(i)+1:ind_subscans(i+1)-1, boloref) $
          - y_bolos(ind_subscans(i):ind_subscans(i+1)-2, boloref))^2.)
      dt = time_s(ind_subscans(i)+1:ind_subscans(i+1)-1) $
         - time_s(ind_subscans(i):ind_subscans(i+1)-2)
      if i eq 0 then vscan = [dd(0) / dt(0), dd / dt] else vscan = [vscan, dd(0) / dt(0), dd / dt]
   endfor
   vscan = vscan * pix_as(detarr)
   vscan_as_s = median(vscan)
   print, crs + 'median scan speed: ' + chain(vscan_as_s, 2) + ' arcsec/s'

   ;;; average number of time steps required to cross the FWHM:
   nt_cross = floor(pix_as_coarse / vscan_as_s / timestep)
   maxtimediff = 5 * nt_cross * timestep


   ;;; coarse time grid made irregular in order not to mix edges of different subscans:
   tc_min = 1.1 * fwhm_as(detarr) / vscan_as_s
   tc_drift_s = max([tc_drift_s, tc_min])
   nt_aver = floor(tc_drift_s / timestep)
   if nt / nt_aver ge 7000 then begin
      print, crs + 'Noise knee frequency too large for the amount of data to treat.'
      print, 'If reducing it is not acceptable, then the data have to first be divided into spatial blocks.'
      nt_aver = nt / 7000
   endif
   print, 'Assuming for drifts a minimum timescale of ' + chain(nt_aver * timestep, 2) + ' s'
   print, 'Number of samples to cross the FWHM: ' + chain(nt_cross)

   w = where(time_s - time_s(0) le tc_drift_s)
   ind_aver = [max(w) + 1]
   wsubs = 0
   repeat begin
      w = where(time_s(max(ind_aver):*) - time_s(max(ind_aver)) le tc_drift_s)
      ind_aver_next = max(ind_aver) + max(w) + 1
      if ind_aver_next lt ind_subscans(wsubs+1) then ind_aver = [ind_aver, ind_aver_next] $
         else begin
            ind_aver = [ind_aver, ind_subscans(wsubs+1)]
            wsubs += 1
         endelse
   endrep until (wsubs ge nobs)
   nt_save = n_elements(ind_aver)
   time_save = (time_s([0, ind_aver(0:nt_save-2)]) + time_s(ind_aver-1)) / 2.


   ;;; minimum number of samples per bolometer crossing:
   ntmin_pass = floor(pix_as_coarse / vscan / timestep)
   w = where(vscan lt 0.5 * vscan_as_s, cw)
   if cw ge 1 then ntmin_pass(w) = floor(2. * pix_as_coarse / vscan_as_s / timestep)
   w = where(vscan gt 2. * vscan_as_s, cw)
   if cw ge 1 then ntmin_pass(w) = floor(pix_as_coarse / vscan_as_s / 2. / timestep)


   ;;; estimate the noise level to be able to detect significant flux gradients:
   compute_wnoise
   print, crs + 'min and max noise for all subscans:'
   print, chain(min(maxnoise), 5) + '   ' + chain(max(maxnoise), 5)


   mask = round(mask)
   w = where(mask eq 1, cw, ncomplement=nz)
   if nz ge 1 then begin
      print, crs + 'Excluding ' + chain(nz) + ' noisy bolometers'
      *signals(detarr) = temporary((*signals(detarr))(*, w))
      nbolo(detarr) = cw
      maxnoise = maxnoise(*, w)
      x_bolos = temporary(x_bolos(*, w))
      y_bolos = temporary(y_bolos(*, w))
      print, 'new min and max noise:'
      print, chain(min(maxnoise), 5) + '   ' + chain(max(maxnoise), 5) + crs
   endif
   bijection = lonarr(npix_side^2)
   bijection(w) = lindgen(nbolo(detarr))
   boloref_new = bijection(boloref)




   ;;; variables to speed up projections:

   indxy_beam = floor(x_bolos) + dimx * floor(y_bolos)
   histo_indxy = histogram(indxy_beam, reverse_indices=revind_fine, min=0., max=dimx*dimy-1)
   wproj_fine = where(histo_indxy ge 3, nproj_fine)
   histo_indxy = 0

   vectpos_startpsf = fix(wproj_fine mod dimx - xcmin_psf) $
      + dimx * fix(wproj_fine / dimx - xcmin_psf)

   tab_ind = indgen(dim_psf)
   sqdim_psf = dim_psf^2
   vectpos_psf = reform(rebin(tab_ind, dim_psf, dim_psf), sqdim_psf) $
      + dimx * rebin(tab_ind, sqdim_psf, /sample)
   tab_ind = 0


   ;;; projection before drift correction:
   proj_approx_psf
   map_simple = map_signal
   wavoid_proj = where(map_weight(indxy_beam) lt max(map_weight) / 30.)
   indxy_beam = 0


   ;;; variables to speed up drift determination:
   off_coord = -0.5
   ntb = nt * nbolo(detarr)
   indxy_beam_coarse = reform(dimx_iter * floor(y_bolos / fact) + floor(x_bolos / fact), ntb)
   indxy_beam_coarse(wavoid_proj) = -999
   off_indxy_beam_coarse = reform(dimx_iter * floor((y_bolos + fact * off_coord) / fact) $
      + floor((x_bolos + fact * off_coord) / fact), ntb)
   off_indxy_beam_coarse(wavoid_proj) = -999
   histo_indxy = histogram([indxy_beam_coarse, off_indxy_beam_coarse+dimx_iter*dimy_iter], $
      reverse_indices=revind, min=0., max=2*dimx_iter*dimy_iter-1)
   indxy_beam_coarse = 0
   off_indxy_beam_coarse = 0
   n1_revind = n_elements(histo_indxy) + 1
   revind(n1_revind:*) = temporary(revind(n1_revind:*)) mod (nt * nbolo(detarr))
   med = median(ntmin_pass)
   wproj = where(histo_indxy ge med, nproj)
   histo_indxy = 0



   ;;; drift subtraction loop:

   iter = 0
   aver_completed = 1
   mode_drifts = 'drifts of individual bolometers'
   amp_drifts = dblarr(nobs, nbolo(detarr))
   drifts = dblarr(nt, nbolo(detarr))

   repeat begin
      print, 'iteration ' + chain(iter) + ' : ' + mode_drifts

      if iter gt 0 then proj_approx_psf
      w = where(map_weight gt 0., complement=wz)
      map_signal(wz) = median(map_signal(w))
      if keyword_set(visu) then begin
         disp_ima, map_signal, win=1, title='iteration '+chain(iter)+' : input map', $
            weightmap=map_weight
         if iter gt 0 then disp_ima, map_simple - map_signal, win=2, $
            title='map of cumulated removed drifts', weightmap=map_weight
      endif


      amp_drifts *= 0.
      drifts *= 0.
      array_drifts = dblarr(nt_save, nbolo(detarr))
      array_weight = dblarr(nt_save, nbolo(detarr))
      array_npass = dblarr(nt_save, nbolo(detarr))
      proj_rectify_indiv
      array_weight = 0

      w = where(array_npass gt 0.)
      med = median(array_drifts(w))
      for b = long(0), nbolo(detarr) - 1 do for i = 0, nobs - 1 do begin
         w = where(time_save ge time_s(ind_subscans(i)) and time_save le time_s(ind_subscans(i+1)-1) $
            and array_npass(*, b) gt 0., cw)
         if cw ge 3 then begin
            tab_time = [time_s(ind_subscans(i)) - timestep, time_save(w), time_s(ind_subscans(i+1)-1) + timestep]
            tab_drifts = [array_drifts(w(0), b), array_drifts(w, b), array_drifts(w(cw-1), b)]
            subs_drifts = interpol(tab_drifts, tab_time, time_s(ind_subscans(i):ind_subscans(i+1)-1))
            drifts(ind_subscans(i):ind_subscans(i+1)-1, b) = subs_drifts
            dev = sqrt(mean((subs_drifts - med)^2.))
            amp_drifts(i, b) = 3. * dev
         endif
      endfor
      array_npass = 0
      off = median(drifts)
      drifts -= off
      array_drifts -= off


;      if keyword_set(visu) then begin
      if keyword_set(visu) and iter eq 0 then begin
         window, 0, xs=1000
         signals_aver = total(*signals(detarr), 2) / double(nbolo(detarr))
         med = median(signals_aver)
         dev = sqrt(mean(((*signals(detarr))(*, boloref_new) - med)^2.))
         b38 = where(bijection eq 38)
         otherplot, time_s, signals_aver - med, charsize=1.5, yrange=[-4.*dev, 2.*dev], $
            title='iteration '+chain(iter)+' : drifts (bolometers ' + chain(boloref) $
            + ' and ' + chain(b38(0)) + ')', $
            xtitle='LST (s)', ytitle='surface brightness (Jy/beam)'
         b = boloref_new
         otherplot, /over, psym=4, time_save, array_drifts(*, b)-dev, color=10
         otherplot, /over, time_s, drifts(*, b)-dev, color=100
         b = 38
         otherplot, /over, psym=4, time_save, array_drifts(*, b)-2.*dev, color=50
         otherplot, /over, time_s, drifts(*, b)-2.*dev, color=150
         xlabel = 0.95 * min(time_s) + 0.05 * max(time_s)
         xyouts, [xlabel], [1.5*dev], 'average signal', color=0, charsize=1.5, charthick=2.
         xyouts, [xlabel], [-3.7*dev], 'uncorrelated drifts of bolometers ' + chain(boloref) $
            + ' and ' + chain(b38(0)) + ' (with offsets)', color=100, charsize=1.5, charthick=2.
         print, crs + 'Type ".c" to continue.'
	 print, "nobs = ", nobs
;         stop
         b = boloref_new
         xrange = time_s([0, ind_subscans(5)])
;         xrange = time_s([0, ind_subscans(nobs)])
         otherplot, time_s, (*signals(detarr))(*, b) - med, charsize=1.5, $
            yrange=[-3.*dev, 3.*dev], xrange=xrange, $
            title='zoom in time  -  iteration '+chain(iter)+' : drifts (bolometer ' + chain(boloref) + ')', $
            xtitle='LST (s)', ytitle='surface brightness (Jy/beam)'
         otherplot, /over, psym=4, time_save, array_drifts(*, b)-dev, color=10
         otherplot, /over, time_s, drifts(*, b)-dev, color=100
         xlabel = 0.95 * xrange(0) + 0.05 * xrange(1)
         xyouts, [xlabel], [2.5*dev], 'signal of bolometer ' + chain(boloref), $
            color=0, charsize=1.5, charthick=2.
         xyouts, [xlabel], [-2.7*dev], 'uncorrelated drift (with offset)', $
            color=100, charsize=1.5, charthick=2.
         print, crs + 'Type ".c" to continue.'
;         stop
      endif

      array_drifts = 0


      ;;; prepare next iteration:

      *signals(detarr) -= drifts
      iter += 1
      print, '              removed drifts amplitude / noise (median) = ' $
         + chain(median(amp_drifts / maxnoise), 2)

   endrep until (iter ge iter_max or median(amp_drifts / maxnoise) lt 1.)



   ;;; save results:

   print, crs + 'Saving processed subscans in ' + dir_in
   w = where(mask eq 1, cw)
   ind_obs = long(-1)
   for k = long(0), nscans - 1 do for i = long(0), nsubscans - 1 do begin
      ind_file = k * nsubscans + i
      if mask_save_subscan(ind_file) eq 1 then begin
         ind_obs += 1
         nsub = ind_subscans(ind_obs+1) - ind_subscans(ind_obs)
         cube_signal = fltarr(npix_side^2, nsub)
         for p = 0, cw - 1 do cube_signal(w(p), *) = $
            (*signals(detarr))(ind_subscans(ind_obs):ind_subscans(ind_obs+1)-1, p)
         cube_signal = reform(temporary(cube_signal), npix_side, npix_side, nsub)
         restore, dir_in + files_scans(ind_file)
         donnees_red.cube = cube_signal
	 
	 if keyword_set(pfov) then begin
	   donnees_red.cdelt1 = pfov
	   donnees_red.cdelt2 = pfov
	 endif
	 
         save, donnees_red, filename=dir_in+files_out(ind_file), /xdr
      endif
   endfor
   carte_scan = 0
   donnees_uncal = 0
   donnees = 0
   donnees_red = 0



   if keyword_set(visu) then begin
      proj_approx_psf
       w = where(map_weight gt 0., complement=wz)
      map_signal(wz) = median(map_signal(w))
      disp_ima, map_signal, win=1, title='iteration '+chain(iter)+' : input map', weightmap=map_weight
      disp_ima, map_simple - map_signal, win=2, title='map of cumulated removed drifts', $
         weightmap=map_weight
   endif

endif

print, crs + 'Subtraction of uncorrelated drifts completed.'

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro compute_wnoise

common scanquant, detarr, signals, x_bolos, y_bolos, nbolo
common timekeeping, time_s, timestep, maxtimediff, nt, nobs, ind_scans, ind_subscans
common algoquant, maxnoise, nt_cross, ntmin_pass, ind_aver


maxnoise = dblarr(nobs, nbolo(detarr))

for i = long(0), nobs - 1 do begin
   ind_end = ind_subscans(i+1) - 1
   if (ind_end - ind_subscans(i) + 1) mod 2 gt 0 then ind_end = ind_end - 1
   nt_scan = ind_end - ind_subscans(i) + 1
   dt = nt_scan * timestep
   freq = [dindgen(nt_scan/2+1) / dt, -reverse((dindgen(nt_scan/2-1) + 1.) / dt)]
   w_high = where(abs(freq) gt 2.)
   for b = long(0), nbolo(detarr) - 1 do begin
      fft_bolo = fft((*signals(detarr))(ind_subscans(i):ind_end, b))
      power = mean((abs(fft_bolo(w_high)))^2.)
      ;;; peculiar normalization of FFT in IDL:
      maxnoise(i, b) = sqrt(power * nt_scan)
   endfor
endfor

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro compute_psfs, pixsize=pixsize, fwhm_as_psf=fwhm_as_psf, rmax_beam=rmax_beam, $
                  model_r_as, model_psf

common psfuse, psfs, dim_psf, xcmin_psf, nstep_psf


n_beam = 2. * rmax_beam
dim_psf = round(n_beam * fwhm_as_psf / pixsize)
if dim_psf mod 2 eq 0 then dim_psf += 1
xcmin_psf = dim_psf / 2
np = nstep_psf^2
psfs = dblarr(np, dim_psf, dim_psf)

xc_psf = dblarr(np)
yc_psf = dblarr(np)
for i = long(0), nstep_psf - 1 do begin
   xc_psf(i*nstep_psf:(i+1)*nstep_psf-1) = dindgen(nstep_psf)
   yc_psf(i*nstep_psf:(i+1)*nstep_psf-1) = i
endfor
xc_psf = xc_psf / double(nstep_psf) + xcmin_psf
yc_psf = yc_psf / double(nstep_psf) + xcmin_psf

dim_fine = dim_psf * nstep_psf
tab_x_fine = (dblarr(dim_fine) + 1.) ## dindgen(dim_fine) + 0.5
tab_y_fine = dindgen(dim_fine) ## (dblarr(dim_fine) + 1.) + 0.5
xc_fine = xc_psf * nstep_psf
yc_fine = yc_psf * nstep_psf

scale = pixsize / float(nstep_psf)
for i = long(0), np - 1 do begin
   ray_as = sqrt((tab_x_fine - xc_fine(i))^2. + (tab_y_fine - yc_fine(i))^2.) * scale
   beam_fine = interpol(model_psf, model_r_as, ray_as)
   beam = rebin(beam_fine, dim_psf, dim_psf)
   psfs(i, *, *) = beam / total(beam)
endfor
beam_sqas = total(beam_fine) * scale^2.

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro proj_rectify_indiv

common scanquant, detarr, signals, x_bolos, y_bolos, nbolo
common timekeeping, time_s, timestep, maxtimediff, nt, nobs, ind_scans, ind_subscans
common psfuse, psfs, dim_psf, xcmin_psf, nstep_psf
common mapuse, map_signal, map_weight, dimx, dimy, dimx_iter, dimy_iter, fact
common algoquant, maxnoise, nt_cross, ntmin_pass, ind_aver
common finegridproj, wproj_fine, revind_fine, nproj_fine, vectpos_startpsf, vectpos_psf
common coarsegridproj, wproj, revind, nproj
common driftmatrices, array_drifts, array_weight, array_npass


nt_save = n_elements(ind_aver)
time_save = (time_s([0, ind_aver(0:nt_save-2)]) + time_s(ind_aver-1)) / 2.

ntmin_pass_fine = ntmin_pass / fact
w = where(ntmin_pass_fine lt 3, cw)
if cw ge 1 then ntmin_pass_fine(w) = 3
nproj_zoom = fact^2
tab_dxpf = indgen(nproj_zoom) mod fact
tab_dypf = indgen(nproj_zoom) / fact


;;; correct signal for PSF wings before drift correction:
map_rebin = rebin(map_signal, 5*dimx, 5*dimy)
diff_signals = *signals(detarr) - map_rebin(floor(5.*x_bolos) + 5*dimx * floor(5.*y_bolos))
map_rebin = 0


tab_aver = intarr(nt)
tab_aver(0:ind_aver(0)-1) = 0
for i = long(1), nt_save - 1 do tab_aver(ind_aver(i-1):ind_aver(i)-1) = i

npass_crit = ceil(median(revind(wproj+1) - revind(wproj)) / median(ntmin_pass) / 4.)



for k = long(0), nproj - 1 do begin
   revind_0 = revind(wproj(k))
   revind_1 = revind(wproj(k)+1)
   if revind_0 ne revind_1 then wp = revind(revind_0:revind_1-1) else wp = 0
   cp = n_elements(wp)
   wt = wp mod nt

   if cp ge 3 * mean(ntmin_pass(wt)) then begin
      wbolo = wp / nt
      distrib_time = time_s(wt)

      diff_bolo = [1, wbolo(1:*) - wbolo(0:cp-2)]
      diff_time = [0., distrib_time(1:*) - distrib_time(0:cp-2)]
      ind_startpass = where(diff_bolo ge 1 or diff_time gt maxtimediff, npass)
      if npass ge 2 then ind_endpass = [ind_startpass(1:*) - 1, cp-1] else ind_endpass = [cp-1]
      distrib_flux = diff_signals(wp)

      med_pass = dblarr(npass)
      var_pass = dblarr(npass)
      ns_pass = ind_endpass - ind_startpass + 1
      flag_ntmin = intarr(npass)
      maxnoise_pass = dblarr(npass)
      for i = 0, npass - 1 do begin
         tab_flux = distrib_flux(ind_startpass(i):ind_endpass(i))
         med_pass(i) = median(tab_flux)
         var_pass(i) = mean(abs(tab_flux - med_pass(i)))
         if ns_pass(i) ge min(ntmin_pass(wt(ind_startpass(i):ind_endpass(i)))) $
            then flag_ntmin(i) = 1 else flag_ntmin(i) = 0
         flag_subscans = [1, wt(ind_startpass(i)) / ind_subscans(1:*)]
         wsubs = where(flag_subscans gt 0)
         wsubs = max(wsubs)
         maxnoise_pass(i) = maxnoise(wsubs, wbolo(ind_startpass(i)))
      endfor
      maxnoise_weight = maxnoise_pass / median(maxnoise)

      wcen = where(flag_ntmin eq 1, npass_cen)
      wsave = where(flag_ntmin eq 1 and var_pass lt maxnoise_pass, npass_save)
      xpf0 = min(floor(x_bolos(wp)))
      ypf0 = min(floor(y_bolos(wp)))

      if npass_save ge max([npass_crit, floor(0.85 * npass_cen)]) then begin
         itmean_pass = wt((ind_startpass(wsave) + ind_endpass(wsave)) / 2)
         it_aver = tab_aver(itmean_pass)
         w = where(time_s(itmean_pass) lt time_save(it_aver) and it_aver gt 0, cw)
         if cw ge 1 then it_aver(w) -= 1
         wgt2 = (time_s(itmean_pass) - time_save(it_aver)) / (time_save(it_aver+1) - time_save(it_aver))
         w = where(time_s(itmean_pass) lt time_save(it_aver) and it_aver eq 0, cw)
         if cw ge 1 then wgt2(w) = 0.
         w = where(it_aver eq nt_save - 1, cw)
         if cw ge 1 then begin
            it_aver(w) -= 1
            wgt2(w) = 1.
         endif
         wgt1 = 1. - wgt2
         weight = 1. / maxnoise_weight(wsave)
         val = total(med_pass(wsave) * weight) / total(weight)
         drift_pass = med_pass(wsave) - val
         weight += mean(weight)
         bolo_pass = wbolo(ind_startpass(wsave))
         ind_array = it_aver + nt_save * bolo_pass
         array_drifts(ind_array) += drift_pass * weight * wgt1
         array_weight(ind_array) += weight * wgt1
         array_npass(ind_array) += npass_save * weight * wgt1
         ind_array += 1
         array_drifts(ind_array) += drift_pass * weight * wgt2
         array_weight(ind_array) += weight * wgt2
         array_npass(ind_array) += npass_save * weight * wgt2

      endif else for kf = long(0), nproj_zoom - 1 do begin
         xpf = xpf0 + tab_dxpf(kf)
         ypf = ypf0 + tab_dypf(kf)
         wpf = where(floor(x_bolos(wp)) eq xpf and floor(y_bolos(wp)) eq ypf, cpf)
         if cpf ge 1 then wtf = wt(wpf) else wtf = 0
         if cpf ge 3 * mean(ntmin_pass_fine(wtf)) then begin
            wpsub = wp(wpf)
            wfbolo = wbolo(wpf)
	    distrib_time_f = distrib_time(wpf)
            diff_bolo = [1, wfbolo(1:*) - wfbolo(0:cpf-2)]
            diff_time = [0., distrib_time_f(1:*) - distrib_time_f(0:cpf-2)]
            ind_startpass = where(diff_bolo ge 1 or diff_time gt maxtimediff, npass)
            if npass ge 2 then ind_endpass = [ind_startpass(1:*) - 1, cpf-1] else ind_endpass = [cpf-1]
            distrib_flux_f = diff_signals(wpsub)

            med_pass = dblarr(npass)
            var_pass = dblarr(npass)
            ns_pass = ind_endpass - ind_startpass + 1
            flag_ntmin = intarr(npass)
            maxnoise_pass = dblarr(npass)
            for i = 0, npass - 1 do begin
               tab_flux = distrib_flux_f(ind_startpass(i):ind_endpass(i))
               med_pass(i) = median(tab_flux)
               var_pass(i) = mean(abs(tab_flux - med_pass(i)))
               if ns_pass(i) ge min(ntmin_pass_fine(wtf(ind_startpass(i):ind_endpass(i)))) $
                  then flag_ntmin(i) = 1 else flag_ntmin(i) = 0
               flag_subscans = [1, wtf(ind_startpass(i)) / ind_subscans(1:*)]
               wsubs = where(flag_subscans gt 0)
               wsubs = max(wsubs)
               maxnoise_pass(i) = maxnoise(wsubs, wfbolo(ind_startpass(i)))
            endfor
            maxnoise_weight = maxnoise_pass / median(maxnoise)

            wcen = where(flag_ntmin eq 1, npass_cen)
            wsave = where(flag_ntmin eq 1 and var_pass lt maxnoise_pass, npass_save)

            if npass_save ge max([4, floor(0.85 * npass_cen)]) then begin
               itmean_pass = wtf((ind_startpass(wsave) + ind_endpass(wsave)) / 2)
               it_aver = tab_aver(itmean_pass)
               w = where(time_s(itmean_pass) lt time_save(it_aver) and it_aver gt 0, cw)
               if cw ge 1 then it_aver(w) -= 1
               wgt2 = (time_s(itmean_pass) - time_save(it_aver)) / (time_save(it_aver+1) - time_save(it_aver))
               w = where(time_s(itmean_pass) lt time_save(it_aver) and it_aver eq 0, cw)
               if cw ge 1 then wgt2(w) = 0.
               w = where(it_aver eq nt_save - 1, cw)
               if cw ge 1 then begin
                  it_aver(w) -= 1
                  wgt2(w) = 1.
               endif
               wgt1 = 1. - wgt2
               weight = 1. / maxnoise_weight(wsave) / fact
               val = total(med_pass(wsave) * weight) / total(weight)
               drift_pass = med_pass(wsave) - val
               weight += mean(weight)
               bolo_pass = wfbolo(ind_startpass(wsave))
               ind_array = it_aver + nt_save * bolo_pass
               array_drifts(ind_array) += drift_pass * weight * wgt1
               array_weight(ind_array) += weight * wgt1
               array_npass(ind_array) += npass_save * weight * wgt1
               ind_array += 1
               array_drifts(ind_array) += drift_pass * weight * wgt2
               array_weight(ind_array) += weight * wgt2
               array_npass(ind_array) += npass_save * weight * wgt2

            endif
         endif
      endfor

   endif
endfor

w = where(array_weight gt 0)
array_drifts(w) /= array_weight(w)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro proj_approx_psf

common scanquant, detarr, signals, x_bolos, y_bolos, nbolo
common timekeeping, time_s, timestep, maxtimediff, nt, nobs, ind_scans, ind_subscans
common psfuse, psfs, dim_psf, xcmin_psf, nstep_psf
common mapuse, map_signal, map_weight, dimx, dimy, dimx_iter, dimy_iter, fact
common algoquant, maxnoise, nt_cross, ntmin_pass, ind_aver
common finegridproj, wproj_fine, revind_fine, nproj_fine, vectpos_startpsf, vectpos_psf


map_weight = dblarr(dimx, dimy)
map_signal = dblarr(dimx, dimy)

indpsf = nstep_psf * floor(nstep_psf * (y_bolos - floor(y_bolos))) $
   + floor(nstep_psf * (x_bolos - floor(x_bolos)))
sq_dim_psf = dim_psf^2
psfs_reform = reform(psfs, nstep_psf^2, sq_dim_psf)
tab_subs = lonarr(ind_subscans(1))
for i = long(1), nobs - 1 do tab_subs = [temporary(tab_subs), i + lonarr(ind_subscans(i+1) - ind_subscans(i))]
weight = 1. / maxnoise

for k = long(0), nproj_fine - 1 do begin
   revind_0 = revind_fine(wproj_fine(k))
   revind_1 = revind_fine(wproj_fine(k)+1)
   wp = revind_fine(revind_0:revind_1-1)
   vectpos = vectpos_startpsf(k) + vectpos_psf
   weight_sample = weight(tab_subs(wp mod nt) + nobs * (wp / nt))
   add_weight = 0.
   add_signal = 0.
   histo = histogram(indpsf(wp), locations=indpsf_sample)
   wstep = where(histo gt 0, cw)
   for ind = long(0), cw - 1 do begin
      indpsf_cur = indpsf_sample(wstep(ind))
      w_cur = where(indpsf(wp) eq indpsf_cur)
      add_weight += total(weight_sample(w_cur)) * psfs_reform(indpsf_cur, *)
      add_signal += total((*signals(detarr))(wp(w_cur)) * weight_sample(w_cur)) * psfs_reform(indpsf_cur, *)
   endfor
   map_weight(vectpos) += add_weight
   map_signal(vectpos) += add_signal
endfor

wcov = where(map_weight gt 0.)
map_signal(wcov) = map_signal(wcov) / map_weight(wcov)
map_weight = map_weight / mean(weight)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro disp_ima, map, win=win, title=title, weightmap=weightmap

nx = n_elements(map(*, 0))
ny = n_elements(map(0, *))
fact_zoom_guess = 800. / max([nx, ny])

n = 0.
repeat begin
   n += 1.
   fact_zoom = floor(fact_zoom_guess * n) / n
endrep until fact_zoom gt 0.
new_nx = floor(nx * fact_zoom) / fact_zoom
new_ny = floor(ny * fact_zoom) / fact_zoom
xs = new_nx * fact_zoom
ys = new_ny * fact_zoom
if fact_zoom ge 1. then sample = 1 else sample = 0

window, win, xs=xs, ys=ys, title=title
if keyword_set(weightmap) then begin
   w = where(weightmap gt max(weightmap) / 10.)
   min_map = min(map(w))
   max_map = max(map(w))
endif else begin
   min_map = min(map)
   max_map = max(map)
endelse
tvscl, rebin(map(0:new_nx-1, 0:new_ny-1), xs, ys, sample=sample) >min_map <max_map

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function chain, val, ndec

if not keyword_set(ndec) then ndec = 0
n = n_elements(val)
ch_sign = strarr(n)
w = where(val lt 0., cw)
if cw ge 1 then ch_sign(w) = '-'
value = abs(val)
val_int = floor(value)
val_dec = round((value - val_int) * 10.^ndec)
w = where(val_dec eq 10.^ndec, cw)
if cw ge 1 then begin
   val_int(w) = val_int(w) + 1
   val_dec(w) = 0
endif
ch_int = strtrim(string(val_int), 2)
ch_dec = strtrim(string(val_dec), 2)
size_dec = strlen(ch_dec)
nz = ndec - size_dec
for i = 0, n - 1 do for k = 0, nz(i) - 1 do ch_dec(i) = '0' + ch_dec(i)
res = ch_sign + ch_int 
if ndec gt 0 then res = res + '.' + ch_dec
if n eq 1 then res = res(0)
return, res

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function find_all_regex, str, regexpr

str_rest = str
match = ''
repeat begin
   pos = stregex(str_rest, regexpr, length=len)
   match = [match, strmid(str_rest, pos, len)]
   n = strlen(str_rest)
   if pos + len lt n then str_end = strmid(str_rest, pos + len) else str_end = ''
   if pos gt 0 then str_rest = strmid(str_rest, 0, pos) + str_end else str_rest = str_end
endrep until len lt 1
return, match(1:*)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro otherplot, x, y, psym=psym, over=over, xrange=xrange, yrange=yrange, $
    title=title, xtitle=xtitle, ytitle=ytitle, charsize=charsize, color=color, zero=zero

if not keyword_set(psym) then psym = 0
if not keyword_set(xrange) then begin
   xmin = min(x)
   xmax = max(x)
   rx = xmax - xmin
   if rx eq 0. then rx = xmax
   xrange = [xmin - rx/20., xmax + rx/20.]
endif
if not keyword_set(yrange) then begin
   ymin = min(y)
   ymax = max(y)
   if keyword_set(zero) then ymin = 0.
   ry = ymax - ymin
   if ry eq 0. then ry = ymax
   if keyword_set(zero) then ydeb = 0. else ydeb = ymin - ry/20.
   yrange = [ydeb, ymax + ry/20.]
endif
if not keyword_set(title) then title = ''
if not keyword_set(xtitle) then xtitle = ''
if not keyword_set(ytitle) then ytitle = ''
if not keyword_set(color) then color = 0

if not keyword_set(over) then plot, x, y, psym=psym, xrange=xrange, xstyle=1, $
   yrange=yrange, ystyle=1, title='!6'+title, xtitle='!6'+xtitle, ytitle='!6'+ytitle, $
   charsize=charsize, color=color, background=!d.n_colors-1, $
   thick=2., xthick=2., ythick=2., charthick=2., symsize=symbsize $
 else oplot, x, y, psym=psym, color=color, thick=2., symsize=symbsize

save, filename='map_signal', map_signal

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
