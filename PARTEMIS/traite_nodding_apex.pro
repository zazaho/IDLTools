pro traite_nodding_apex,scan_list,image,rms,cube_nodding,nodding,dir_dat=dir_dat,nopowermap=nopowermap,tau=tau,mean_lst=mean_lst,kill50Hz=kill50Hz,decorrel=decorrel,image_nod=image_nod,pwv=pwv

; Usage : traite_nodding_apex, scan_saturn_49657, saturn_image_49657, rms, cube_nodding, nodding, tau = 0.87, mean_lst = mean_lst, /kill50Hz, image_nod=image_nod, pwv=pwv
;
;	  scan_b59tts_44532 = 44532L + indgen(12)
;         traite_nodding_apex, scan_b59tts_44532, b59tts_44532, rms, cube_nodding, nodding, tau = 0.9, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_neptune_24528 = [24528L, 24529L, 24530L, 24531L, 24531L, 24532L, 24533L, 24534L]
;         traite_nodding_apex, scan_neptune_24528, neptune_24528, rms, cube_nodding, nodding, tau = 1.5, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_24498 = [24498L, 24499L, 24500L, 24501L]
;         traite_nodding_apex, scan_uranus_24498, uranus_24498, rms, cube_nodding, nodding, tau = 1.5, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_24502 = [24502L, 24503L, 24504L, 24505L]
;         traite_nodding_apex, scan_uranus_24502, uranus_24502, rms, cube_nodding, nodding, tau = 1.5, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_24498_tot = [24498L, 24499L, 24500L, 24501L, 24502L, 24503L, 24504L, 24505L]
;         traite_nodding_apex, scan_uranus_24498_tot, uranus_24498_tot, rms, cube_nodding, nodding, tau = 1.5, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_24950 = [24950L, 24951L, 24952L, 24953L]
;         traite_nodding_apex, scan_uranus_24950, uranus_24950, rms, cube_nodding, nodding, tau = 0.49, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_25021 = [25021, 25022, 25023, 25024]
;         traite_nodding_apex, scan_uranus_25021, uranus_25021, rms, cube_nodding, nodding, tau = 0.5, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_hd181327_24959 = indgen(48)+24959
;         traite_nodding_apex, scan_hd181327_24959, hd181327_24959, rms, cube_nodding, nodding, tau = 0.49, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_neptune_25010 = [25010, 25011, 25012, 25013, 25014, 25015, 25016, 25017]
;         traite_nodding_apex, scan_neptune_25010, neptune_25010, rms, cube_nodding, nodding, tau = 0.5, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_mars_20540 = [20540L, 20541L, 20542L, 20543L, 20544L, 20545L, 20546L, 20547L, 20548L, 20549L, 20550L, 20551L]
;         traite_nodding_apex, scan_mars_20540, mars_20540, rms, cube_nodding, nodding, tau = 0.9, mean_lst = mean_lst, /kill50Hz, image_nod=image_nod, pwv=pwv
;
;	  scan_mars_22912 = [22912L, 22913L, 22914L, 22915L, 22916L, 22917L, 22918L, 22919L,22920L, 22921L, 22922L, 22923L, 22924L, 22926L, 22927L]
;         traite_nodding_apex, scan_mars_22912, mars_22912, rms, cube_nodding, nodding, tau = 0.79, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;	  scan_mars = [22912L, 22913L, 22914L, 22915L]
;         traite_nodding_apex, scan_mars, mars_22912_test, rms, cube_nodding, nodding, tau = 0.79, mean_lst = mean_lst, /kill50Hz, image_nod=image_nod, pwv=pwv
;
;	  Disques = '/Users/artemis/Documents/apexdata/Disques/'
;	  restore, Disques+'hd97048_49703.xdr', /verb
;	  scan_hd_49703 = scanhd97_2
;	  traite_nodding_apex, scan_hd_49703, hd_image_49703, rms, cube_nodding, nodding, tau = 0.85, mean_lst = mean_lst, /kill50Hz, image_nod=image_nod, pwv=pwv
;
;	  traite_nodding_apex, scan_hd_49703, hd_image_49703, rms, cube_nodding, nodding, tau = 0.85, mean_lst = mean_lst, /kill50Hz, /decorrel, image_nod=image_nod, pwv=pwv
;	  gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/hd97048_49703_nod.gdf", image_nod, /write
;
;	  traite_nodding_apex, scan_hd100546_50709, hd100546_image_50709, rms, cube_nodding, nodding, tau = 1.3, mean_lst = mean_lst, /kill50Hz, /decorrel, image_nod=image_nod, pwv=pwv
;	  gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/hd100546_50709_nod.gdf", image_nod, /write
;
;	  Disques = '/Users/artemis/Documents/apexdata/Disques/'
;	  restore, Disques+'betapic_50175.xdr', /verb
;	  scan_betapic_50175 = scanbeta
;	  traite_nodding_apex, scan_betapic_50175, betapic_image_50175, rms, cube_nodding, nodding, tau = 0.65, mean_lst = mean_lst, /kill50Hz, /decorrel, image_nod = image_nod, pwv = pwv
;	  crota1 = 111.2	; Mean rotation angle
;	  rot_image = rot(image_nod,-crota1)
;
; Inputs   : scan_nod_list (+ dir_log, dir_dat if set)
; Outputs  : image, cube_nodding, nodding
;

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	TRAITE_NODDING_APEX, nod_list, image, cube_nodding, nodding, '
	print, '		dir_dat=dir_dat, nopowermap=nopowermap, tau=tau, mean_lst=mean_lst'
	return
endif

order = scan_list-scan_list(0)
order = order - fix(order/4)*4
phase = order*0
phase(where(order eq 0 or order eq 3)) = 1
phase(where(order eq 1 or order eq 2)) = -1

;Calib_partemis = '/Users/artemis/Desktop/Calib_partemis/'
;
;restore, Calib_partemis+'goodpix_ima_mars_48812_50010.xdr', /verb	;  <-- goodpix_ima
;restore, Calib_partemis+'flat_mars_48812_50010.xdr', /verb		;  <-- flat_mars
;flat = flat_mars

COMMON obs1_configb, work_dir, project_name, calibration_table     ; chargement des common variables depuis obs1_config

init_obs, scan_number=scan_list(0), type='map', init_obs_str

goodpix_ima = init_obs_str.goodpix_ima
flat = init_obs_str.flat_field


dir=find_all_dir(work_dir + 'apexdata/map_otf_xdr/' + project_name)  ; si le repertoire ou l'on souhaite stocker les donnees reduites n'existe pas, il est cree

if dir EQ '' then begin

print, 'creating directory'+ work_dir + 'map_otf_xdr/' + project_name

spawn, 'mkdir' + ' ' + work_dir + 'apexdata/map_otf_xdr/' + project_name

endif

n_file = n_elements(scan_list)

cubetot = dblarr(16,16,2)
cube_nodding = fltarr(16,16,n_file)
cube_nodding_alt = fltarr(16,16,n_file)
rms_nodding = fltarr(16,16,n_file)
;nodding=intarr(n_file)
nodding = phase
mean_lst = fltarr(n_file)
pwv = fltarr(n_file)

!p.multi=[0,5,6]

bidon = dblarr(16,16)
bidon1 = dblarr(16,16)
;;;bidon(10:13,12:14) = 1.
bidon1(9:13,10:12) = 1
bidon(5:10,2:4) = 1
mask = bidon
mask1 = bidon1

index_good = where(goodpix_ima eq 1)
index_mask = where(mask eq 1)
index_mask1 = where(mask1 eq 1)

Conv = 1.

;
if ((scan_list(0) ge 20522) and (scan_list(0) le 47547)) then begin
  Conv = 2.55e-4				; pW ff corr/Jy		17 May 2009
endif
;
Conv = init_obs_str.conv
;
print, "Conv = ", Conv
;

rot_angle = fltarr(n_file)

total_int_time = 0.

for i=0, n_file-1 do begin
    init_obs, scan_number=scan_list(i), type='map', init_obs_str 
;    
    donnees = read_apex(scan_list(i), '1', filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)
;    donnees = read_apex(scan_list(i), '1', filetype, path='/Users/artemis/Documents/apexdata/basic_xdr_2007/')
    phase(i) = fix(sgn(mean(donnees.datapar.longoff)))
    pwv(i) = donnees.obslog.pwv
endfor
pwv_mean = mean(pwv)
print, "Mean PWV (mm)  = ", pwv_mean

for i=0, n_file-1 do begin
    init_obs, scan_number=scan_list(i), type='map', init_obs_str 
;    
    donnees = read_apex(scan_list(i), '1', filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)
;    donnees = read_apex(scan_list(i), '1', filetype, path='/Users/artemis/Documents/apexdata/basic_xdr_2007/' )
    
    if i eq 0 then lst_start = donnees.lst(0)

    if i eq n_file-1 then lst_end = donnees.lst((size(donnees.lst))(1)-1)
     
    donnees_uncal = donnees
    
    print, "***********************************************"
    print, "Scan # ", scan_list(i)
    print, "PWV (mm)  = ", donnees.obslog.pwv
    pwv(i) = donnees.obslog.pwv
        
    if keyword_set(tau) and donnees.unit eq 'pW' then begin
    	amass = 1./sin(donnees.elevatio*!pi/180.)
;	tau_eff = tau
	tau_eff = tau*pwv(i)/pwv_mean
	print, "Correcting mean opacity by a factor :  ", pwv(i)/pwv_mean
	donnees.cube = donnees.cube*exp(tau_eff*amass)/Conv		; Calibration (extinction correction + pW --> Jy conversion)
          flat_cube = donnees.cube*0.
	  for kk=0, (size(donnees.cube))(3)-1 do flat_cube(*,*,kk) = flat
	donnees.unit = 'Jy/beam'	  
	donnees.cube = donnees.cube/flat_cube		; Flat field correction
	donnees.unit = 'Jy/beam (flat field corr.)'
;	donnees.unit = 'pW (ext. corr./flat field corr.)' 	
	print, " Unit: ",  donnees.unit
    endif
    
    if keyword_set(kill50Hz) then begin
    	print, "Killing 50 Hz noise component"
	kill_50Hz, donnees, donnees_50Hz, rms_noise, rms_noise_50Hz
;        filter_wobbler, donnees, donnees_filtrees, rms_noise, rms_noise_filtered
;	donnees_50Hz = donnees_filtrees
	donnees = donnees_50Hz
    endif

    ha = mean(donnees.lst)*15/3600. - donnees.lambda
    parangle = parallactic_angle(ha, donnees.beta, site_lat=donnees.sitelat)
    print, " Hour angle (hr) = ", ha/15.
    print, " Elevation (deg) = ", donnees.elevatio
    print, " Parallactic angle (deg) = ", parangle
    crota1 = parangle + donnees.elevatio
    print, " Rotation angle (deg) = ", crota1
    rot_angle(i) = crota1
    
    donnees.goodpix_ima = goodpix_ima
    donnees.cdelt1	= 5.85   ; 5.5   ; 4.5   ; 3.6  ;  3.65			; taille du pixel P-Artemis 450 microns a APEX en arcsec
    donnees.cdelt2	= 5.85   ; 5.5   ; 4.5   ; 3.6  ;  3.65
;    n_images = fix(donnees.scantime*donnees.freq)

    if keyword_set(decorrel) then begin
       cube = donnees.cube    
       test_decorrel, cube, mask, cube_decorrel, correl_matrice, a_matrice
       print, "Median value of correlation matrix : ", median(correl_matrice)
       donnees.cube = cube_decorrel
    endif   
     
    asso_images_2007, donnees, diff, error
     
    if i eq 0 then begin
	    t_cube_tot = transpose(diff.cube)
	    lst_tot = diff.lst
    endif else begin
    	    t_cube_tot =[t_cube_tot,transpose(diff.cube)]
            lst_tot = [lst_tot,diff.lst]
    endelse

        extract_beam, donnees.cube, donnees.chopbeam, cube_pos, cube_neg, count = nim_eff	
	if ((size(cube_pos))(0) eq 3 and (size(cube_neg))(0) eq 3) then begin
;		ima_neg = extract_ima_moy(cube_neg, rms_neg)
;		ima_pos = extract_ima_moy(cube_pos, rms_pos)
		ima_neg = extract_ima_moy(cube_neg, rms_neg, clip = 2.5)
		ima_pos = extract_ima_moy(cube_pos, rms_pos, clip = 2.5)		
		ima_brute = ima_pos-ima_neg
		rms = sqrt(rms_pos^2+rms_neg^2)
		print, i, "  Flux = ", ima_brute(7,7), "  +- ", stdev(ima_brute(index_mask))
		print, "Effective integration time (sec): ", nim_eff/donnees.freq
		total_int_time = total_int_time + nim_eff/donnees.freq
;		if (phase(i) gt 0) then begin
;			print, i, "  Max = ", max(ima_brute(index_good)), "  +- ", stdev(ima_brute(index_mask))
;		endif else begin
;			print, i, "  Min = ", min(ima_brute(index_good)), "  +- ", stdev(ima_brute(index_mask))		
;		endelse	
	endif else begin
		ima_brute = fltarr(16,16)
		rms = fltarr(16,16)
	endelse
	 
;	plot, donnees.chopbeam(0:100), ystyle=1, yrange=[-2,11], psym=10	
;
	cube_nodding(*,*,i) = ima_brute
;;;;    rot_angle(i) = crota1
;
;	ima_rot = rot(ima_brute,-crota1,1,7,7, /interp)
;	cube_nodding(*,*,i) = ima_rot
	rms_nodding(*,*,i) = rms
	if ((size(cube_pos))(0) ne 3 or (size(cube_neg))(0) ne 3) then begin
		nodding(i) = 0
	endif
	mean_lst(i) = mean(donnees.datapar.lst)
endfor

print, "Min rotation angle (deg) : ", min(rot_angle)
print, "Max rotation angle (deg) : ", max(rot_angle)
print, "Mean rotation angle (deg) : ", mean(rot_angle)
print, "Total effective integration time (sec) : ", total_int_time

total_obs_time = lst_end - lst_start
total_obs_time = dlst2dtloc(total_obs_time)

print, "Total telescope time (sec) : ", total_obs_time

cube_tot = transpose(t_cube_tot)
t_cube_tot = 0

donnees_tot = [{lst: lst_tot, cube: cube_tot}]

test_decorrel, cube_tot, mask, cube_tot_decorrel, correl_matrice, a_matrice
print, "Median value of correlation matrix on cube_tot : ", median(correl_matrice)

donnees_tot_decorrel = donnees_tot
donnees_tot_decorrel.cube = cube_tot_decorrel

;Calib_partemis = '/Users/artemis/Documents/Calib_partemis/
Calib_partemis = work_dir+'Calib_partemis/'
restore, Calib_partemis+'goodpix_ima_mars_20539_10may09.xdr', /verb

noise_fft_pro, donnees_tot, goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av
print, "Mean level over valid pixels : ", mean_level_av
print, "Mean rms noise over valid pixels : ", rms_noise_av

noise_fft_pro, donnees_tot_decorrel, goodpix_ima, mean_level_corr, mean_level_corr_av, rms_noise_corr, rms_noise_corr_av, cube_corr_pow_spec, pow_spec_corr_av
print, "Mean level over valid pixels after decorrelation : ", mean_level_corr_av
print, "Mean rms noise over valid pixels after decorrelation : ", rms_noise_corr_av
print, "Improvement factor due to decorrelation (rms_noise_av/rms_noise_corr_av): ", rms_noise_av/rms_noise_corr_av  

taille = size(cube_tot)
ndim = taille(3)
lst = donnees_tot.lst
lst0 = lst(0:ndim-2)
lst1 = lst(1:ndim-1)

dt = dlst2dtloc(median(lst1-lst0))
print, "Sampling time (sec) :", dt 

time = findgen(ndim)*dt		; timeline in sec
sfint = 1./(ndim*dt)		; smallest non-zero positive frequency in Hz
frequency = findgen(1+ndim/2)*sfint
freqmax = 1./(2.*dt)		; Nyquist frequency (highest frequency that
;					; can be sampled)

window, 20
window, 21
!p.multi=0
  
wset, 20
pow_max = max(pow_spec_av(0:ndim/2))
pow_min = min(pow_spec_av(0:ndim/2))
plot, frequency, pow_spec_av(0:ndim/2), xtitle="Frequency (Hz)", $
ytitle="Relative amplitude", title="Mean power spectrum over valid pixels", $
xrange=[0,freqmax], yrange=[0,pow_max/2.], /XSTYLE

plot, frequency, pow_spec_av(0:ndim/2),  xtitle="Frequency (Hz)", $
ytitle="Relative amplitude", title="Mean power spectrum over valid pixels", $
xrange=[0,freqmax], yrange=[0.,pow_max/2.], /XSTYLE

;wset, 21
;plot, lst_tot, cube_tot(7,7,*)

;STOP

nod =  n_elements(nodding)
signal_for_base = fltarr(nod/2)
index1 = 2*(indgen(nod/2))
index2 = 2*(indgen(nod/2))+1

count = 1
while count gt 0 do begin
        ind = where(nodding(index1)*nodding(index2) eq 1, count)
	if count gt 0 then begin
  		id = min(ind)
  		n_el = n_elements(index1)
  		index1 = [index1(0:id-1),index2(id:n_el-1)]
  		index2 = index1+1
	    indc = where(index2 ge nod, countc)
	if countc gt 0 then begin
	      idc = min(indc)
	      index2 = index2(0:idc-1)
	      index1 = index1(0:idc-1)
	    endif
	endif
endwhile

;count = 1
;while count gt 0 do begin & $
;        ind = where(nodding(index1)*nodding(index2) eq 1, count) & $
;	print, "count  = ", count & $
;	if count gt 0 then begin & $
;  		id = min(ind) & $
;  		n_el = n_elements(index1) & $
;  		index1 = [index1(0:id-1),index2(id:n_el-1)] & $
;  		index2 = index1+1 & $
;	    indc = where(index2 ge nod, countc) & $
;	    if countc gt 0 then begin & $
;	      idc = min(indc) & $
;	      print, "countc  = ", countc, "  idc  = ", idc & $
;	      index2 = index2(0:idc-1) & $
;	      index1 = index1(0:idc-1) & $
;	    endif & $
;	endif & $
;endwhile

n_el = n_elements(index1)

lst_for_base = fltarr(n_el)
lst_for_base = (mean_lst(index1)+mean_lst(index2))/2.
cube_nodding_alt2 = fltarr(16,16,n_el)
rms_nodding2 = fltarr(16,16,n_el)
;
image_nod = fltarr(16,16)
for j = 0, 15 do begin
  for i = 0, 15 do begin
    signal_for_base = (cube_nodding(i,j,index1)+cube_nodding(i,j,index2))/2.
    errors = sqrt(rms_nodding(i,j,index1)^2+rms_nodding(i,j,index2)^2)/2.
   if (n_elements(nodding) lt 8) then begin 
    coeff_base = poly_fit(lst_for_base,signal_for_base,1,yfit=base, yerror = base_error)
    base = coeff_base(0)+coeff_base(1)*mean_lst
   endif else begin     
    coeff_base = poly_fit(lst_for_base,signal_for_base,2,yfit=base, measure_errors = errors, yerror = base_error)
    base = coeff_base(0)+coeff_base(1)*mean_lst+coeff_base(2)*mean_lst^2
   endelse 
    cube_nodding(i,j,*) = cube_nodding(i,j,*)-base
    cube_nodding_alt(i,j,*) = cube_nodding(i,j,*)*nodding
    cube_nodding_alt2(i,j,*) = (cube_nodding(i,j,index1)-cube_nodding(i,j,index2))*nodding(index1)/2. 
    rms_nodding2(i,j,*) = errors  
;    image_nod(i,j) = mean(cube_nodding(i,j,*)*nodding)
;    rms(i,j) = sigma(cube_nodding(i,j,*)*nodding)/sqrt(n_elements(nodding))    
  endfor
endfor

;test_decorrel, cube_nodding_alt, mask, test_cube_nodding_alt, correl_matrice, a_matrice

image_nod = extract_ima_moy(cube_nodding_alt, rms_nod, cube_rms = rms_nodding, clip = 2.5)
;image_nod = extract_ima_moy(test_cube_nodding_alt, rms_nod, cube_rms = rms_nodding, clip = 2.5)
rms = rms_nod

image_nod2 = extract_ima_moy(cube_nodding_alt2, rms_nod2, cube_rms = rms_nodding2, clip = 2.5)

;stop
extract_beam, cube_nodding, nodding, cubea, cubeb
extract_beam, rms_nodding, nodding, rms_a, rms_b

test_cubeac  = cubea
test_cubebc  = cubeb

;test_decorrel, cubea, mask, test_cubeac, correl_matrice_a, a_matrice_a
;test_decorrel, cubeb, mask, test_cubebc, correl_matrice_b, a_matrice_b

imaac = extract_ima_moy(test_cubeac, rmsac, cube_rms = rms_a, clip = 2.5)
imabc = extract_ima_moy(test_cubebc, rmsbc, cube_rms = rms_b, clip = 2.5)

;sigmaac = stdev(imaac(11:14,3:7))
;sigmabc = stdev(imabc(11:14,3:7))
sigmaac = stdev(imaac(index_mask))
sigmabc = stdev(imabc(index_mask))

norm = (1./sigmaac^2)+(1./sigmabc^2)
image = (imaac/sigmaac^2-imabc/sigmabc^2)/norm

print, "Final image -  Max = ", max(image(index_good)), "  +- ", stdev(image(index_mask1))
;print, "Final image -  Max = ", max(image(index_good)), "  +- ", stdev(image(11:14,3:7))

;print, "Final image -  Flux(7,7) = ", mean(cube_nodding(7,7,*)*nodding), "  +- ", sigma(cube_nodding(7,7,*)*nodding)/sqrt(n_elements(nodding))
print, "Final image -  Flux(7,7) = ", image_nod(7,7), "  +- ", rms_nod(7,7)
print, "Final image -  Flux(8,7) = ", image_nod(8,7), "  +- ", rms_nod(8,7)
print, "*******************************************************************
print, "Final image -  Flux2(7,7) = ", image_nod2(7,7), "  +- ", rms_nod2(7,7)
print, "Final image -  Flux2(8,7) = ", image_nod2(8,7), "  +- ", rms_nod2(8,7)
print, "*******************************************************************
print, "rms_image = ", stdev(image(index_mask1))
print, "rms_image_nod = ", stdev(image_nod(index_mask1))
print, "rms_image_nod2 = ", stdev(image_nod2(index_mask1))

wset, 21
plot, cube_nodding(7,7,*)

atv, image*goodpix_ima

!p.multi=0
return
end

; wset, 0
; plot, cube_nodding(7,7,*), psym = 10, xrange=[-1, n_elements(nodding)]
; oplot, nodding*max(cube_nodding(7,7,*)), psym =2
; 
; plot, mean_lst, cube_nodding(7,7,*), psym = 10, yrange=[-1.2*max(cube_nodding(7,7,*)),+1.2*max(cube_nodding(7,7,*))]
; oplot, mean_lst, nodding*max(cube_nodding(7,7,*)), psym =2
;
; plot, mean_lst, pwv

;
;set_plot,'ps'
;device,file="wobbling_nodding_"+strcompress(string(scan_mars_22912(0)), /remove_all)+".ps"
;;
;plot, cube_nodding(7,7,*), xtitle='Scan number', ytitle='Flux density (Jy/beam)', $
;			title = 'Wobbling/Nodding Sequence' + string(scan_mars_22912(0))
;;
;device,/close
;set_plot,'x'
;
