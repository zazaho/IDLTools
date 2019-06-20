pro traite_nodding_apex_derot,scan_list,image,rms,cube_nodding,nodding,tau=tau,mean_lst=mean_lst,kill50Hz=kill50Hz,decorrel=decorrel,image_nod=image_nod,pwv=pwv,scan_ini=scan_ini,nofilt=nofilt

; Usage : scan_hd181327_24959 = indgen(48)+24959
;	  traite_nodding_apex_derot, scan_hd181327_24959, hd181327_24959, rms, cube_nodding, nodding, tau = 0.49, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  traite_nodding_apex_derot, scan_hd181327_24959(1:*), hd181327_24959_1, rms, cube_nodding, nodding, tau = 0.49, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_24950 = [24950L, 24951L, 24952L, 24953L]
;         traite_nodding_apex_derot, scan_uranus_24950, uranus_24950, rms, cube_nodding, nodding, tau = 0.49, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_uranus_41243 = [41243L, 41244L, 41245L, 41246L]
;	  traite_nodding_apex_derot, scan_uranus_41243, uranus_41243, rms, cube_nodding, nodding, tau = 0.78, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv, /kill50Hz
;
;	  scan_mars_20540 = [20540L, 20541L, 20542L, 20543L, 20544L, 20545L, 20546L, 20547L, 20548L, 20549L, 20550L, 20551L]
;         traite_nodding_apex_derot, scan_mars_20540, mars_20540, rms, cube_nodding, nodding, tau = 0.9, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;	  scan_mars_22912 = [22912L, 22913L, 22914L, 22915L, 22916L, 22917L, 22918L, 22919L,22920L, 22921L, 22922L, 22923L, 22924L, 22926L, 22927L]
;         traite_nodding_apex_derot, scan_mars_22912, mars_22912, rms, cube_nodding, nodding, tau = 0.79, mean_lst = mean_lst, image_nod=image_nod, pwv=pwv
;
;
;	  traite_nodding_apex_derot, scan_saturn_49657, saturn_image_49657, rms, cube_nodding, nodding, tau = 0.87, mean_lst = mean_lst, /kill50Hz, image_nod=image_nod, /pwv
;
;	  traite_nodding_apex_derot, scan_hd_49703, hd_image_49703, rms, cube_nodding, nodding, tau = 0.85, mean_lst = mean_lst, /kill50Hz, /decorrel, image_nod=image_nod, /pwv
;	  gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/hd97048_49703_nod.gdf", image_nod, /write
;
;	  traite_nodding_apex_derot, scan_hd100546_50709, hd100546_image_50709, rms, cube_nodding, nodding, tau = 1.3, mean_lst = mean_lst, /kill50Hz, /decorrel, image_nod=image_nod, /pwv
;	  gildas, "/Users/artemis/Documents/apexdata/map_otf_fits/hd100546_50709_nod.gdf", image_nod, /write
;
;	  Disques = '/Users/artemis/Documents/apexdata/Disques/'
;	  restore, Disques+'betapic_50175.xdr', /verb
;	  scan_betapic_50175 = scanbeta
;	  traite_nodding_apex_derot, scan_betapic_50175, betapic_image_50175, rms, cube_nodding, nodding, tau = 0.65, mean_lst = mean_lst, /kill50Hz, image_nod = image_nod, /pwv
;
;	  traite_nodding_apex_derot, scan_betapic_49443, betapic_image_49443, rms, cube_nodding, nodding, tau = 0.9, mean_lst = mean_lst, /kill50Hz, image_nod = image_nod, /pwv
;
; Inputs   : scan_nod_list (+ dir_log, dir_dat if set)
; Outputs  : image, cube_nodding, nodding
;
;.r UTILITIES.PRO

COMMON obs1_configb, work_dir, project_name, calibration_table     ; chargement des common variables depuis obs1_config

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	TRAITE_NODDING_APEX, nod_list, image, cube_nodding, nodding, '
	print, '		dir_dat=dir_dat, nopowermap=nopowermap, tau=tau, mean_lst=mean_lst'
	return
endif

;if keyword_set(scan_ini) then begin
;  order = scan_list-scan_ini
;endif else begin
;  order = scan_list-scan_list(0)
;endelse

;
;order = order - fix(order/4)*4
;phase = order*0
;phase(where(order eq 0 or order eq 3)) = 1
;phase(where(order eq 1 or order eq 2)) = -1

phase = intarr(n_elements(scan_list))

;Calib_partemis = '/Users/artemis/Desktop/Calib_partemis/'
;restore, Calib_partemis+'goodpix_ima_mars_28nov07.xdr', /verb		;  <-- goodpix_ima_50010
;restore, Calib_partemis+'flat_mars_50010_28nov07.xdr', /verb		;  <-- flat_mars_50010
;goodpix_ima = goodpix_ima_50010
;flat = flat_mars_50010
;
;restore, Calib_partemis+'goodpix_ima_mars_48812_50010.xdr', /verb	;  <-- goodpix_ima
;restore, Calib_partemis+'flat_mars_48812_50010.xdr', /verb		;  <-- flat_mars
;flat = flat_mars

Calib_partemis = work_dir+'Calib_partemis/'

init_obs, scan_number=scan_list(0), type='map', init_obs_str

goodpix_ima = init_obs_str.goodpix_ima
flat = init_obs_str.flat_field

n_file = n_elements(scan_list)

echant = 3
npixreb = 24*echant 

goodpix_ima_ext = intarr(24,24)
goodpix_ima_ext(4:19,4:19) = goodpix_ima
goodpix_ima_res = rebin(goodpix_ima_ext, npixreb, npixreb, /sample)

masque = intarr(npixreb,npixreb)


nxchan = indgen(npixreb)#(transpose(intarr(npixreb))+1)
nychan = (intarr(npixreb)+1)#transpose(indgen(npixreb))

cubetot = dblarr(npixreb,npixreb,2)
cube_nodding = dblarr(npixreb,npixreb,n_file)
cube_nodding_alt = dblarr(npixreb,npixreb,n_file)
rms_nodding = dblarr(npixreb,npixreb,n_file)+1.
;nodding=intarr(n_file)
nodding = phase
mean_lst = fltarr(n_file)
pwv = fltarr(n_file)

!p.multi=[0,5,6]

bidon = intarr(16,16)
bidon1 = intarr(24,24)
bidon1(9+4:13+4,10+4:12+4) = 1
bidon(5:10,2:4) = 1
mask = bidon
mask1 = bidon1

index_good = where(goodpix_ima eq 1)
index_mask = where(mask eq 1)
;index_mask1 = where(mask1 eq 1)

Conv = 1.
if ((scan_list(0) ge 47513) and (scan_list(0) le 47547)) then begin
  Conv = 1.287e-4				; pW ff corr/Jy		16 Nov
endif
if (scan_list(0) ge 47913) then begin
   Conv = 2.69d-4				; pW ff corr/Jy		> 18 Nov     Mars after filtering to 2*6 = 12" effective resolution
;   Conv = 1.88d-4				; pW ff corr/Jy		> 18 Nov     Saturn after filtering to 2*6 = 12" effective resolution
;  Conv = 2.56d-4				; pW ff corr/Jy		> 18 Nov     Saturn before filtering
;;;  Conv = 3.65e-4							; pW ff corr/Jy	   Mars beam map # 50010	28 Nov  
endif

Conv = init_obs_str.conv
;
print, "Conv = ", Conv

;

if keyword_set(pwv) then begin
  for i=0, n_file-1 do begin    
    donnees = read_apex(scan_list(i), '1', filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)
    pwv(i) = donnees.obslog.pwv
  endfor
  pwv_mean = mean(pwv)
  print, "Mean PWV (mm)  = ", pwv_mean 
endif

rot_angle = fltarr(n_file)

for i=0, n_file-1 do begin
    
    donnees = read_apex(scan_list(i), '1', filetype, calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)
    
    phase(i) = fix(sgn(median(donnees.datapar.longoff)))
    nodding(i) = phase(i)
    
    donnees_uncal = donnees

    print, "***********************************************"
    print, "Scan # ", scan_list(i)
    print, "PWV (mm)  = ", donnees.obslog.pwv
    pwv(i) = donnees.obslog.pwv
        
    if keyword_set(tau) and donnees.unit eq 'pW' then begin
    	amass = 1.d0/sin(donnees.elevatio*!pi/180.d0)
	tau_eff = tau
	if keyword_set(pwv) then begin
	   tau_eff = tau*pwv(i)/pwv_mean
	   print, "Correcting mean opacity by a factor :  ", pwv(i)/pwv_mean
	endif   
	donnees.cube = donnees.cube*exp(tau_eff*amass)/Conv		    ; Calibration (extinction correction + pW --> Jy conversion)
          flat_cube = donnees.cube*0.d0
	  for kk=0, (size(donnees.cube))(3)-1 do flat_cube(*,*,kk) = flat
;	donnees.unit = 'Jy/beam'	  
	donnees.cube = donnees.cube/flat_cube		; Flat field correction
	donnees.unit = 'Jy/beam (flat field corr.)'
;	donnees.unit = 'pW (ext. corr./flat field corr.)' 	
	print, " Unit: ",  donnees.unit
    endif
    
    if keyword_set(kill50Hz) then begin
    	print, "Killing 50 Hz noise component"
	kill_50Hz, donnees, donnees_50Hz, rms_noise, rms_noise_50Hz
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
    donnees.cdelt1	= 6.2    ; 5.85   ; 5.5   ; 4.5   ; 3.6  ;  3.65			; taille du pixel P-Artemis 450 microns a APEX en arcsec
    donnees.cdelt2	= 6.2    ; 5.85   ; 5.5   ; 4.5   ; 3.6  ;  3.65
;    n_images = fix(donnees.scantime*donnees.freq)

    if keyword_set(decorrel) then begin
      cube = donnees.cube
      test_decorrel, cube, mask, cube_decorrel, correl_matrice, a_matrice
      print, "Median value of correlation matrix : ", median(correl_matrice)      
      donnees.cube = cube_decorrel
    endif

        extract_beam, donnees.cube, donnees.chopbeam, cube_pos, cube_neg
	if ((size(cube_pos))(0) eq 3 and (size(cube_neg))(0) eq 3) then begin
		ima_neg = extract_ima_moy(cube_neg, rms_neg)
		ima_pos = extract_ima_moy(cube_pos, rms_pos)
;		ima_neg = extract_ima_moy(cube_neg, rms_neg, clip = 2.5)
;		ima_pos = extract_ima_moy(cube_pos, rms_pos, clip = 2.5)		
		ima_brute = ima_pos-ima_neg
		rms = sqrt(rms_pos^2+rms_neg^2)
		print, i, "  Flux = ", ima_brute(7,7), "  +- ", stdev(ima_brute(index_mask))
;		if (phase(i) gt 0) then begin
;			print, i, "  Max = ", max(ima_brute(index_good)), "  +- ", stdev(ima_brute(index_mask))
;		endif else begin
;			print, i, "  Min = ", min(ima_brute(index_good)), "  +- ", stdev(ima_brute(index_mask))		
;		endelse	
	endif else begin
		ima_brute = fltarr(16,16)
		rms = fltarr(16,16)
	endelse
;
	ima_brute_ext = dblarr(24,24)
	ima_brute_ext(4:19,4:19) = ima_brute
	ima_brute_ext_res = rebin(ima_brute_ext, npixreb, npixreb, /sample)
	ima_brute_ext_res = ima_brute_ext_res*goodpix_ima_res
;;;;	rms_ext = dblarr(24,24)+1.d-8
;	rms_ext = dblarr(24,24)+1.d3
	rms_ext = dblarr(24,24)
	rms_ext(4:19,4:19) = rms
	rms_ext_res = rebin(rms_ext, npixreb, npixreb, /sample)
	rms_ext_res = rms_ext_res*goodpix_ima_res
	ind = where(rms_ext_res eq 0.)
	rms_ext_res(ind) = 1.d3
;
	filter_image, ima_brute_ext_res, ima_brute_ext_res_filtree	
	ima = ima_brute_ext_res_filtree
;	ima = ima*goodpix_ima_res
;	
;	filter_image, rms_ext_res, rms_ext_res_filtree
;	rms_ext_res = rms_ext_res_filtree
	rms = rms_ext_res
;
	elev  = donnees.elevatio					; elevation in deg
	throw = 2.*donnees.wobthrow*3600.				; throw in arcsec
	pix = donnees.cdelt1/echant					; pixel in arcsec after rebinning
	del = 0.
	daz = -throw*phase(i)
	rot_dazdel2dxdy, daz, del, elev, dx, dy				; dx, dy = offsets of "negative beam" in arcsec and array coordinates
	dx_p = nint(dx/pix)
	dy_p = nint(dy/pix)						; dx_p, dy_p = offsets of "negative beam" in array coordinates and units of (rebinned) pixels
	masque = masque*0
	ind = where((nxchan+dx_p-4*echant)*(nxchan+dx_p-(19+1)*echant+1) le 0 and (nychan+dy_p-4*echant)*(nychan+dy_p-(19+1)*echant+1) le 0)
	masque(ind) = 1
	ind = where((nxchan-dx_p-4*echant)*(nxchan-dx_p-(19+1)*echant+1) gt 0 or (nychan-dy_p-4*echant)*(nychan-dy_p-(19+1)*echant+1) gt 0)
	masque(ind) = 0
;
	ind = where(masque eq 1)
;	ima(ind) = ima(ind)/rms(ind)^2 - ima(nxchan(ind)+dx_p,nychan(ind)+dy_p)/rms(nxchan(ind)+dx_p,nychan(ind)+dy_p)^2
;	ima(ind) = ima(ind)/(1./rms(ind)^2 + 1./rms(nxchan(ind)+dx_p,nychan(ind)+dy_p)^2)
	
;	rms(ind) = 1./sqrt(1./rms(ind)^2 + 1./rms(nxchan(ind)+dx_p,nychan(ind)+dy_p)^2)
;	rms_ext_res = rms
;	
	ima_brute_ext_res = ima
	ima_rot = rot(ima_brute_ext_res,-crota1,1,11*echant+echant/2,11*echant+echant/2, /interp)
;;;	filter_image, ima_rot, ima_rot_filtree
;;;	ima_rot = ima_rot_filtree
	rms_rot = rot(rms_ext_res,-crota1,1,11*echant+echant/2,11*echant+echant/2, /interp)
	cube_nodding(*,*,i) = ima_rot
	rms_nodding(*,*,i) = rms_rot
	if ((size(cube_pos))(0) ne 3 or (size(cube_neg))(0) ne 3) then begin
		nodding(i) = 0
	endif
	mean_lst(i) = mean(donnees.datapar.lst)
endfor

print, "Min rotation angle (deg) : ", min(rot_angle)
print, "Max rotation angle (deg) : ", max(rot_angle)
print, "Mean rotation angle (deg) : ", mean(rot_angle)

mask1_res = rebin(mask1, npixreb, npixreb, /sample)

;mask1_rot = rot(mask1,-mean(rot_angle),1,11,11, /interp)
mask1_rot = rot(mask1_res,-mean(rot_angle),1,11*echant+echant/2,11*echant+echant/2, /interp)
index_mask1 = where(mask1_rot gt 0.45)

array = intarr(16,16)+1
array_ext = intarr(24,24)
array_ext(4:19,4:19) = array
array_res = rebin(array_ext, npixreb, npixreb, /sample)
;array_rot = rot(array_res,-mean(rot_angle),1,11*echant+echant/2,11*echant+echant/2, /interp)
array_rot = rot(goodpix_ima_res,-mean(rot_angle),1,11*echant+echant/2,11*echant+echant/2, /interp)

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
		if id gt 0 then begin
  		   index1 = [index1(0:id-1),index2(id:n_el-1)]
		endif else begin
		   index1 = index2(id:n_el-1)
		endelse   
  		index2 = index1+1
	    indc = where(index2 ge nod, countc)
	if countc gt 0 then begin
	      idc = min(indc)
	      index2 = index2(0:idc-1)
	      index1 = index1(0:idc-1)
	    endif
	endif
endwhile

n_el = n_elements(index1)

lst_for_base = fltarr(n_el)
lst_for_base = (mean_lst(index1)+mean_lst(index2))/2.
cube_nodding_alt2 = dblarr(npixreb,npixreb,n_el)
rms_nodding2 = dblarr(npixreb,npixreb,n_el)
;
;image_nod = dblrarr(npixreb,npixreb)
status_arr = intarr(npixreb,npixreb)

;degree = 2*n_el/8
degree = 2
if n_el lt 8 then degree = 1
if n_el lt 4 then degree = 0
print, "degree of baselines : ", degree

for j = 0, npixreb-1 do begin
  for i = 0, npixreb-1 do begin
   test = 1
   if (n_el gt 1) then begin
     test = stdev(cube_nodding(i,j,index1))+stdev(cube_nodding(i,j,index2))
   endif
   if (test gt 0.) then begin
    signal_for_base = (cube_nodding(i,j,index1)+cube_nodding(i,j,index2))/2.
    errors = sqrt(rms_nodding(i,j,index1)^2+rms_nodding(i,j,index2)^2)/2.       
    status = 0
;    coeff_base = poly_fit(lst_for_base,signal_for_base,2,/double, yfit=base, measure_errors = errors, yerror = base_error, status=status)
;    status_arr(i,j) = status
;    base = coeff_base(0)+coeff_base(1)*mean_lst+coeff_base(2)*mean_lst^2
;
    coeff_base = poly_fit(lst_for_base,signal_for_base,degree,/double, yfit=base, measure_errors = errors, yerror = base_error, status=status)
    status_arr(i,j) = status
    base = mean_lst*0.
    for h = 0, degree do base = base+coeff_base(h)*mean_lst^h
;        
    cube_nodding(i,j,*) = cube_nodding(i,j,*)-base  
    rms_nodding2(i,j,*) = errors   
   endif
    cube_nodding_alt(i,j,*) = cube_nodding(i,j,*)*nodding
    cube_nodding_alt2(i,j,*) = (cube_nodding(i,j,index1)-cube_nodding(i,j,index2))*nodding(index1)/2.   
  endfor
endfor

;;;;test_decorrel, cube_nodding_alt, mask, test_cube_nodding_alt, correl_matrice, a_matrice

;image_nod = extract_ima_moy(cube_nodding_alt, rms_nod, cube_rms = rms_nodding)
image_nod = extract_ima_moy(cube_nodding_alt, rms_nod, cube_rms = rms_nodding, clip = 2.5)
;;;;;image_nod = extract_ima_moy(test_cube_nodding_alt, rms_nod, cube_rms = rms_nodding, clip = 2.5)
;

if not keyword_set(nofilt) then begin
   filter_image, image_nod, image_nod_filtree
   image_nod = image_nod_filtree
   filter_image, rms_nod, rms_nod_filtree
   rms_nod = rms_nod_filtree  
;
   rms = rms_nod*array_rot
   ind = where(rms eq 0.,count)
   if count gt 0 then rms(ind) = 1.e-8      
endif else begin
   rms = rms_nod*array_rot
   ind = where(rms eq 0.,count)
   if count gt 0 then rms(ind) = 1.d3
endelse
image_nod = image_nod*array_rot

image_nod2 = image_nod*0.
rms_nod2 = rms_nod*0.
;image_nod2 = extract_ima_moy(cube_nodding_alt2, rms_nod2, cube_rms = rms_nodding2)
;image_nod2 = extract_ima_moy(cube_nodding_alt2, rms_nod2, cube_rms = rms_nodding2, clip = 2.5)

if not keyword_set(nofilt) then begin
   filter_image, image_nod2, image_nod2_filtree
   image_nod2 = image_nod2_filtree
  ;filter_image, rms_nod2, rms_nod2_filtree
  ;rms_nod2 = rms_nod2_filtree
endif

;stop

extract_beam, cube_nodding, nodding, cubea, cubeb
extract_beam, rms_nodding, nodding, rms_a, rms_b

test_cubeac  = cubea
test_cubebc  = cubeb

;stop

;test_decorrel, cubea, mask, test_cubeac, correl_matrice_a, a_matrice_a
;test_decorrel, cubeb, mask, test_cubebc, correl_matrice_b, a_matrice_b

;imaac = extract_ima_moy(test_cubeac, rmsac, cube_rms = rms_a)
;imabc = extract_ima_moy(test_cubebc, rmsbc, cube_rms = rms_b)
;
imaac = extract_ima_moy(test_cubeac, rmsac, cube_rms = rms_a, clip = 2.5)
imabc = extract_ima_moy(test_cubebc, rmsbc, cube_rms = rms_b, clip = 2.5)

;sigmaac = stdev(imaac(11:14,3:7))
;sigmabc = stdev(imabc(11:14,3:7))
sigmaac = stdev(imaac(index_mask1))
sigmabc = stdev(imabc(index_mask1))

norm = (1./sigmaac^2)+(1./sigmabc^2)
image = (imaac/sigmaac^2-imabc/sigmabc^2)/norm

if not keyword_set(nofilt) then begin
   filter_image, image, image_filtree
   image = image_filtree
endif   

print, "RA-DEC image -  Max = ", max(image), "  +- ", stdev(image(index_mask1))
;print, "RA-DEC image -  Max = ", max(image(index_good)), "  +- ", stdev(image(11:14,3:7))

;print, "RA-DEC image -  Flux(7,7) = ", mean(cube_nodding(7,7,*)*nodding), "  +- ", sigma(cube_nodding(7,7,*)*nodding)/sqrt(n_elements(nodding))
print, "RA-DEC image -  Flux(35,35) = ", image_nod(35,35), "  +- ", rms_nod(35,35)
print, "RA-DEC image -  Flux(34,40) = ", image_nod(34,13*echant+echant/2), "  +- ", rms_nod(34,13*echant+echant/2)
print, "*******************************************************************
print, "RA-DEC image -  Flux2(35,35) = ", image_nod2(35,35), "  +- ", rms_nod2(35,35)
print, "RA-DEC image -  Flux2(34,40) = ", image_nod2(34,13*echant+echant/2), "  +- ", rms_nod2(34,13*echant+echant/2)
print, "*******************************************************************
print, "rms_image = ", stdev(image(index_mask1))
print, "rms_image_nod = ", stdev(image_nod(index_mask1))
print, "rms_image_nod2 = ", stdev(image_nod2(index_mask1))

image = image*array_rot
atv, image

;image_nod = image_nod*array_rot

ind = where(status_arr eq 2. and array_rot eq 1,count)
if count gt 0 then print, "POLY_FIT: Baseline problem"

;;;;;stop

!p.multi=0
return
end

; wset, 0
; plot, cube_nodding(7,7,*), psym = 10, xrange=[-1, n_elements(nodding)]
; oplot, nodding*max(cube_nodding(7,7,*)), psym =2
; 
; plot, mean_lst, cube_nodding(7,7,*), psym = 10, yrange=[-1.2*max(cube_nodding(7,7,*)),+1.2*max(cube_nodding(7,7,*))]
; oplot, mean_lst, nodding*max(cube_nodding(7,7,*)), psym =2


;test = fltarr(24,24)
;test(where(image_nod gt -1000.)) = image_nod(where(image_nod gt -1000.))
