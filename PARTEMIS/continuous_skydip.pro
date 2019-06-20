
pro continuous_skydip, skydip_number, nopowermap=nopowermap


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	NOISE_FFT_SKYDIP, Skydip_number, Noise_str
	return
endif	



COMMON obs1_configb, work_dir, project_name



init_obs, scan_number=skydip_number, type='map', init_obs_str



window, 20
window, 21

bidon = fltarr(16,16)
bidon(9:13,8:13) = 1.
mask = bidon

if n_elements(init_obs_str.subscan_liste) ne 1 then begin
  print, "This cannot be a continuous skydip. "
  print, "Number of susbscans: ", n_elements(init_obs_str.subscan_liste)
endif
	

 if keyword_set(nopowermap) then begin         ; do not calibrate data from V to pW

  donnees = read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(0), filetype, $
  calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima, /nopowermap)

 endif else begin	                          ; calibrate data from V to pW or restore existing calibrated data

  donnees = read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(0), filetype, $
  calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)

 endelse

  print, "Subscan : ", init_obs_str.subscan_name(0)
;  print, "Elevation (deg) : ", donnees.elevatio
  
  ind = where(donnees.datapar.elevatio ne -999., n_points)
  
  if n_points eq 0 then begin
     print, "Error: no valid data"
     return
  endif
  
  
  elev = (donnees.datapar.elevatio)(ind)

  amass = 1./sin(elev*!pi/180.)
  
  cube = donnees.cube
  
  goodpix_ima = init_obs_str.goodpix_ima
  
  id = where(goodpix_ima eq 1)

  int = fltarr(n_points)
  rms_int = fltarr(n_points)
  
  for k = 0, n_points-1 do begin
    data_mat = cube(*,*,k)*goodpix_ima
    data_vec = data_mat(id)
    int(k) = mean(data_vec)
    rms_int(k) = stdev(data_vec)
  endfor
  
  loadct, 15
  wset, 21
;  plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [min(int),max(int)]
  plot, amass, int, psym = 2, /ynozero, xrange= [0.95,2.55], yrange = [min(int),max(int)]

chi = 0.
;apar = [0.5, 14.6, 3.3]
;apar = [0.65, 14.5, 3.0]
;apar = [1.5, 16.5, 6.8]
;apar = [0.82, 18.25, 5.0]
apar = [0.93, 16.87, 6.46]
rms = apar*0.
;
;.run skydip_fun
skydip_fit = LMFIT(amass,int,APAR,SIGMA=RMS,CHISQ=chi,FUNCTION_NAME='skydip_fun',/double)
;
oplot, amass, skydip_fit, psym=-1, color=130
;
print, " Tau (450 microns) at zenith = ", apar(0), " +- ", rms(0)
print, " PWV (mm) = ", donnees.obslog.pwv
;
print, apar

;
wset, 20
amass_ext = [0.,0.5,[amass]]
Result = skydip_fun(amass_ext,apar)
int_fit_ext = result(*,0)

plot, amass_ext, int_fit_ext, psym = 1, /ynozero, xrange= [-0.05,3.05], yrange = [min(int_fit_ext),max(int_fit_ext)]

fitapar = [1, 0, 0]		; fitting only tau
; 
skydip_fit = LMFIT(amass,int,APAR,fita=fitapar,SIGMA=RMS,CHISQ=chi,FUNCTION_NAME='skydip_fun',/double)

;
;oplot, amass, skydip_fit, psym=-1, color=130
;
print, " Tau (450 microns) at zenith = ", apar(0), " +- ", rms(0), "... when fitting only Tau and fixing other 2 parameters"
;
print, apar

return
end
