pro noise_fft_cal, cal_number, goodpix_ima, noise_str, dir_dat=dir_dat, amb_cold = amb_cold

; Usage: 
; !path = '/Users/artemis/Desktop/apex_idl/apexpro:'+!path
; @obs1_config
; restore, Calib_partemis+'goodpix_ima_venus_23824_19may09.xdr', /verb   ;  --> restoring goodpix_ima
; restore, Calib_partemis+'flat_venus_23824_19may09.xdr', /verb   ;  --> restoring flat_field
;
; init_obs, scan_number= 24537, type = 'map', init_obs_24537_str
; goodpix_ima = init_obs_24537_str.goodpix_ima
; flat_field = init_obs_24537_str.flat_field 
;
; ima_amb_cold_24537 = fltarr(16,16)
; noise_fft_cal, 24537, goodpix_ima, cal_noise_str, amb_cold = ima_amb_cold_24537
;
; conv = init_obs_24537_str.conv		;  2.0e-4			; Mars/Uranus/Neptune May 2009
;
; conv = 2.66890e-4				; conv_mars_24822(7,7)
; restore, '/Users/pandre/kosma/artemis/Calib_partemis/flat_mars_24822_22may09.xdr', /verb		; --> restoring flat_field
; flat_field(0,15) = 1.
;
; for i = 0, 2 do print, "Power = ", cal_noise_str(i).mean_level_av,"  ", cal_noise_str(i).scanname, "   El = ", cal_noise_str(i).elevatio
; for i = 0, 2 do print, "Median NEFD = ", median((exp(0.8/sin(50*!pi/180.))/sqrt(40.))*cal_noise_str(i).rms_noise_corr/flat_field/conv)
; for i = 0, 2 do print, "Min NEFD = ", min((exp(0.8/sin(50*!pi/180.))/sqrt(40.))*cal_noise_str(i).rms_noise_corr/flat_field/conv)
; atv, (exp(0.8/sin(50*!pi/180.))/sqrt(40.))*cal_noise_str(1).rms_noise_corr/flat_field/conv
;
; noise_fft_skydip, 24538, sky_noise_str
; for i = 0, 5 do print, "Power = ", sky_noise_str(i).mean_level_av,"  ", sky_noise_str(i).scanname, "   El = ", sky_noise_str(i).elevatio
; for i = 0, 5 do print, "Median NEFD = ", median((exp(0.8/sin(50*!pi/180.))/sqrt(40.))*sky_noise_str(i).rms_noise_corr/flat_field/conv)
; for i = 0, 5 do print, "Min NEFD = ", min((exp(0.8/sin(50*!pi/180.))/sqrt(40.))*sky_noise_str(i).rms_noise_corr/flat_field/conv)
;
; Inputs :  cal_number (log)  goodpix_ima(16,16)
; Ouput:   noise_str

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	NOISE_FFT_CAL, cal_number, goodpix_ima, noise_str, amb_cold = amb_cold
	return
endif	



COMMON obs1_configb, work_dir, project_name



init_obs, scan_number=cal_number, type='map', init_obs_str

bidon = fltarr(16,16)
bidon(9:13,8:13) = 1.
mask = bidon

	
for i=0, n_elements(init_obs_str.subscan_liste)-1 do begin

 if keyword_set(nopowermap) then begin         ; do not calibrate data from V to pW

  donnees = read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), filetype, $
  calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima, /nopowermap)

 endif else begin	                          ; calibrate data from V to pW or restore existing calibrated data

  donnees = read_apex(init_obs_str.scan_number, init_obs_str.subscan_liste(i), filetype, $
  calibration_camera=init_obs_str.calibration_camera, goodpix_ima=init_obs_str.goodpix_ima)

 endelse

  print, "Subscan : ", init_obs_str.subscan_name(i)
  print, "Elevation (deg) : ", donnees.elevatio

; Elimination de la COMPOSANTE DE BRUIT a 50Hz (<--> 10 Hz)
;
; goodpix_ima = intarr(16,16)+1
  kill_50Hz, donnees, donnees_50Hz, rms_noise, rms_noise_50Hz

;  donnees = donnees_50Hz

  n_images = fix(10*donnees.freq)		; 2007 calibrations

  command = donnees.obslog.command                                ;  recuperation de donnees additionnelles dans les OBSLOG
	if strpos(command,'time=') ne -1L then begin			; 'angle=' is contained in command
	  command = strmid(command,strpos(command,'time='))
	endif else begin
	  command = -1
	endelse  
	if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
	 	time = (strsplit(command,',',/extract))(0)
		time =  float(strmid(time,strpos(time,'=')+1))	; integration time (sec) of calibration subscans
		n_images = fix(time*donnees.freq)
	endif

  cube = donnees.cube(*,*,0:n_images-1)	
  test_decorrel, cube, mask, cube_corr, correl_matrice, a_matrice
;
  taille = size(cube)
  ndim = taille(3)
  lst = donnees.lst
  lst0 = lst(0:ndim-2)
  lst1 = lst(1:ndim-1)
  dt = dlst2dtloc(median(lst1-lst0))
  print, "Sampling time (sec) :", dt 
;  
  time = findgen(ndim)*dt		; timeline in sec
  sfint = 1./(ndim*dt)		; smallest non-zero positive frequency in Hz
  frequency = findgen(1+ndim/2)*sfint
  freqmax = 1./(2.*dt)		; Nyquist frequency (highest frequency that
;					; can be sampled)  

;  noise_fft_pro, cube, goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av
  noise_fft_pro, donnees, goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av, ndim=ndim
  print, "Mean level over valid pixels : ", mean_level_av
  print, "Mean rms noise over valid pixels : ", rms_noise_av

  cube_pow_spec = cube_pow_spec(*,*,0:n_images-1)
  pow_spec_av = pow_spec_av(0:n_images-1)

  wset, 20
  pow_max = max(pow_spec_av(0:ndim/2))
  plot_oo, frequency, pow_spec_av(0:ndim/2), xtitle="Frequency (Hz)", $
  ytitle="Relative amplitude", title="Mean power spectrum over valid pixels for scan "+init_obs_str.subscan_name(i), $
  xrange=[0.1,freqmax], yrange=[pow_max*1.e-3,pow_max/2.], /XSTYLE  

  donnees_corr = donnees
  donnees_corr.cube = cube_corr

;  noise_fft_pro, cube_corr, goodpix_ima, mean_level_corr, mean_level_corr_av, rms_noise_corr, rms_noise_corr_av, cube_corr_pow_spec, pow_spec_corr_av
  noise_fft_pro, donnees_corr, goodpix_ima, mean_level_corr, mean_level_corr_av, rms_noise_corr, rms_noise_corr_av, cube_corr_pow_spec, pow_spec_corr_av, ndim = ndim
  print, "Mean level over valid pixels after decorrelation : ", mean_level_corr_av
  print, "Mean rms noise over valid pixels after decorrelation : ", rms_noise_corr_av

  cube_corr_pow_spec = cube_corr_pow_spec(*,*,0:n_images-1)
  pow_spec_corr_av = pow_spec_corr_av(0:n_images-1)

  wset, 21
  pow_max = max(pow_spec_corr_av(0:ndim/2))
  plot_oo, frequency, pow_spec_corr_av(0:ndim/2), xtitle="Frequency (Hz)", $
  ytitle="Relative amplitude", title="Mean power spectrum over valid pixels after decorrelation for scan "+init_obs_str.subscan_name(i), $
  xrange=[0.1,freqmax], yrange=[pow_max*1.e-3,pow_max/2.], /XSTYLE

  champ  = [{scanname : init_obs_str.subscan_name(i), cube : cube, cube_corr : cube_corr, mean_level : mean_level, mean_level_av : mean_level_av, $
			rms_noise : rms_noise, rms_noise_av : rms_noise_av, elevatio : donnees.elevatio,  $
                        cube_pow_spec : cube_pow_spec , pow_spec_av : pow_spec_av, rms_noise_corr : rms_noise_corr, $
			rms_noise_corr_av : rms_noise_corr_av , cube_corr_pow_spec : cube_corr_pow_spec, $
			pow_spec_corr_av : pow_spec_corr_av }]
  
  IF i eq 0 THEN BEGIN

  	noise_str = [champ]

  ENDIF ELSE BEGIN	; i > 0
  
  	noise_str = [noise_str, champ]

  ENDELSE

endfor


if (n_elements(init_obs_str.subscan_liste) eq 3) then begin

	cube_amb  = noise_str(1).cube
	cube_cold = noise_str(2).cube
	
	ima_amb  = extract_ima_moy(cube_amb, rms_amb)
	ima_cold = extract_ima_moy(cube_cold, rms_cold)
	
	amb_cold = ima_amb - ima_cold
;	atv, amb_cold
	
;	if keyword_set(amb_cold) then begin
;	
;	endif	

endif else begin

	print, "Not a cal cold : Less than 3 subscans  ", n_elements(init_obs_str.subscan_liste), "  subscans " 

endelse

return
end


