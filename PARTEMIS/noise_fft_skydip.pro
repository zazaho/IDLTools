
;+
; NAME:
;	NOISE_FFT_SKYDIP
;
; PURPOSE:
;
;	Calculate the optical opacity tau from a skydip observation		
;
; CALLING SEQUENCE:
;	
;	NOISE_FFT_SKYDIP, SKYDIP_NUMBER, NOISE_STR
;
; INPUTS:
;
;	SKYDIP_NUMBER:	Observation number.
;			
; OUTPUTS:
;
;	NOISE_STR:	Output noise structure.
;	
; EXAMPLE:	
;
;		NOISE_FFT_SKYDIP, 47519, sky_noise_str
;
; MODIFICATION HISTORY:
; 				
;			
;-


pro noise_fft_skydip, skydip_number, noise_str, nopowermap=nopowermap


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	NOISE_FFT_SKYDIP, Skydip_number, Noise_str
	return
endif	



COMMON obs1_configb, work_dir, project_name



init_obs, scan_number=skydip_number, type='map', init_obs_str

goodpix_ima=init_obs_str.goodpix_ima

window, 20
window, 21

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
    
;  n_images = fix(15*donnees.freq)		; skydips
  n_images = fix(13.5*donnees.freq)		; skydips
;  n_images = 575
;  n_images = fix(10*donnees.freq)		; calibrations

  command = donnees.obslog.command                                ;  recuperation de donnees additionnelles dans les OBSLOG
	if strpos(command,'time=') ne -1L then begin			; 'angle=' is contained in command
	  command = strmid(command,strpos(command,'time='))
	endif else begin
	  command = -1
	endelse  
	if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
	 	time = (strsplit(command,',',/extract))(0)
		time =  float(strmid(time,strpos(time,'=')+1))	; integration time (sec) of calibration subscans
		n_images = fix(time*donnees.freq)-5
	endif

  cube = donnees.cube(*,*,0:n_images-1)	
;  cube = donnees.cube
  test_decorrel, cube, mask, cube_corr, correl_matrice, a_matrice
;
  taille = size(cube)
  ndim = taille(3)
  lst = donnees.lst
  lst0 = lst(0:ndim-2)
  lst1 = lst(1:ndim-1)

  dt = dlst2dtloc(median(lst1-lst0))
  print, "Sampling time (sec) :", dt 
  
  time = findgen(ndim)*dt		; timeline in sec
  sfint = 1./(ndim*dt)		; smallest non-zero positive frequency in Hz
  frequency = findgen(1+ndim/2)*sfint
  freqmax = 1./(2.*dt)		; Nyquist frequency (highest frequency that
;					; can be sampled)
  donnees_corr = donnees
  donnees_corr.cube = cube_corr

;  noise_fft_pro, donnees, init_obs_str.goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av, ndim=ndim
  noise_fft_pro, donnees, init_obs_str.goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av, ndim=ndim, /sub_mean
  print, "Mean level over valid pixels : ", mean_level_av
  print, "Mean rms noise over valid pixels : ", rms_noise_av
  
  cube_pow_spec = cube_pow_spec(*,*,0:n_images-1)
  pow_spec_av = pow_spec_av(0:n_images-1)
  
;  invntt_vector = fft(1./pow_spec_av,-1)
;  
;  window, 22
;  wset, 22
;  plot, time(0:ndim/2), invntt_vector(0:ndim/2)
; 
;  stop
  
  wset, 20
  pow_max = max(pow_spec_av(0:ndim/2))
  plot, frequency, pow_spec_av(0:ndim/2), xtitle="Frequency (Hz)", $
  ytitle="Relative amplitude", title="Mean power spectrum over valid pixels for scan "+init_obs_str.subscan_name(i), $
  xrange=[0,freqmax], yrange=[-0.1*pow_max/sqrt(2.),pow_max/2.], /XSTYLE  
;  xrange=[0,freqmax/1.7], yrange=[-0.1*pow_max/sqrt(2.),pow_max/2.], /XSTYLE

;;;;;;  noise_fft_pro, cube_corr, goodpix_ima, mean_level_corr, mean_level_corr_av, rms_noise_corr, rms_noise_corr_av, cube_corr_pow_spec, pow_spec_corr_av
;  noise_fft_pro, donnees_corr, init_obs_str.goodpix_ima, mean_level_corr, mean_level_corr_av, rms_noise_corr, rms_noise_corr_av, cube_corr_pow_spec, pow_spec_corr_av, ndim=ndim
  noise_fft_pro, donnees_corr, init_obs_str.goodpix_ima, mean_level_corr, mean_level_corr_av, rms_noise_corr, rms_noise_corr_av, cube_corr_pow_spec, pow_spec_corr_av, ndim=ndim, /sub_mean
  print, "Mean level over valid pixels after decorrelation : ", mean_level_corr_av
  print, "Mean rms noise over valid pixels after decorrelation : ", rms_noise_corr_av
  print, "Improvement factor due to decorrelation (rms_noise_av/rms_noise_corr_av): ", rms_noise_av/rms_noise_corr_av  

  cube_corr_pow_spec = cube_corr_pow_spec(*,*,0:n_images-1)
  pow_spec_corr_av = pow_spec_corr_av(0:n_images-1)

;stop
  
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

;pro noise_fft_skydip, skydip_number, goodpix_ima, noise_str, dir_dat=dir_dat
;
; noise_fft_skydip, 47513, goodpix_ima, sky_47513_noise_str
; for i = 0, 4 do print, "Power = ", sky_47513_noise_str(i).mean_level_av,"  ", sky_47513_noise_str(i).scanname, "   El = ", sky_47513_noise_str(i).elevatio

amass = 1./sin(noise_str.elevatio*!pi/180.)
int   = noise_str.mean_level_av

loadct, 15
wset, 21
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [14.5,16.5]
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [15.5,17.5]
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [22.,25.]
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [21.,24.]
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [20.,23.]
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [19.5,23.]
;plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [19.5,24.]
plot, amass, int, psym = 2, /ynozero, xrange= [0.95,3.05], yrange = [min(int),max(int)]

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

;plot, amass_ext, int_fit_ext, psym = 1, /ynozero, xrange= [-0.05,3.05], yrange = [min(int_fit_ext),max(int_fit_ext)]

;openW, lun, init_obs_str.Tau_table_, /APPEND, /GET_LUN

;tau=apar[0]
;tau=STRTRIM(apar[0], 2)

;date=donnees.date_obs
;date=clean_chaine(date,':','-')
;date=clean_chaine(date,':','-')
;date=clean_chaine(date,'T','-')
;date=strsplit(date,/extract,'-')
;date=long(date)

;datemjd=(julday(date(1),date(2),date(0),date(3),date(4),date(5))-2400000.5)

;printf, lun, skydip_number, double(datemjd), [tau, donnees.date_obs]

;close, lun


;
;apar = [1.5, 16.5, 6.8]
;apar = [0.82, 18.25, 5.0]
fitapar = [1, 0, 0]		; fitting only tau
; 
skydip_fit = LMFIT(amass,int,APAR,fita=fitapar,SIGMA=RMS,CHISQ=chi,FUNCTION_NAME='skydip_fun',/double)

;
;oplot, amass, skydip_fit, psym=-1, color=130
;
print, " Tau (450 microns) at zenith = ", apar(0), " +- ", rms(0), "... when fitting only Tau and fixing other 2 parameters"
;
print, apar
;
;apar = [1.7, 16.5, 6.8]
;fitapar = [0., 1, 1]		; fitting all parameters but tau
; 
;skydip_fit = LMFIT(amass,int,APAR,fita=fitapar,SIGMA=RMS,CHISQ=chi,FUNCTION_NAME='skydip_fun',/double)
;
;oplot, amass, skydip_fit, psym=-1, color=130
;
;
;print, "    "
;print, "Best parameters when tau is fixed :"
;print, apar
;free_lun, lun
;
;stop

return
end
