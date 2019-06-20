pro filter_low, donnees, donnees_filt, rms_noise, rms_noise_filt

;filter lows-frequency noise in Fourier space


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	filter_low, donnees, donnees_filt, rms_noise, rms_noise_filt
	return
endif	

lambda = 450.e-6			        ;  Wavelength in m
tel_diam = 12.				        ;  Telescope diameter in m
l_to_d = (lambda/tel_diam)/(!pi/180./3600.)     ;  lambda/D in arcsec  ~ 7.75  ;   donnees.scanxvel = scanning velocity in arcsec/sec
;
freq_min = 0.35		;  Minimum meaningful frequency (in Hz) in data stream

print, "Filtering low-frequency noise: freq (Hz) < ", freq_min

cube = donnees.cube
cube_filt = cube
donnees_filt = donnees

taille = size(cube)
mean_level = fltarr(taille(1),taille(2))
rms_noise = fltarr(taille(1),taille(2))
rms_noise_filt = fltarr(taille(1),taille(2))
nchan = taille(1)*taille(2)
cube_pow_spec = cube*0.
cube_pow_spec_filt = cube*0.
goodpix_ima = donnees.goodpix_ima
index = where(goodpix_ima gt 0)

ndim = taille(3)
time = findgen(ndim)*25.e-3		; timeline in sec
sfint = 1./(ndim*25.e-3)	; smallest non-zero positive frequency in Hz
frequency = findgen(1+ndim/2)*sfint
freqmax = 1./(2.*25.e-3)	; Nyquist frequency (highest frequency that
				; can be sampled)

ind = where(frequency lt freq_min, count)		 	;  Positions of frequencies lower than minimum meaningful frequency 

for j = 0, taille(2)-1 do begin
   for i = 0, taille(1)-1 do begin
;     if goodpix_ima(i,j) gt 0. then begin
	rms_noise(i,j)   = stdev(cube(i,j,*))
        signal = cube(i,j,*)
	mean_level(i,j)  = mean(signal)
	signal = signal-mean_level(i,j)
	ft_signal = fft(signal)
	ft_signal_filt = ft_signal
	ft_signal_filt(ind) = complex(0,0)			;  killing low-frequency components  (< freq_min)
	ft_signal_filt(ndim-ind) = complex(0,0)	
	signal_filt = real_part(fft(ft_signal_filt,/inverse))
	signal_filt = signal_filt + mean_level(i,j)
	cube_filt(i,j,*) = signal_filt
	rms_noise_filt(i,j)   = stdev(signal_filt)
	pow_spec = float(ft_signal*conj(ft_signal))
	cube_pow_spec(i,j,*) = pow_spec
	pow_spec_filt = float(ft_signal_filt*conj(ft_signal_filt))
	cube_pow_spec_filt(i,j,*) = pow_spec_filt			
;     endif
   endfor
endfor

mean_level_av = mean(mean_level(index))
rms_noise_av = mean(rms_noise(index))
rms_noise_av_filt = mean(rms_noise_filt(index))

pow_spec_ref = reform(cube_pow_spec,nchan,ndim)
pow_spec_av = fltarr((size(index))(1),ndim)
pow_spec_av = pow_spec_ref(index,0:ndim-1)
pow_spec_av = total(pow_spec_av,1)/(size(index))(1)

pow_spec_ref = reform(cube_pow_spec_filt,nchan,ndim)
pow_spec_filt_av = fltarr((size(index))(1),ndim)
pow_spec_filt_av = pow_spec_ref(index,0:ndim-1)
pow_spec_filt_av = total(pow_spec_filt_av,1)/(size(index))(1)

print, "Mean rms noise over valid pixels : ", rms_noise_av
print, "Mean rms noise over valid pixels after subtraction of low-frequency noise component : ", rms_noise_av_filt


donnees_filt.cube = cube_filt

return
end

