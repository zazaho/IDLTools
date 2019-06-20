;+
; NAME:
;	KILL_high
;
; PURPOSE:
;
;	Eliminate high-frequency noise in Fourier space.
;
; CALLING SEQUENCE:
;	
;	KILL_high, Donnees, Donnees_high, Rms_noise, Rms_noise_high
;
; INPUTS:
;
;	Donnees:	Input data structure.	
;
; OUTPUTS:
;
;	Donnees_high:	Ouput data structure after frequency cut off.
;	Rms_noise:	Initial noise.
;	Rms_noise_high:	Noise after frequency cut off.
;
; EXAMPLE:
;		KILL_high, Donnees, Donnees_high, Rms_noise, Rms_noise_high
;
; MODIFICATION HISTORY:
;
;-

pro kill_high, donnees, donnees_high, rms_noise, rms_noise_high

;eliminate high-frequency noise in Fourier space


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	KILL_high, Donnees, Donnees_high, Rms_noise, Rms_noise_high
	return
endif	

lambda = 450.e-6			        ;  Wavelength in m
tel_diam = 12.				        ;  Telescope diameter in m
l_to_d = (lambda/tel_diam)/(!pi/180./3600.)     ;  lambda/D in arcsec  ~ 7.75  ;   donnees.scanxvel = scanning velocity in arcsec/sec
;
freq_max_phys = donnees.scanxvel/l_to_d		;  Maximum meaningful frequency (in Hz) in data stream

print, "Filtering high-frequency noise: freq (Hz) > ", freq_max_phys, "  -  Scanning velocity (arcsec/sec) = ", donnees.scanxvel

cube = donnees.cube
cube_high = cube
donnees_high = donnees

taille = size(cube)
mean_level = fltarr(taille(1),taille(2))
rms_noise = fltarr(taille(1),taille(2))
rms_noise_high = fltarr(taille(1),taille(2))
nchan = taille(1)*taille(2)
cube_pow_spec = cube*0.
cube_pow_spec_high = cube*0.
goodpix_ima = donnees.goodpix_ima
index = where(goodpix_ima gt 0)

ndim = taille(3)
time = findgen(ndim)*25.e-3		; timeline in sec
sfint = 1./(ndim*25.e-3)	; smallest non-zero positive frequency in Hz
frequency = findgen(1+ndim/2)*sfint
freqmax = 1./(2.*25.e-3)	; Nyquist frequency (highest frequency that
				; can be sampled)

ind = where(frequency gt freq_max_phys, count)		 	;  Positions of frequencies higher than maximum meaningful frequency 

for j = 0, taille(2)-1 do begin
   for i = 0, taille(1)-1 do begin
;     if goodpix_ima(i,j) gt 0. then begin
	rms_noise(i,j)   = stdev(cube(i,j,*))
        signal = cube(i,j,*)
	mean_level(i,j)  = mean(signal)
	signal = signal-mean_level(i,j)
	ft_signal = fft(signal)
	ft_signal_high = ft_signal
	ft_signal_high(ind) = complex(0,0)			;  killing high-frequency components  (> freq_max_phys)
	ft_signal_high(ndim-ind) = complex(0,0)	
	signal_high = real_part(fft(ft_signal_high,/inverse))
	signal_high = signal_high + mean_level(i,j)
	cube_high(i,j,*) = signal_high
	rms_noise_high(i,j)   = stdev(signal_high)
	pow_spec = float(ft_signal*conj(ft_signal))
	cube_pow_spec(i,j,*) = pow_spec
	pow_spec_high = float(ft_signal_high*conj(ft_signal_high))
	cube_pow_spec_high(i,j,*) = pow_spec_high			
;     endif
   endfor
endfor

mean_level_av = mean(mean_level(index))
rms_noise_av = mean(rms_noise(index))
rms_noise_av_high = mean(rms_noise_high(index))

pow_spec_ref = reform(cube_pow_spec,nchan,ndim)
pow_spec_av = fltarr((size(index))(1),ndim)
pow_spec_av = pow_spec_ref(index,0:ndim-1)
pow_spec_av = total(pow_spec_av,1)/(size(index))(1)

pow_spec_ref = reform(cube_pow_spec_high,nchan,ndim)
pow_spec_high_av = fltarr((size(index))(1),ndim)
pow_spec_high_av = pow_spec_ref(index,0:ndim-1)
pow_spec_high_av = total(pow_spec_high_av,1)/(size(index))(1)

print, "Mean rms noise over valid pixels : ", rms_noise_av
print, "Mean rms noise over valid pixels after subtraction of high-frequency noise component : ", rms_noise_av_high


donnees_high.cube = cube_high

return
end

