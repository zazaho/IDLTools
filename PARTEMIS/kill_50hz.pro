;+
; NAME:
;	KILL_50HZ
;
; PURPOSE:
;
;	Eliminate 50 Hz in Fourier space.
;
; CALLING SEQUENCE:
;	
;	KILL_50HZ, Donnees, Donnees_50Hz, Rms_noise, Rms_noise_50Hz
;
; INPUTS:
;
;	Donnees:	Input data structure.	
;
; OUTPUTS:
;
;	Donnees_50Hz:	Ouput data structure after frequency cut off.
;	Rms_noise:	Initial noise.
;	Rms_noise_50Hz:	Noise after frequency cut off.
;
; EXAMPLE:
;		KILL_50HZ, Donnees, Donnees_50Hz, Rms_noise, Rms_noise_50Hz
;
; MODIFICATION HISTORY:
;
;-

pro kill_50Hz, donnees, donnees_50Hz, rms_noise, rms_noise_50Hz

;elimate 50 Hz noise in Fourier space


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	KILL_50HZ, Donnees, Donnees_50Hz, Rms_noise, Rms_noise_50Hz
	return
endif	

cube = donnees.cube
cube_50Hz = cube
donnees_50Hz = donnees

taille = size(cube)
mean_level = fltarr(taille(1),taille(2))
rms_noise = fltarr(taille(1),taille(2))
rms_noise_50Hz = fltarr(taille(1),taille(2))
nchan = taille(1)*taille(2)
cube_pow_spec = cube*0.
cube_pow_spec_50Hz = cube*0.
goodpix_ima = donnees.goodpix_ima
index = where(goodpix_ima gt 0)

ndim = taille(3)
time = findgen(ndim)*25.e-3		; timeline in sec
sfint = 1./(ndim*25.e-3)	; smallest non-zero positive frequency in Hz
frequency = findgen(ndim)*sfint
freqmax = 1./(2.*25.e-3)	; Nyquist frequency (highest frequency that
				; can be sampled)

ind = where(abs(frequency-10.) lt 4.*sfint, count)		;  position of 10Hz (<-> 50 Hz) in frequency vector  ; repliement des frequences
ind1 = where(abs(frequency-20.) lt sfint/2., count1)		;  position of 10Hz (<-> 50 Hz) in frequency vector

for j = 0, taille(2)-1 do begin
   for i = 0, taille(1)-1 do begin
;     if goodpix_ima(i,j) gt 0. then begin
	rms_noise(i,j)   = stdev(cube(i,j,*))
        signal = cube(i,j,*)
	mean_level(i,j)  = mean(signal)
	signal = signal-mean_level(i,j)
	ft_signal = fft(signal)
	ft_signal_50Hz = ft_signal
	ft_signal_50Hz(ind) = complex(0,0)			;  killing 10Hz component  (<--> 50 Hz)
	ft_signal_50Hz(ndim-ind) = complex(0,0)
    if count1 gt 0. then begin
	ft_signal_50Hz(ind1) = complex(0,0)			;  killing 10Hz component  (<--> 50 Hz)
	ft_signal_50Hz(ndim-ind1) = complex(0,0)
    endif	
	signal_50Hz = real_part(fft(ft_signal_50Hz,/inverse))
	signal_50Hz = signal_50Hz + mean_level(i,j)
	cube_50Hz(i,j,*) = signal_50Hz
	rms_noise_50Hz(i,j)   = stdev(signal_50Hz)
	pow_spec = float(ft_signal*conj(ft_signal))
	cube_pow_spec(i,j,*) = pow_spec
	pow_spec_50Hz = float(ft_signal_50Hz*conj(ft_signal_50Hz))
	cube_pow_spec_50Hz(i,j,*) = pow_spec_50Hz			
;     endif
   endfor
endfor

mean_level_av = mean(mean_level(index))
rms_noise_av = mean(rms_noise(index))
rms_noise_av_50Hz = mean(rms_noise_50Hz(index))

pow_spec_ref = reform(cube_pow_spec,nchan,ndim)
pow_spec_av = fltarr((size(index))(1),ndim)
pow_spec_av = pow_spec_ref(index,0:ndim-1)
pow_spec_av = total(pow_spec_av,1)/(size(index))(1)

pow_spec_ref = reform(cube_pow_spec_50Hz,nchan,ndim)
pow_spec_50Hz_av = fltarr((size(index))(1),ndim)
pow_spec_50Hz_av = pow_spec_ref(index,0:ndim-1)
pow_spec_50Hz_av = total(pow_spec_50Hz_av,1)/(size(index))(1)

print, "Mean rms noise over valid pixels : ", rms_noise_av
print, "Mean rms noise over valid pixels after subtraction of 50 Hz noise component : ", rms_noise_av_50Hz


donnees_50Hz.cube = cube_50Hz

return
end

