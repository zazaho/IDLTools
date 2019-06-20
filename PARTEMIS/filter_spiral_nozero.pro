;+
; NAME:
;	filter_spiral
;
; PURPOSE:
;
;	Filter low-frequency noise in Fourier space, preserving harmonics of spiral frequency
;	+ kill 50Hz component
;
; CALLING SEQUENCE:
;	
;	filter_spiral, donnees, donnees_filtered, rms_noise, rms_noise_filtered, spiral_freq=spiral_freq
;
; INPUTS:
;
;	Donnees:	Input data structure.	
;
; OUTPUTS:
;
;	donnees_filtered:	Ouput data structure after low-frequency noise filtering.
;	rms_noise:		Initial noise.
;	rms_noise_filtered:	Noise after low-frequency noise filtering.
;
; EXAMPLE:
;		filter_spiral, donnees, donnees_filtered, rms_noise, rms_noise_filtered, spiral_freq=spiral_freq
;
; MODIFICATION HISTORY:
;
;-

pro filter_spiral, donnees, donnees_filtered, rms_noise, rms_noise_filtered, spiral_freq=spiral_freq

;; Filter low-frequency noise in Fourier space, preserving harmonics of spiral frequency


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	filter_spiral, donnees, donnees_filtered, rms_noise, rms_noise_filtered
	return
endif	

cube = donnees.cube
cube_filtered = cube
donnees_filtered = donnees

taille = size(cube)
mean_level = fltarr(taille(1),taille(2))
rms_noise = fltarr(taille(1),taille(2))
rms_noise_filtered = fltarr(taille(1),taille(2))
nchan = taille(1)*taille(2)
cube_pow_spec = cube*0.
cube_pow_spec_filtered = cube*0.
goodpix_ima = donnees.goodpix_ima
index = where(goodpix_ima gt 0)

ndim = taille(3)
time = findgen(ndim)*25.e-3		; timeline in sec
sfint = 1./(ndim*25.e-3)	; smallest non-zero positive frequency in Hz
;frequency = findgen(ndim)*sfint
frequency = findgen(1+ndim/2)*sfint
freqmax = 1./(2.*25.e-3)	; Nyquist frequency (highest frequency that
				; can be sampled)

def_spiral_freq = 80./360.	; default spiral frequency  (Rotation at 80 deg/sec)  ~  0.222 Hz

if not keyword_set(spiral_freq) then begin
	spiral_freq = def_spiral_freq
endif

command = donnees.obslog.command                                ;  recuperation de donnees additionnelles dans les OBSLOG
;	

command = strmid(command,strpos(command,'time='))
;
freq_cycle = -1
;
if ((size(command))(1) eq 7) then begin				;  command is a string (i.e. strmid did not return -1)
	int_time_elem = (strsplit(command,',',/extract))(0)
	int_time_elem =  float(strmid(int_time_elem,strpos(int_time_elem,'=')+1))	; Integration time per spiral when subscan is made up of several spirals

	if (1./sfint ge 2.*int_time_elem) then freq_cycle = 1./int_time_elem		; Spiral repetition frequency	  
endif
;

;ind0 = where(abs(frequency-spiral_freq) lt 3.*sfint, count0)		                   ;  position of spiral_frequency in frequency vector  ; repliement des frequences

mod_frequency = frequency mod spiral_freq
;;;;ind0 = where(mod_frequency lt 3.*sfint or spiral_freq-mod_frequency lt 3.*sfint, count0)   ;  positions of harmonics of spiral_frequency in frequency vector


;ind0 = where(frequency lt 4.*sfint, count0)   						   ;  positions close to zero frequency in frequency vector
;
if freq_cycle gt 0. then begin
   print, "Integration time per spiral (s) = ", int_time_elem, " freq_cycle (Hz) = ", freq_cycle
;
   ind  = where(frequency gt freq_cycle+2*sfint and mod_frequency gt 0.05 and spiral_freq-mod_frequency gt 0.05, count)    ; frequencies away from harmonics of spiral_frequency: 0.05 Hz = 3*1./60.
;   
;   ind0 = where(frequency lt 1./15. and abs(frequency-freq_cycle) gt 1.3*sfint and abs(frequency-2*freq_cycle) gt sfint, count0)    ;  frequencies close to 0, excluding spiral repetition frequency
;   ind0 = where(frequency lt 1./15. and frequency gt freq_cycle+1.75*sfint and abs(frequency-2.*freq_cycle) gt sfint and abs(frequency-3.*freq_cycle) gt sfint, count0)   	
    ind0 = ind
;   
;   print, "frequency(ind0(count0-1)) = ", frequency(ind0(count0-1))
;   print, "double freq cycle = ", 2*freq_cycle
;   print, "frequency(ind(0)) = ", frequency(ind(0))
endif else begin
;
;   ind  = where(mod_frequency gt 3.*sfint and spiral_freq-mod_frequency gt 3.*sfint, count)                   ; positions away from harmonics of spiral_frequency in frequency vector
;   ind  = where(frequency gt 1./15. and mod_frequency gt 0.05 and spiral_freq-mod_frequency gt 0.05, count)    ; frequencies away from harmonics of spiral_frequency: 0.05 Hz = 3*1./60.
   ind  = where(frequency gt 1./15. and mod_frequency gt 0.0857 and spiral_freq-mod_frequency gt 0.0857, count)    ; frequencies away from harmonics of spiral_frequency: 0.0857 Hz = 3*1./35.

   ind0 = where(frequency lt 1./15., count0)   					   		;  positions close to zero frequency in frequency vector
;;;   ind0 = where(frequency lt 1./15. and mod_frequency gt 0.05, count0)   				;  positions close to zero frequency in frequency vector

endelse

ind50 = where(abs(frequency-10.) lt 4.*sfint or abs(frequency-20.) lt sfint/2., count50)   ;  position of 10Hz (<-> 50 Hz) in frequency vector  ; repliement des frequences


for j = 0, taille(2)-1 do begin
   for i = 0, taille(1)-1 do begin
;     if goodpix_ima(i,j) gt 0. then begin
	rms_noise(i,j)   = stdev(cube(i,j,*))
        signal = cube(i,j,*)
	mean_level(i,j)  = mean(signal)
	signal = signal-mean_level(i,j)
	ft_signal = fft(signal)	
	ft_signal_filtered = ft_signal
;	
	ft_signal_filtered(ind) = complex(0,0)			   ;  filtering frequency components which are not close to harmonics of spiral frequency
	ft_signal_filtered(ndim-ind) = complex(0,0)
	ft_signal_filtered(ind0) = complex(0,0)			   ;  filtering low-frequency noise
	ft_signal_filtered(ndim-ind0) = complex(0,0)
	ft_signal_filtered(ind50) = complex(0,0)	           ;  killing 10Hz and 20Hz components  (<--> 50 Hz)
	ft_signal_filtered(ndim-ind50) = complex(0,0)	
;			
	signal_filtered = real_part(fft(ft_signal_filtered,/inverse))
	signal_filtered = signal_filtered + mean_level(i,j)
	cube_filtered(i,j,*) = signal_filtered
	rms_noise_filtered(i,j)   = stdev(signal_filtered)
	pow_spec = float(ft_signal*conj(ft_signal))
	cube_pow_spec(i,j,*) = pow_spec
	pow_spec_filtered = float(ft_signal_filtered*conj(ft_signal_filtered))
	cube_pow_spec_filtered(i,j,*) = pow_spec_filtered			
;     endif
   endfor
endfor

mean_level_av = mean(mean_level(index))
rms_noise_av = mean(rms_noise(index))
rms_noise_av_filtered = mean(rms_noise_filtered(index))

pow_spec_ref = reform(cube_pow_spec,nchan,ndim)
pow_spec_av = fltarr((size(index))(1),ndim)
pow_spec_av = pow_spec_ref(index,0:ndim-1)
pow_spec_av = total(pow_spec_av,1)/(size(index))(1)

pow_spec_ref = reform(cube_pow_spec_filtered,nchan,ndim)
pow_spec_filtered_av = fltarr((size(index))(1),ndim)
pow_spec_filtered_av = pow_spec_ref(index,0:ndim-1)
pow_spec_filtered_av = total(pow_spec_filtered_av,1)/(size(index))(1)

print, "Mean rms noise over valid pixels : ", rms_noise_av
print, "Mean rms noise over valid pixels after frequency filtering : ", rms_noise_av_filtered

donnees_filtered.cube = cube_filtered

;stop

return
end

