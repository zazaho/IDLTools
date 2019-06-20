
;+
; NAME:
;	NOISE_FFT_PRO
;
; PURPOSE:
;
;	Evaluate noise from a skydip observation		
;
; CALLING SEQUENCE:
;	
;	NOISE_FFT_PRO, DONNEES, GOODPIX_IMA, MEAN_LEVEL, MEAN_LEVEL_AV, RMS_NOISE, RMS_NOISE_AV, CUBE_POW_SPEC, POW_SPEC_AV
;
; INPUTS:
;
;	DONNEES:	Input data structure.
;	GOODPIX_IMA:	Good pixels array.
;		
; OUTPUTS:
;
;	MEAN_LEVEL:	
;	MEAN_LEVEL_AV:
;	RMS_NOISE:
;	RMS_NOISE_AV:
;	CUBE_POW_SPEC:
;	POW_SPEC_AV:
;	
; EXAMPLE:	
;
;		NOISE_FFT_SKYDIP, 47519, sky_noise_str
;
; MODIFICATION HISTORY:
; 				
;			
;-



pro noise_fft_pro, donnees, goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av, ndim = ndim, sub_mean = sub_mean


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	NOISE_FFT_PRO, Donnees, Goodpix_ima, Mean_level, Mean_level_av, Rms_noise, Rms_noise_av, Cube_pow_spec, Pow_spec_av
	return
endif	

taille = size(donnees.cube)
mean_level = fltarr(taille(1),taille(2))
rms_noise = fltarr(taille(1),taille(2))
nchan = taille(1)*taille(2)

if not keyword_set(ndim) then begin
  ndim = taille(3)
endif

;print, "ndim = ", ndim

cube = (donnees.cube)(*,*,0:ndim-1)

cube_pow_spec = cube*0.
index = where(goodpix_ima gt 0)

lst = (donnees.lst)(0:ndim-1)
lst0 = lst(0:ndim-2)
lst1 = lst(1:ndim-1)

dt = dlst2dtloc(median(lst1-lst0))

time = findgen(ndim)*dt		; timeline in sec
sfint = 1./(ndim*dt)	; smallest non-zero positive frequency in Hz
frequency = findgen(1+ndim/2)*sfint
freqmax = 1./(2.*dt)	; Nyquist frequency (highest frequency that
				; can be sampled)

for j = 0, taille(2)-1 do begin
   for i = 0, taille(1)-1 do begin
;     if goodpix_ima(i,j) gt 0. then begin
	rms_noise(i,j)   = stdev(cube(i,j,*))
        signal = cube(i,j,*)
      if keyword_set(sub_mean) then begin
	mean_level(i,j)  = mean(signal)
	signal = signal-mean_level(i,j)
      endif	
	ft_signal = fft(signal)
	pow_spec = float(ft_signal*conj(ft_signal))
	cube_pow_spec(i,j,*) = pow_spec
;     endif
   endfor
endfor

mean_level_av = mean(mean_level(index))
rms_noise_av = mean(rms_noise(index))
pow_spec_ref = reform(cube_pow_spec,nchan,ndim)
pow_spec_av = fltarr((size(index))(1),ndim)
pow_spec_av = pow_spec_ref(index,0:ndim-1)

pow_spec_av = total(pow_spec_av,1)/(size(index))(1)

;print, "Mean rms noise over valid pixels : ", rms_noise_av

;
;wset, 0
;plot, frequency, pow_spec_av(0:ndim/2), xtitle="Frequency (Hz)", $
;ytitle="Relative amplitude", title="Mean power spectrum over valid pixels", $
;xrange=[0,freqmax/1.7], yrange=[-2e-10,14e-10], /XSTYLE
;
;			PRINTING POWER SPECTRUM
;
;set_plot,'ps'
;device,file="/users/artemis/Desktop/pow_spec.ps"

;plot, frequency, pow_spec_av(0:ndim/2), xtitle="Frequency (Hz)", $
;ytitle="Relative amplitude", title="Mean power spectrum over valid pixels", $
;xrange=[0,freqmax/1.7], yrange=[-2e-10,14e-10], /XSTYLE

;device,/close
;set_plot,'x'

return
end

