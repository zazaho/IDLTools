pro plot_fft, donnees, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av, pow_spec_row, pow_spec_row_av


noise_fft_pro, donnees, donnees.goodpix_ima, mean_level, mean_level_av, rms_noise, rms_noise_av, cube_pow_spec, pow_spec_av

taille = size(donnees.cube)

cube = donnees.cube

mean_level = fltarr(taille(1),taille(2))
rms_noise = fltarr(taille(1),taille(2))
nchan = taille(1)*taille(2)

cube_pow_spec = cube*0.
id_good = where(donnees.goodpix_ima eq 1)

goodpix_ima = donnees.goodpix_ima
vec_goodpix_ima = fltarr(taille(1)*taille(2))
vec_goodpix_ima(*) = goodpix_ima(*)
;
weight = reform(matrix_multiply(vec_goodpix_ima,fltarr(taille(3))+1.,/btranspose),taille(1),taille(2),taille(3))

;if not keyword_set(ndim) then begin
; ndim = taille(3)
;endif

;lst = (donnees.lst)(0:ndim-1)
lst = donnees.lst
ind = where(lst ne -999.)
id0 = min(ind)
id1 = max(ind)

lst0 = lst(id0:id1-1)
lst1 = lst(id0+1:id1)

;dt = dlst2dtloc(median(lst1-lst0))

dt = 25e-3

lst = lst(id0:id1)
ndim = n_elements(lst)

time = findgen(ndim)*dt		; timeline in sec
sfint = 1./(ndim*dt)	; smallest non-zero positive frequency in Hz
frequency = findgen(1+ndim/2)*sfint
freqmax = 1./(2.*dt)	; Nyquist frequency (highest frequency that
				; can be sampled)

pow_max = max(pow_spec_av(0:ndim/2))

wset, 20
plot, frequency, pow_spec_av(0:ndim/2), xtitle="Frequency (Hz)", $
ytitle="Relative amplitude", title="Mean power spectrum over valid pixels", $
xrange=[0,3.], yrange=[-0.1*pow_max/sqrt(2.),pow_max*1.05], /XSTYLE

av_signal_row = Total(cube*weight,1)/Total(weight,1)
pow_spec_row = fltarr(taille(2),ndim)

for j = 0, taille(2)-1 do begin
  signal = av_signal_row(j,id0:id1)
  ft_signal = fft(signal)
  pow_spec_signal = float(ft_signal*conj(ft_signal))
  pow_spec_row(j,*) = pow_spec_signal
endfor

row_weight = fltarr(taille(2))+1.
for j = 0, taille(2)-1 do row_weight(j) = abs(mean(pow_spec_row(j,*)))
id_nan = where(finite(row_weight,/nan) eq 1,cnt_nan)
row_weight = fltarr(taille(2))+1.
if (cnt_nan gt 0) then begin
   row_weight(id_nan) = 0.
   pow_spec_row(id_nan,*) = 0.
endif

row_weight = matrix_multiply(row_weight,fltarr(ndim)+1.,/btranspose)

;pow_spec_row_av = total(pow_spec_row,1)/taille(2)
pow_spec_row_av = total(pow_spec_row*row_weight,1)/total(row_weight,1)

pow_row_max = max(pow_spec_row_av(0:ndim/2-1))

wset, 22
plot, frequency, pow_spec_row_av(0:ndim/2), xtitle="Frequency (Hz)", $
ytitle="Relative amplitude", title="Average Power spectrum of signal averaged over each row", $
xrange=[0,3.], yrange=[-0.1*pow_max/sqrt(2.),pow_row_max*1.05], /XSTYLE



av_signal = fltarr(taille(3))

for k = id0, id1 do begin
 array_val = cube(*,*,k)
 array_val = array_val(id_good)
 av_signal(k) = mean(array_val) 
endfor

wset, 21
plot, time, av_signal(id0:id1), xtitle="Time (s)", $
ytitle="Signal", title="Mean signal over valid pixels"

signal = av_signal(id0:id1)
ft_signal = fft(signal)
pow_spec_av_signal = float(ft_signal*conj(ft_signal))

print, "Mean level of average signal:", mean(signal)
print, "Median level of average signal:", median(signal)
print, "Standard deviation of average signal:", stdev(signal)

pow_max = max(pow_spec_av_signal(0:ndim/2))

wset, 20
plot, frequency, pow_spec_av_signal(0:ndim/2), xtitle="Frequency (Hz)", $
ytitle="Relative amplitude", title="Power spectrum of mean signal over valid pixels", $
xrange=[0,3.], yrange=[-0.1*pow_max/sqrt(2.),pow_max*1.05], /XSTYLE

print, "Power spectrum of average signal at zero frequency:", pow_spec_av_signal(0)

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

