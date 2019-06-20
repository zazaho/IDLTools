function grid_kernel, radius
;
;	INPUT: Angular distance from point source center (radius)
;	OUTPUT: Signal (normalized to 1 at source center)
;

;;fwhm = 9.4		; arcsec

fwhm = 3.87		; arcsec
;fwhm = 3.0		; arcsec

;print, "grid fwhm (arcsec)", fwhm 

psf = radius*0.

arg_exp = alog(2.)*(2.*radius/fwhm)^2

;ind = where(arg_exp le 86.,count)
ind = where(arg_exp le 86. and radius le fwhm,count)
if count gt 0 then begin
 psf(ind) = exp(-arg_exp(ind))
endif
;
return, psf
end
