pro gauss2D_fit, mapradecstr, map_gaussfit_aparm
;
; gauss2D_fit, model_str, model_gaussfit_aparm
; gauss2D_fit, b59mms2_mod_spiral_44517, gaussfit_aparm

image = mapradecstr.image
wei_max = max(mapradecstr.weight)
ind = where(mapradecstr.weight lt 0.25*wei_max, count)
if (count gt 0) then begin
 image(ind) = 0.
endif

;map_gaussfit = gauss2dfit(mapradecstr.image,map_gaussfit_aparm,/tilt)
map_gaussfit = gauss2dfit(image,map_gaussfit_aparm,/tilt)

print, "Zero Level (Jy/beam) = ", map_gaussfit_aparm(0)
print, "Peak Intensity (Jy/beam) = ", map_gaussfit_aparm(1)
print, "Delta x  (arcsec) = ", (map_gaussfit_aparm(4)-mapradecstr.crpix1)*mapradecstr.cdelt1*3600.
print, "Delta y  (arcsec) = ", (map_gaussfit_aparm(5)-mapradecstr.crpix2)*mapradecstr.cdelt2*3600.
print, "Position angle of major axis measured counterclockwise from x axis (deg) = ", map_gaussfit_aparm(6)
print, "Maj FWHM (arcsec) = ", map_gaussfit_aparm(2)*sqrt(8.*alog(2.))*mapradecstr.cdelt2*3600.
print, "Min FWHM (arcsec) = ", map_gaussfit_aparm(3)*sqrt(8.*alog(2.))*mapradecstr.cdelt2*3600.
;

return
end
