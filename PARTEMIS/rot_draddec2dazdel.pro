PRO ROT_draddec2dazdel, delta_alpha, delta_delta, par_ang, delta_az, delta_el  

parang = par_ang*!pi/180		; Parallactic angle in radians

;delta_alpha = -cos(parang)*delta_az + sin(parang)*delta_el
;delta_delta =  sin(parang)*delta_az + cos(parang)*delta_el

delta_az = -cos(parang)*delta_alpha + sin(parang)*delta_delta
delta_el =  sin(parang)*delta_alpha + cos(parang)*delta_delta

return 
end
