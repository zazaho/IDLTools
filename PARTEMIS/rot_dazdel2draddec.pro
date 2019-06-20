PRO ROT_dazdel2draddec, delta_az, delta_el, par_ang, delta_alpha, delta_delta

parang = par_ang*!pi/180		; Parallactic angle in radians

delta_alpha = -cos(parang)*delta_az + sin(parang)*delta_el
delta_delta =  sin(parang)*delta_az + cos(parang)*delta_el

return 
end
