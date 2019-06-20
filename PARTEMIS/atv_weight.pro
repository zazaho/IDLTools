pro atv_weight, mapradecstr, frac_weight = frac_weight


image  = mapradecstr.image*0.
weight = mapradecstr.weight

weight_max = max(mapradecstr.weight)

if not keyword_set(frac_weight) then frac_weight = 0.2

print, "Displaying ", mapradecstr.name,"  ", mapradecstr.source, " above ", frac_weight, " x maximum weight" 

ind = where(weight gt frac_weight*weight_max)

image(ind) = mapradecstr.image(ind)

atv, image

return
end

