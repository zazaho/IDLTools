function same_array,a1,a2

if (n_elements(a1) ne n_elements(a2)) then return,0

IF (min(a1 NE a2)) THEN return,0

return,1
end
