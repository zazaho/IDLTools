PRO subdiv, nx, ny
; subdivide graphic page into nx X ny area
; 97/05/23 nx(ny) = 0 when inputs are 1

if (nx eq 1) then nx = 0
if (ny eq 1) then ny = 0

!P.MULTI(0:2)=[0, nx, ny]

return
end
