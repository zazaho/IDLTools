function fix3e,in
fix = sh_getpolycorrection(in,/band,/flamb,/cband,xr=[22,32])
print,"r = sh_calcline(r,12,add=sh_calcaar(r,poly=["+ $
      f2s(fix.offset[0],3)+","+f2s(fix.offset[1],3)+"]))"
print,"r = sh_calcline(r,12,mult=sh_calcaar(r,poly=["+f2s(fix.scaling[0],3)+ $
      ","+f2s(fix.scaling[1],3)+"]))"
return,fix
end
