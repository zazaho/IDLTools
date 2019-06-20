function fix2c,in,_extra=_extra
fix = sh_getpolycorrection(in,/band,/cband,xr=[5,15],_extra=_extra)
print,"r = sh_calcline(r,7,add=sh_calcaar(r,poly=["+ $
      f2s(fix.offset[0],3)+","+f2s(fix.offset[1],3)+"]))"
print,"r = sh_calcline(r,7,mult=sh_calcaar(r,poly=["+f2s(fix.scaling[0],3)+ $
      ","+f2s(fix.scaling[1],3)+"]))"
return,fix
end
