function fix2a,in,_extra=_extra
fix = sh_getpolycorrection(in,/band,/cband,xr=[3,10],_extra=_extra)
print,"r = sh_calcline(r,5,add=sh_calcaar(r,poly=["+ $
      f2s(fix.offset[0],3)+","+f2s(fix.offset[1],3)+"]))"
print,"r = sh_calcline(r,5,mult=sh_calcaar(r,poly=["+f2s(fix.scaling[0],3)+ $
      ","+f2s(fix.scaling[1],3)+"]))"
return,fix
end
