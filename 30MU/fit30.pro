function fit30,in,power=power
sel = sh_select_range(in,xr=[16.5,19],/qu)
sel = sh_combine(sel,sh_select_range(in,xr=[44.2,46],/qu))

default,power,-0.5
print,object(in)
cont = sh_bbfit(sel,temp=300,/fix,power=power,outwave=in)
pl,in,/autoy,/b,/white
pl,cont,/opl,/white,linestyle=1
return,cont
end
