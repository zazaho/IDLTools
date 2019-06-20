PRO oploterr_x, x, y, xerr
          

nx = n_elements(x)

x_min = x-xerr
x_max = x+xerr

for loop=0,nx-1 do begin
 usersym, [0,0], [-1,1]
 plots, [x_min(loop), x_max(loop)], [y(loop), y(loop)], ps=-8
endfor



END




