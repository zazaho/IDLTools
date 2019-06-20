PRO oploterr_y, x, y, yerr
          

nx = n_elements(x)

y_min = y-yerr
y_max = y+yerr

for loop=0,nx-1 do begin
 usersym, [-1,1], [0,0]
 plots, [x(loop), x(loop)], [y_min(loop), y_max(loop)], ps=-8
endfor



END




