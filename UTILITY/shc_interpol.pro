;(SH Feb 26 1999)
;We need to make the grid mononic therefore spline needs to do
;some cleaning before execution
function shc_interpol,x,y,a
  yt = y(uniq(x))
  xt = x(uniq(x))
  yt = yt(sort(xt))
  xt = xt(sort(xt))
  at= a(sort(a))
  bt = interpol(yt,xt,at)
  b =bt
  b(sort(a)) = bt
  return,b
end
