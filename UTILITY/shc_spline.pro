;(SH Feb 26 1999)
;We need to make the grid mononic therefore spline needs to do
;some cleaning before execution
function shc_spline,x,y,a,bspline=bspline
  yt = y(uniq(x))
  xt = x(uniq(x))
  yt = yt(sort(xt))
  xt = xt(sort(xt))
  at= a(sort(a))
  if keyword_set(bspline) then begin
     sset = bspline_iterfit(x,y,nord=bspline,maxiter=0,bkspace=10)
     bt = bspline_valu(x,sset)
  endif else begin
     bt = spline(xt,yt,at)
  endelse
  
  b =bt
  b(sort(a)) = bt
  return,b
end

