function trap_int,x,y,xrange=xrange
  return,sh_integrate(transpose([[reform(x)],[reform(y)]]),xrange=xrange,/noconvert,/noplot,/quiet)
end
