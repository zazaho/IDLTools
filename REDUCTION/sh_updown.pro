;This procedure makes a quick red of up and down of p1
pro sh_updown,p1,p2,up=ru1,down=rd1,_extra=_extra
  u1 = getscan(p1,/up)
  d1 = getscan(p1,/down)
  rd1 = reb(sc(ff(d1,/nop),3,nit=2))
  ru1 = reb(sc(ff(u1),3,nit=2))
  if (n_params() eq 2 ) then pl,p2,ps=0,_extra=_extra
  pl,rd1,ps=0,/o,thi=2
  pl,ru1,/o,ps=0,thi=2
end
