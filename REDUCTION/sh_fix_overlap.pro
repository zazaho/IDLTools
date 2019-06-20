function sh_fix_overlap,r1,r2

jump1:
  n1 = r1
  n2 = r2
  x1 = 0d0
  x2 = 0d0
  rs = 0d0
  yn = ''

  print,'between which wavelengths ?'
  read,x1,x2
  plotaar,psym=0,xr=[x1,x2],n2
  plotaar,psym=0,n1,/oplot

  wait,0.5
  veri,o1,/nostatus
  wait,0.5
  veri,o2,/nostatus

  u1 = min([o1,o2])
  u2 = max([o1,o2])

  overlap1 = sel_region(n1,xr=[u1,u2])
  overlap2 = sel_region(n2,xr=[u1,u2])
  overlap = sh_combine(overlap1,overlap2)

  print,'Rebin to what resolution ?'
  read,rs
  roverlap = sws_rebin(overlap,res=rs,met='wmean',wei='s',/nosmooth,/merge)
  n1cut = sel_region(n1,xr=[0.,u1])
  n2cut = sel_region(n2,xr=[u2,200.])
  plotaar,psym=0,n2cut,xr=[x1,x2]
  plotaar,roverlap,psym=0,/oplot
  plotaar,n1cut,psym=0,/oplot
  print,'Is this alright ? (y/n)'
  read,yn
  if (yn eq 'n') then goto,jump1

  tot = sh_combine(n1cut,roverlap)
  tot = sh_combine(tot,n2cut)
  return,tot
end
