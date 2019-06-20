pro pah_pacs100,im1,im2,_extra=_extra
  h1 = headfits(im1)
  i2 = readfits(im2,h2)
  hswarp,i2,h2,ni2,nh2,h1,/fixheader,/keepedge

  newname=str_replace(im1,"im112","impacs100")
  writefits,newname,float(ni2),nh2
end
