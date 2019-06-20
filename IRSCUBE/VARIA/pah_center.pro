pro pah_center,files
  for iii=0,n_elements(files)-1 do begin
     file = files[iii]
     i = readfits(file,h)
     xc = (n_elements(i[0,*])-1)/2d0
     yc = (n_elements(i[*,0])-1)/2d0
     xyad,h,xc,yc,r,d
     print,file,r,d
  endfor
end
