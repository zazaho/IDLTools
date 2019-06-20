pro center2ad,filename
  hdr=headfits(filename)

  nx = sxpar(hdr,"NAXIS1")
  ny = sxpar(hdr,"NAXIS2")

  ;; centers of pixels run from 0 to nx-1
  ;; center of the central pixels is at
  ;; (nx-1)/2.0,(ny-1)/2.0
  xyad,hdr,(nx-1)/2.0,(ny-1)/2.0,ra,de
  
  print,ra2s(ra)," ",dec2s(de)
  
end
