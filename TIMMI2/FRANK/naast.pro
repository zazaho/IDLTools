function naast,img_in1,img_in2,show=show
  
; bijelkaar voegen en tonen van 2 images

img1 = img_in1
img2 = img_in2

  x1 = n_elements(img1[*,0])
  x2 = n_elements(img2[*,0])
  y1 = n_elements(img1[0,*])
  y2 = n_elements(img2[0,*])
  ymax = max([y1,y2])
  newimg = fltarr(x1+x2,ymax)
  newimg[0:x1-1,0:y1-1] = img1
  newimg[x1:x1+x2-1,0:y2-1] = img2

;  xfact = fix(700/(x1+x2))
;  yfact = fix(500/(ymax))
;  maxfact = min([xfact,yfact])
;  print,xfact,yfact,maxfact
if (n_elements(show) NE 0) then begin
  xfact = fix(750/(x1+x2))
  yfact = fix(550/(ymax))
  maxfact = min([xfact,yfact])
;  print,xfact,yfact,maxfact
tvscl,rebin(newimg,(x1+x2)*maxfact,ymax*maxfact)
endif

return,newimg

END
