pro tvs,img_in,maxfact

; tonen van 1 image op een maximale schaal van 750 bij 550 pixels

img1 = img_in


  x1 = n_elements(img1[*,0])
  y1 = n_elements(img1[0,*])

  xfact = fix(750/x1)
  yfact = fix(550/y1)
  maxfact = min([xfact,yfact])
  tvscl,rebin(img1,x1*maxfact,y1*maxfact)

END
