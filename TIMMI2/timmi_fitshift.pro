FUNCTION timmi_fitshift_fitfunction,p
  COMMON COMMON_timmi_fitshift,r,i,esize
  ;; We have 4 independant fit parameters:
  ;; p[0] shift in the x direction
  ;; p[1] shift in the y direction
  ;; p[2] offset level
  ;; p[3] scaling factor
  ;; Shift the image and subtract it from the comparison image(img1)
  diff = r-(p[3]*(timmi_shift(i,p[0],p[1]))+p[2])
  ;; Take the edges of
  sdiff = size(diff)
  diff = diff[esize:sdiff[1]-esize,esize:sdiff[2]-esize]
;;  print,total(diff),total(diff^2d0),p
  return,total(diff^2d0)
END
  
;;(SH Mar 21 2001)
;;Function to find the optimal shifting of the image in order to able
;;to add the images.
;;Input img1,img2
;;
;;Procedure:
;;Take the first input image and shift the second roughly to
;;the right position. Do this simply by the brightest spot
;;Take a box of both image which consist of the bright part and a
;;buffer border. The buffer is needed because after the shifting we
;;need to cut of some edge.
;;Scale both images from median to max =0-1 and shift img2 around
;;until the subtracted images give least residu. We use amoeba for
;;this

FUNCTION timmi_fitshift,ref,img,x=x,y=y,boxsize=boxsize,edgesize=edgesize
  
  COMMON COMMON_timmi_fitshift,r,i,esize
  
  default,boxsize,30
  default,edgesize,10
  default,x,160
  default,y,120
  r=ref
  i=img
  esize=edgesize

;; Note we expect the ref image to be centered on (160,120)
  
  ;; Now shift the i roughly to the right position
  deltax = 160-x
  deltay = 120-y
  i = shift(i,deltax,deltay)

  ;; and take the sections wanted
  r = r[0>160-boxsize-edgesize:320<160+boxsize+edgesize, $
              0>120-boxsize-edgesize:240<120+boxsize+edgesize]
  i = i[0>160-boxsize-edgesize:320<160+boxsize+edgesize, $
              0>120-boxsize-edgesize:240<120+boxsize+edgesize]
  ;; And do the scaling
  r = r/max(r)
  i = i/max(i)
  
  ;; Now we do the fitting business
  p = amoeba(1d-4,FUNCTION_NAME='timmi_fitshift_fitfunction', $
                  P0=[0d0,0d0,0d0,1d0],SCALE=[10d0,10d0,median(i),2d0])
  print,[deltax+p[0],deltay+p[1]]
  return,[deltax+p[0],deltay+p[1]]
END
