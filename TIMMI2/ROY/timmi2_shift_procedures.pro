;;(RvB Aug 14 2001) 
;;functions to do the various shifts (and fitting of shifts) nescessarry to
;;optimize the extraction of spectra in the timmi 2 data. Mostly stuff written
;;by SH, but optimized for spectra. There are 2 kinds of functions: 
;;1D fit procedures/shifts (eg. timmi2_1D_fitshift, timmi2_1D_fitshift_fitfunction,
;;OneD_real_shift) and 2D ones (eg. timmi2_2D_fitshift, timmi2_2D_fitshift_fitfunction,
;;timmi2_2D_shift)

;FUNCTION OneD_real_shift,in,shift
;  intshift = floor(shift)
;  fracshift= shift-intshift
;  return,(1-fracshift)*shift(in,intshift)+fracshift*shift(in,intshift+1)
;end
FUNCTION OneD_real_shift,in,dx
  x=in
  for i=0,(n_elements(x)-1) do begin
    x[i]=i-dx
  endfor
  output=interpolate(in,x,cubic=-0.5,/grid)
  return,output
end 

;; Function to correct timmi2 spectra for gradients in the chop
;; direction. We fit a linear gradient and remove it from each column
FUNCTION timmi2s_rectify,in,slit=slit
  
  default,slit,[70,170]
  out=in
  ny = n_elements(out)

  ;; First we take only the valid range because the slit blocks part of
  ;; the detector
  colg = out[slit[0]:slit[1]]
  
  ;; There might be a gradient. Take it out by poly_fit,order 1
  baseline_params = poly_fit(dindgen(n_elements(colg))+slit[0],colg,1)

  ;; Correction per pixel along Y
  correction = (baseline_params[0]+baseline_params[1]*dindgen(ny))

  out = out - correction
  out[0:slit[0]] = 0d0
  out[slit[1]:*] = 0d0
  
  return,out
END

FUNCTION timmi2_1D_fitshift_fitfunction,p

  COMMON COMMON_timmi2_1D_fitshift,l1,l2,buf
  ;; We have 2 independant fit parameters:
  ;; p[0] shift
  ;; p[1] scaling factor
  ;; Shift the image and subtract it from the comparison image(img1)
  diff = l1-p[1]*OneD_real_shift(l2,p[0])
  ;; Take the edges of
  ndiff = n_elements(diff)
  diff = diff[buf:ndiff-buf-1]
  return,total(diff^2d0)
END

;; Function to shift timmi2 spectra on top of eachother.
;; The procedure is quite simple:
;; i1 contains the reference where to shift to
;; i2 contains the frame to split and shift to the reference.
;; we first shift along y (cross-disp)
;; after the take a region a fit along x (disp) (this is needed
;; because the dispersion in not exactly along x :-(.
FUNCTION timmi2_1D_fitshift,i1,i2,xsize=xsize,ysize=ysize,xbuf=xbuf,ybuf=ybuf, $
                          slit=slit
  
  COMMON COMMON_timmi2_1D_fitshift,l1,l2,buf
  
  default,ysize,10
  default,ybuf,5
  default,slit,[70,170]

  ;; Find the optimal shift
  ;; First in y direction -------------------
  col1 = total(i1,1)
  col2 = total(i2,1)
;  print,'col1'
;  plot,col1
;  tvscl,i1
;  wait,1
;  print,'col2'
;  plot,col2
;  wait,2
  ;; Now we need to rectify those
  col1=timmi2s_rectify(col1,slit=slit)
  col2=timmi2s_rectify(col2,slit=slit)
  ;; Normalize
  nor1 = col1/max(col1,max1)
  nor2 = col2/max(col2,max2)
  ;; Do rough shift in the right direction:
  deltay = max1-max2
  nor2 = shift(nor2,deltay)
  ;; and take the parts wanted:
  nor1 = nor1[0>max1-ysize-ybuf:320<max1+ysize+ybuf]
  nor2 = nor2[0>max1-ysize-ybuf:320<max1+ysize+ybuf]
  
  buf=ybuf
  l1=nor1
  l2=nor2

  dy = amoeba(1d-6,FUNCTION_NAME='timmi2_1D_fitshift_fitfunction', $
              P0=[0d0,1d0],SCALE=[10d0,2d0])
  dy = deltay+dy[0]
  ;; First in y direction ------------------- End

  ;; Now do the x direction ----------------------
  ;; We simply assume we know the dispersion relation over the detector.
  ;; My rough estimate was 2/100 pixels. This should be done better
  ;; than this and also make sure to check the sign!!
  dx = timmi2s_disp_offset(dy)
  ;; Now do the x direction ---------------------- End
  dx = 0
  
  return,[dx,dy]
END

;;(SH Mar 21 2001)
;; Function to take and image and shift it. 
;; We try this to see if we can improve on the resample-gaussfit-shift method
;; The idea is to take one image and shift the second such that after
;; subtraction the residue is minimal (see also timmi_shiftadd2.pro)

FUNCTION timmi2_2D_shift,in,dx,dy
  x=in[*,0]
  y=in[0,*]
  
  for i=0,(n_elements(x)-1) do begin
    x[i]=i-dx
  endfor
  for i=0,(n_elements(y)-1) do begin
    y[i]=i-dy
  endfor

  s=interpolate(in,x,y,cubic=-0.5,/grid)
  return,s
END

FUNCTION timmi2_2D_fitshift_fitfunction,p
  COMMON COMMON_timmi2_2D_fitshift,img1,img2,esize
  ;; We have 4 independant fit parameters:
  ;; p[0] shift in the x direction
  ;; p[1] shift in the y direction
  ;; p[2] offset level
  ;; p[3] scaling factor
  ;; Shift the image and subtract it from the comparison image(img1)
  diff = img1-(p[3]*(timmi2_2D_shift(img2,p[0],p[1]))+p[2])
  ;; Take the edges of
  sdiff = size(diff)
  diff = diff[esize:sdiff[1]-esize,esize:sdiff[2]-esize]
  return,total(diff^2d0)
END

FUNCTION timmi2_2D_fitshift,i1,i2,center=center,size=size,edgesize=edgesize
  
  COMMON COMMON_timmi2_2D_fitshift,img1,img2,esize
  
  img1=i1
  img2=i2
  esize=edgesize
  
  ;; take the sections wanted
  img1 = img1[(center-size):(center+size),(center-size):(center+size)]
  img2 = img2[(center-size):(center+size),(center-size):(center+size)]
  ;; And do the scaling
  img1 = img1/max(img1)
  img2 = img2/max(img2)
  
  ;; Now we do the fitting business
  p = amoeba(1d-6,FUNCTION_NAME='timmi2_2D_fitshift_fitfunction', $
                  P0=[0d0,0d0,0d0,1d0],SCALE=[10d0,10d0,median(img2),2d0])
  return,[p[0],p[1]]
END


;;(RvB Aug 10 2001)
;;(adapted from timmi_fitshift, SH Mar 21 2001)
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
