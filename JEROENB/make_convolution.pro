@conv_gauss.pro

pro make_convolution,file,back=back

COMMON SHARE_GRID, XDim, YDim, XRef, YRef, XVal, YVal, Xinc, Yinc


get_xy,'image_75_'+file+'_non_sub.dat',radius1,t1,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_110_'+file+'_non_sub.dat',radius2,t2,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_170_'+file+'_non_sub.dat',radius3,t3,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_250_'+file+'_non_sub.dat',radius4,t4,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_350_'+file+'_non_sub.dat',radius5,t5,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_500_'+file+'_non_sub.dat',radius6,t6,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_850_'+file+'_non_sub.dat',radius7,t7,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3
get_xy,'image_1250_'+file+'_non_sub.dat',radius8,t8,ncolumns=3,xcolumn=2,ycolumn=3,nheader=3

;start of convolution procedure
;convolution for expected Hershel performance

;nprofile = n_elements(radius1)
;npmax= nprofile-1

;max radius in profile
;rmax = radius1[npmax]

;distance in pc
dist=160.0

;resolution at 110micron in arcsec
resol = 18.0

;required pixel size
npix = 128
rinc = 4.5

;regular grid used with interpolation
radius = indgen(npix)*rinc

;determine index background
rclosest  = min(abs(radius-back),index_back)


profile1 = interpol(t1,radius1,radius)
index = where (profile1 < 0.0 , count)
if count ne 0 then profile1[index] = 0.0

profile2 = interpol(t2,radius2,radius)
index = where (profile2 < 0.0 , count)
if count ne 0 then profile2[index] = 0.0

profile3 = interpol(t3,radius3,radius)
index = where (profile3 < 0.0 , count)
if count ne 0 then profile3[index] = 0.0

profile4 = interpol(t4,radius4,radius)
index = where (profile4 < 0.0 , count)
if count ne 0 then profile4[index] = 0.0

profile5 = interpol(t5,radius5,radius)
index = where (profile5 < 0.0 , count)
if count ne 0 then profile5[index] = 0.0

profile6 = interpol(t6,radius6,radius)
index = where (profile6 < 0.0 , count)
if count ne 0 then profile6[index] = 0.0

profile7 = interpol(t7,radius7,radius)
index = where (profile7 < 0.0 , count)
if count ne 0 then profile7[index] = 0.0

profile8 = interpol(t8,radius8,radius)
index = where (profile8 < 0.0 , count)
if count ne 0 then profile8[index] = 0.0

;number of pixels in 2d image
npix_2d=2*npix-1

;scaling factors
fac = !pi*(18.*!pi/(180.*3600.))^2/(4.*alog(2.)) ; Scaling factor: Beam solid angle in sr
fac2 = (rinc*!pi/(180.*3600.))^2 ; Scaling factor: pixel solid angle in sr

;make 2d image
image1 = circsym(profile1)
print, 'total (Jy)) image: ',total(image1*fac2*1.e23)
image1 = image1*fac*1.e23    ; to scale image in Jy/beam

image2 = circsym(profile2)
print, 'total (Jy)) image: ',total(image2*fac2*1.e23)
image2 = image2*fac*1.e23    ; to scale image in Jy/beam

image3 = circsym(profile3)
print, 'total (Jy)) image: ',total(image3*fac2*1.e23)
image3 = image3*fac*1.e23    ; to scale image in Jy/beam

image4 = circsym(profile4)
print, 'total (Jy)) image: ',total(image4*fac2*1.e23)
image4 = image4*fac*1.e23    ; to scale image in Jy/beam

image5 = circsym(profile5)
print, 'total (Jy)) image: ',total(image5*fac2*1.e23)
image5 = image5*fac*1.e23    ; to scale image in Jy/beam

image6 = circsym(profile6)
print, 'total (Jy)) image: ',total(image6*fac2*1.e23)
image6 = image6*fac*1.e23    ; to scale image in Jy/beam

image7 = circsym(profile7)
print, 'total (Jy)) image: ',total(image7*fac2*1.e23)
image7 = image7*fac*1.e23    ; to scale image in Jy/beam

image8 = circsym(profile8)
print, 'total (Jy)) image: ',total(image8*fac2*1.e23)
image8 = image8*fac*1.e23    ; to scale image in Jy/beam

;convlolution par.

Xdim = npix_2d
Ydim = npix_2d
xinc = rinc
yinc = rinc
xref = (Xdim-1.)/2.
yref = (Ydim-1.)/2.
xval = 0.
yval = 0.

;convolve image

conv_image = p_conv_gauss(image1, resol)
conv_profile1 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub1 = conv_profile1-conv_profile1[index_back]

conv_image = p_conv_gauss(image2, resol)
conv_profile2 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub2 = conv_profile2-conv_profile2[index_back]

conv_image = p_conv_gauss(image3, resol)
conv_profile3 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub3 = conv_profile3-conv_profile3[index_back]

conv_image = p_conv_gauss(image4, resol)
conv_profile4 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub4 = conv_profile4-conv_profile4[index_back]

conv_image = p_conv_gauss(image5, resol)
conv_profile5 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub5 = conv_profile5-conv_profile5[index_back]

conv_image = p_conv_gauss(image6, resol)
conv_profile6 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub6 = conv_profile6-conv_profile6[index_back]

conv_image = p_conv_gauss(image7, resol)
conv_profile7 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub7 = conv_profile7-conv_profile7[index_back]

conv_image = p_conv_gauss(image8, resol)
conv_profile8 = conv_image[npix-1,npix-1:npix_2d-1]/(fac*1.e23)
conv_profile_sub8 = conv_profile8-conv_profile8[index_back]

openw,1,'model_reconstruction_'+file+'.dat'
printf,1,'distance=',dist,'AU'
printf,1,'Background at ',radius[index_back]*dist,' AU'
printf,1,format='(A13,8e13.4)','ISRF value', conv_profile1[index_back],conv_profile2[index_back],conv_profile3[index_back],$
                         conv_profile4[index_back], conv_profile5[index_back], conv_profile6[index_back],$
			  conv_profile7[index_back], conv_profile8[index_back]
printf,1,format='(9A13)','RADIUS','    I75','   I110','   I170','   I250','   I350','   I500','   I850','  I1250'
for i=0,n_elements(radius)-1 do begin
   printf,1,format='(9e13.4)',radius(i)*dist,conv_profile_sub1(i),conv_profile_sub2(i),$
             conv_profile_sub3(i),conv_profile_sub4(i),conv_profile_sub5(i),conv_profile_sub6(i),$
	     conv_profile_sub7(i),conv_profile_sub8(i)
endfor
close,1

end
