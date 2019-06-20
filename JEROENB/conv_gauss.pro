;  conv_gauss.pro: IDL function 
;
;  Inputs: Image0 (Xdim,Ydim), fwhm
;  Output: Image (Xdim,Ydim) = Image0 convolved with Gaussian of fwhm
;
FUNCTION P_CONV_GAUSS, Image0, fwhm
COMMON SHARE_GRID, XDim, YDim, XRef, YRef, XVal, YVal, Xinc, Yinc
;
Image  = Image0
IF (((size(Image))(1) NE XDim) OR ((size(Image))(2) NE YDim)) THEN BEGIN
	print, ' Error on image sizes '
        return, Image0
ENDIF
;
gauss0 = fltarr(Xdim,Ydim)
xarray = fltarr(Xdim,Ydim)
yarray = fltarr(Xdim,Ydim)
arg_exp = fltarr(Xdim,Ydim)
linx = findgen(Xdim)
liny = findgen(Ydim)
for j = 0,Ydim-1 do xarray(*,j) = linx
for i = 0,Xdim-1 do yarray(i,*) = liny
xarray = (xarray - xref)*xinc + xval
yarray = (yarray - yref)*yinc + yval
Rarray = sqrt(xarray^2 + yarray^2)
R2array = xarray^2 + yarray^2
arg_exp = 4.*alog(2.)*R2array/fwhm^2
gauss0(where(arg_exp le 86.)) = exp(-arg_exp(where(arg_exp le 86.)))
xarray = 0.
yarray = 0.
Rarray = 0.
arg_exp = 0.
ind0 = 0
;image0 = image
Image = convolve(Image0,gauss0)
;window, 0
;print, Total(Image)/Total(Image0)
;print, ' Total(gauss0) = ', Total(gauss0)
Image = Image/Total(gauss0)
;print, max(image0)
;print, min(image0)
;
pixel = xinc*yinc		 ; pixel solid angle in arcsec^2
beam  = !pi*fwhm^2/(4.*alog(2.)) ; beam solid angle in arcsec^2
print, ' Total(image) (Jy) = ', total(image0)*pixel/beam
;
;print, max(image)
;print, min(image)
print, ' Total(conv_image) (Jy) = ', total(image)*pixel/beam
print, ' Max(conv_image) = ', max(image)
;
;loadct, 15
;wset, 0 & display, image 
;wset, 1 & display, gauss0
;wset, 2 & display, image0
;
return, Image
end










