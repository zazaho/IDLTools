; circsym.pro: IDL function that generates a circularly symmetric
;             image from a linear radial profile.
;
function circsym, linr
;
ndim = (size(linr))(1)
;
Xdim = 2*ndim-1
Ydim = 2*ndim-1
xinc = 1.
yinc = 1.
xref = (Xdim-1.)/2.
yref = (Ydim-1.)/2.
xval = 0.
yval = 0.
x0 = 0.
y0 = 0.
;
rad = findgen(ndim)*xinc
;
image = fltarr(Xdim,Ydim)
;
xarray = fltarr(Xdim,Ydim)
yarray = fltarr(Xdim,Ydim)
linx = findgen(Xdim)
liny = findgen(Ydim)
for j = 0,Ydim-1 do xarray(*,j) = linx
for i = 0,Xdim-1 do yarray(i,*) = liny
xarray = (xarray - xref)*xinc + xval - x0
yarray = (yarray - yref)*yinc + yval - y0
Rarray = sqrt(xarray^2 + yarray^2)
;R2array = xarray^2 + yarray^2
;
;pind = floor(Rarray/xinc)
;image = linr(pind) + (Rarray - pind*xinc)*(linr(pind+1)-linr(pind))/xinc
;print, rad
;print, linr
;Rarray(where(Rarray gt linr(ndim-1))) = linr(ndim-1)
;Sort_Rarray = Rarray(sort(Rarray))
;image_vec = spline(rad,linr,Sort_Rarray)
;image_vec = image_vec(sort(sort(Rarray)))
;image(0:Xdim*Ydim-1) = image_vec(0:Xdim*Ydim-1)
;;image(where(Rarray gt linr(ndim-1))) = 0.
;
image = interpolate(linr,Rarray,/cubic)
;
;print, max(image)
;print, rarray(xref,yref)
;print, image(xref,yref)
;
;for i = 0, ndim-1 do begin
;  rmax = (i+0.5)*xinc
;  rmin = (i-0.5)*xinc
;  ind = where((Rarray gt rmin) and (Rarray le rmax), count)
;  if (count gt 0) then begin
;    image(ind) = linr(i)
;  endif
;endfor
;
return, image
end
