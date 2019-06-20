; circav.pro: IDL function that returns a circularly-symmetric image corresponding to the circular average of
;             the 2-D input image.
;
function circav_ima, image, xref = xref, yref = yref, radprof = radprof
;
Xdim = (size(image))(1)
Ydim = (size(image))(2)
;
ndim = min([Xdim,Ydim])
xinc = 1.
yinc = 1.
if not keyword_set(xref) then xref = (Xdim-1.)/2.
if not keyword_set(yref) then yref = (Ydim-1.)/2.
help, xref, yref
xval = 0.
yval = 0.
x0 = 0.
y0 = 0.
;
if ((Xdim mod 2) eq 0) then begin
  ;even
  Xdimp = Xdim/2
endif else begin
  ;odd
  Xdimp = Xdim/2 + 1
endelse
;
if ((Ydim mod 2) eq 0) then begin
  ;even
  Ydimp = Ydim/2
endif else begin
  ;odd
  Ydimp = Ydim/2 + 1
endelse
;
ndimp = min([Xdimp,Ydimp])
;
circav_image = image*0.
radprof = fltarr(ndimp)
sample0 = fltarr(ndim,ndim)
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
R2array = xarray^2 + yarray^2
;
IF ((ndim mod 2) eq 0) THEN BEGIN
 ;
 ; 1) ndim is even
 ;
 for i = 0, ndimp-1 do begin
   rmin = (1./sqrt(2.)  + i-1.)*xinc
   rmax = (1./sqrt(2.)  + i)*xinc
   ind = where((Rarray gt rmin) and (Rarray le rmax), count)
;   print, 'Count: ',count
   sample = sample0
   if (count gt 0) then begin
     sample(ind) = image(ind)
     radprof(i) = total(sample)/count
     circav_image(ind) = radprof(i)  
    endif else begin
     radprof(i) = -1.
     circav_image(ind) = -1.     
   endelse
 endfor
ENDIF ELSE BEGIN
 ;
 ; 2) ndim is odd
 ;
  for i = 0, ndimp-1 do begin
   rmin = (i-0.5)*xinc
   rmax = (i+0.5)*xinc
   ind = where((Rarray gt rmin) and (Rarray le rmax), count)
;   print, 'Count: ',count
   sample = sample0
   if (count gt 0) then begin
     sample(ind) = image(ind)
     radprof(i) = total(sample)/count
     circav_image(ind) = radprof(i)     
    endif else begin
     radprof(i) = -1.
     circav_image(ind) = -1.     
   endelse
  endfor
ENDELSE
;
return, circav_image
end
