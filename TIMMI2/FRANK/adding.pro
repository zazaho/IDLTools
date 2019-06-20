function adding,img_in,posneg=posneg,size=size,stepx=stepx,stepy=stepy,autom=autom,glad=glad,nostep=nostep

; to add the images and also finds out whether they are positive or negative
; It also automatically can detect the right distances between the supposed to
; be images. 
; img_in = image in
; posneg = 0: is only positive image
; posneg = 1: is positive + 1 negative image 
; posneg = 2: is positive + 2 negative images
; posneg = 3: is 2 positive + 2 negative images
; autom = automatic determination of the distances, assuming an equal distance between the different images.
; size = the size of the final image in pixels (if possible otherwise it will make it smaller itself until the maximum size)
; stepx = predetermined step in the X-direction for the difference in x position between the different images
; stepy = predetermined step in the Y-direction for the difference in x position between the different images
; glad = smoothing option, determines number of pixels smoothed
; nostep = no determination of the steps, but simply calculated on the spot.

default,glad,5
default,posneg,2
default, size,50
default,stepx,1
default,stepy,1

if (posneg lt 0) then posneg = abs(posneg)

img = fltarr(50+n_elements(img_in(*,0)),50+n_elements(img_in(0,*)))
img(24:(23+n_elements(img_in(*,0))),24:(23+n_elements(img_in(0,*)))) = img_in


;factor=1

tvs,img,factor

x = fltarr(posneg+1)
y = fltarr(posneg+1)
optelx = 0
optely = 0

for kk = 0,posneg do begin
  print,'Click on the ',kk+1,'-e peak/valley of the image'
  cursor,a,b,1,/dev 
  x(kk) = fix((a/factor)+0.5)
  y(kk) = fix((b/factor)+0.5)
  print,x(kk),y(kk)
  wait, 0.5
endfor

ymax= max(y)
ymin= min(y)

print,size,ymin,ymax,n_elements(img[0,*])
if ((n_elements(img[0,*])- ymax) gt (size + 3)) then begin
  if (size lt (ymin - 3)) then begin
    nr = size
  endif else begin
    nr = ymin - 3
  endelse
endif else begin
    nr = (n_elements(img[0,*])- 3 - ymax)
endelse
print,'size =',nr


img_deel = fltarr(posneg+1,2*nr+1,2*nr+1)
img_out = fltarr(2*nr+1,2*nr+1)
;print,x,y

if (n_elements(autom) NE 0) then begin
  tnr = nr
  xaantal = 0
  yaantal = 0
  xverschil = 0
  yverschil = 0
  for i1 = 0,posneg-1 do begin
    pl_sm1 = smooth(img[x(i1)-tnr:x(i1)+tnr,y(i1)-tnr:y(i1)+tnr],glad)
    plx1 = n_elements(pl_sm1[*,0])
    ply1 = n_elements(pl_sm1[0,*])
    pl_smd1 = abs(pl_sm1[glad:plx1-glad-2,glad:ply1-glad-2])
    result1 = gauss2dfit(pl_smd1,p1,/tilt)
    nrx1 = p1(4) + x(i1) - tnr + glad
    nry1 = p1(5) + y(i1) - tnr + glad
    while ((sqrt((x(i1) - nrx1)^2 + (y(i1) - nry1)^2) gt 10) and (tnr gt 10)) do begin
      print,x(i1),nrx1,tnr
      print,y(i1),nry1,tnr
      tnr = tnr -1
      pl_sm1 = smooth(img[x(i1)-tnr:x(i1)+tnr,y(i1)-tnr:y(i1)+tnr],glad)
      plx1 = n_elements(pl_sm1[*,0])
      ply1 = n_elements(pl_sm1[0,*])
      pl_smd1 = abs(pl_sm1[glad:plx1-glad-2,glad:ply1-glad-2])
      result1 = gauss2dfit(pl_smd1,p1,/tilt)
      nrx1 = p1(4) + x(i1) - tnr + glad
      nry1 = p1(5) + y(i1) - tnr + glad
    endwhile
    if (tnr EQ 10) then begin
      nrx1 = x(i1)
      nry1 = y(i1)
    endif
    tnr = nr
    print,'Xposities',x(i1),nrx1
    print,'Yposities',y(i1),nry1
    if ((abs(p1(4)) lt plx1) and (abs(p1(5)) lt ply1)) then begin
      x(i1) = nrx1
      y(i1) = nry1
      for i2 = i1+1,posneg do begin
  	print,i1,i2
  	pl_sm2 = smooth(img[x(i2)-tnr:x(i2)+tnr,y(i2)-tnr:y(i2)+tnr],glad)
  	plx2 = n_elements(pl_sm2[*,0])
  	ply2 = n_elements(pl_sm2[0,*])
  	pl_smd2 = abs(pl_sm2[glad:plx2-glad-2,glad:ply2-glad-2])
  	result2 = gauss2dfit(pl_smd2,p2,/tilt)
  	nrx2 = p2(4) + x(i2) - tnr + glad
  	nry2 = p2(5) + y(i2) - tnr + glad
    	while ((sqrt((x(i2) - nrx2)^2 + (y(i2) - nry2)^2) gt 10) and (tnr gt 10)) do begin
    	  print,x(i2),nrx2,tnr
    	  print,y(i2),nry2,tnr
    	  tnr = tnr -1
    	  pl_sm2 = smooth(img[x(i2)-tnr:x(i2)+tnr,y(i2)-tnr:y(i2)+tnr],glad)
    	  plx2 = n_elements(pl_sm2[*,0])
    	  ply2 = n_elements(pl_sm2[0,*])
    	  pl_smd2 = abs(pl_sm2[glad:plx2-glad-2,glad:ply2-glad-2])
    	  result2 = gauss2dfit(pl_smd2,p2,/tilt)
    	  nrx2 = p2(4) + x(i2) - tnr + glad
    	  nry2 = p2(5) + y(i2) - tnr + glad
    	endwhile
        if (tnr EQ 10) then begin
          nrx2 = x(i2)
          nry2 = y(i2)
        endif
        tnr = nr
        print,'Xposities',x(i2),nrx2
        print,'Yposities',y(i2),nry2
  	if ((abs(p2(4)) lt (plx2- 2*glad)) and (abs(p2(5)) lt (ply2-2*glad))) then begin
          x(i2) = nrx2
          y(i2) = nry2
  	  if (abs(nrx1 - nrx2) gt .7*stepx) then begin     
  	     xaantal = xaantal + 1
	     if (abs(nrx1 - nrx2) gt stepx*1.5) then begin
               xbij = abs(nrx1 - nrx2)/2.
             endif else begin
               xbij = abs(nrx1 - nrx2)
             endelse
  	     xverschil = xverschil + xbij
  	     print,'x',xaantal,xverschil
  	  endif
  	  if (abs(nry1 - nry2) gt .7*stepy) then begin     
  	     yaantal = yaantal + 1
	     if (abs(nry1 - nry2) gt stepy*1.5) then begin
               ybij = abs(nry1 - nry2)/2.
             endif else begin
               ybij = abs(nry1 - nry2)
             endelse
  	     yverschil = yverschil + ybij
  	     print,'y',yaantal,yverschil
  	  endif
        endif 
      endfor
    endif
  endfor
  if (xaantal NE 0) then stepx = fix(xverschil/xaantal + 0.5) 
  if (yaantal NE 0) then stepy = fix(yverschil/yaantal + 0.5) 
endif 

if (N_elements(nostep) EQ 0) then begin
  print,'Step X=',stepx,' Step Y=',stepy

  for ijk = 0,posneg do begin
    optelx = (x(ijk) mod stepx) + optelx
    optely = (y(ijk) mod stepy) + optely
  endfor
  xgem = fix(optelx/(posneg+1) + 0.5)
  ygem = fix(optely/(posneg+1) + 0.5)
  print,'X(0)=',xgem,': Y(0)=',ygem
  
  
  for jj = 0,posneg do begin
    stapjesx = fix((x(jj) - xgem)/stepx + 0.5)
    stapjesy = fix((y(jj) - ygem)/stepy + 0.5)
    xpos = xgem + stapjesx * stepx  
    ypos = ygem + stapjesy * stepy  
    print,'X positie calc=',xpos,' X positie click=', x(jj)
    print,'Y positie calc=',ypos,' Y positie click=', y(jj)
    img_deel = img[xpos-nr:xpos+nr,ypos-nr:ypos+nr]
    img_smooth = smooth(img_deel,glad)
    aanx = n_elements(img_smooth[*,0])
    aany = n_elements(img_smooth[0,*])
    img_smooth = img_smooth - median(img_smooth)
    img_smoothd = img_smooth[glad:aanx-glad-1,glad:aany-glad-1]
    img_smoothdeel = abs(img_smoothd)
    result1 = gauss2dfit(img_smoothd,A,/tilt)
    result2 = gauss2dfit(img_smoothdeel,B,/tilt)
    samen2 = naast(img_smoothd,result2,/show)
    if (abs(A(1)- B(1)) lt 0.1*A(1)) then begin 
      print,'image optellen'
      img_out = img_out + img_deel(*,*)
    endif else begin
      print,'image aftrekken'
      img_out = img_out - img_deel(*,*)
    endelse
  endfor
endif else begin
  for jj = 0,posneg do begin
    img_deel = img(x(jj)-nr:x(jj)+nr,y(jj)-nr:y(jj)+nr)
    img_smooth = smooth(img_deel,glad)
    aanx = n_elements(img_smooth[*,0])
    aany = n_elements(img_smooth[0,*])
    img_smooth = img_smooth - median(img_smooth)
    img_smoothd = img_smooth[glad:aanx-glad-1,glad:aany-glad-1]
    img_smoothdeel = abs(img_smoothd)
    result1 = gauss2dfit(img_smoothd,A,/tilt)
    result2 = gauss2dfit(img_smoothdeel,B,/tilt)
    samen2 = naast(img_smoothd,result2,/show)
    if (abs(A(1)- B(1)) lt 0.1*A(1)) then begin 
      print,'image optellen'        
      xnul = fix(x(jj) -nr + glad + A(4) + 0.5)
      ynul = fix(y(jj) -nr + glad + A(5)+0.5)
      print,'X positie calc=',xnul,' X positie click=', x(jj)
      print,'Y positie calc=',ynul,' Y positie click=', y(jj)
      img_out = img_out + img(xnul-nr:xnul+nr,ynul-nr:ynul+nr)
    endif else begin
      print,'image aftrekken'
      xnul = fix(x(jj) -nr + glad + A(4) + 0.5)
      ynul = fix(y(jj) -nr + glad + A(5)+0.5)
      print,'X positie calc=',xnul,' X positie click=', x(jj)
      print,'Y positie calc=',ynul,' Y positie click=', y(jj)
      img_out = img_out - img(xnul-nr:xnul+nr,ynul-nr:ynul+nr)
    endelse
  endfor
endelse

img_src = img_out
tvs,img_src
return,img_src

end
