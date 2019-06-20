Hi Rien en Sacha,

Ik had ooit nog beloofd om brouwsels met betrekking tot TIMMI2 reductie 
van images naar jullie op te sturen. Omdat ik er nog niet van overtuigd 
ben dat bij zwakke bronnen veel kan winnen door de individuele subimages 
te schuiven voordat je optelt, heb ik me daar niet zo mee beziggehouden 
(die programma's zijn allemaal in de VS gedeleted). Ik heb hier een 
programma geschreven om de positieve en negatieven images (in 1 image) 
op te tellen (adding.pro). De andere twee programmaatjes worden door dit 
programma aangeroepen, maar zijn verder niet echt heel erg interessant.
Adding.pro werkt door middel van het klikken in de buurt van de pieken 
en dalen. Het bepaald zelf (dmv een 2D gaussfit) waar de werkelijk 
piek/dal is en ook of het positief of negatief is. Als je vantevoren al 
weet wat de afstand (in pixels) tussen 2 images moet zijn kan je dat 
ingeven, maar je kan het ook het programma laten bepalen (waarbij erdan 
vanuit wordt gegaan dat het in een bepaalde richting overal gelijk is).  
Maar je kan ook gewoon alleen de pieken bepalen en op basis daarvan 
verplaatsen.

Zoals gewoonlijk wordt het programma zonder garantie geleverd, 
opmerkingen e.d. zijn altijd welkom.

Cheers,

Frank Molster

ESTEC/ESA
Research and Scientific Support Department (ST)
Keplerlaan 1
NL-2201 AZ
Noordwijk
The Netherlands
 
Phone: +31 (0)71 565 3783
FAX:   +31 (0)71 565 4690
Email: frank.molster@esa.int


    [ Part 2: "Attached Text" ]

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

    [ Part 3: "Attached Text" ]

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

    [ Part 4: "Attached Text" ]

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
