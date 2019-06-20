function fm_opelkaar,aar,band=refer,apply=apply,noprint=noprint

; Laat de factoren zien die nodig zijn om de band-edges opelkaar te krijgen
; if apply = 's' or 'S' then the shift is applied 
; if apply = 'f' or 'F' then the factor is applied 

; Check for valid input
if not is_aar(aar) then error,'F','No valid AAR structure specified!'

band=['1a','1b','1d','1e','2a','2b','2c','3a','3c','3d','3e','4']
lowlim=[2.30,2.57,2.995,3.5,4.02,5.29,6.96,12.0,16.0,19.33,27.4,29.45]
uplim =[2.605,3.045,3.7,4.08,5.55,7.03,12.3,16.4,19.53,27.6,29.6,46.0]
mediaanb = fltarr(12)
mediaane = fltarr(12)
correcf = fltarr(12)
correcs = fltarr(12)
gemb = fltarr(12)
geme = fltarr(12)
aaruit = aar
aartest = aar
test = fltarr(12)
tjop = fltarr(12)
if ( n_elements( noprint ) EQ 0 ) then teken = 1
if ( n_elements( noprint ) NE 0 ) then teken = 0
if ( n_elements( refer ) EQ 0 ) then refer = -1
if ( n_elements( apply ) EQ 0 ) then apply = 'n'

aar_nu = aar

for j=0,10 do begin
 
 ador = aot_to_apband(band(j))
 dets = band2det(ador(1))
 ndets = n_elements(dets)-1
 bool1 = (test_status(aar.data.status,aper=ador(0)) and $
 	 test_flag(aar.data.flag,order=ador(2)) and $
 	 test_flag(aar.data.flag,/nodata) eq 0 and $
 	 aar.data.det ge dets(0) and aar.data.det le dets(ndets))
 index = where(bool1 eq 1)
 i2 = where(aar.data(index).wave gt lowlim(j+1) and aar.data(index).wave lt uplim(j))
 geme(j) = total(aar.data(index(i2)).flux)/n_elements(i2)
 mediaane(j) = median(aar.data(index(i2)).flux)
 if (teken) then begin
   print
   print,'integratie',lowlim(j+1),' tot',uplim(j),' voor band ',band(j),' is:',geme(j)
 endif 

 ador = aot_to_apband(band(j+1))
 dets = band2det(ador(1))
 ndets = n_elements(dets)-1
 bool1 = (test_status(aar.data.status,aper=ador(0)) and $
 	 test_flag(aar.data.flag,order=ador(2)) and $
 	 test_flag(aar.data.flag,/nodata) eq 0 and $
 	 aar.data.det ge dets(0) and aar.data.det le dets(ndets))
 index = where(bool1 eq 1)
 i2 = where(aar.data(index).wave gt lowlim(j+1) and aar.data(index).wave lt uplim(j))
 gemb(j+1) = total(aar.data(index(i2)).flux)/n_elements(i2)
 mediaanb(j+1) = median(aar.data(index(i2)).flux)
 if (teken) then begin
   print,'integratie',lowlim(j+1),' tot',uplim(j),' voor band ',band(j+1),' is:',gemb(j+1)
 
   print,'verhouding band ',band(j),' band ',band(j+1),'          =',geme(j)/gemb(j+1),'  1/x =',gemb(j+1)/geme(j)
   print,'(mediaan) verhouding band ',band(j),' band ',band(j+1),'=',mediaane(j)/mediaanb(j+1),'  1/x =',mediaanb(j+1)/mediaane(j)
 endif
endfor

print
gemb(0) = geme(0)
geme(11) = gemb(11)
for j=0,11 do begin
 if (band(j) EQ refer) then begin
  correcf(j)=1
  correcs(j)=0
  test(j) = 0
  if (j GT 0) then begin
      for k=j-1,0,-1 do begin
        correcf(k) = correcf(k+1)*gemb(k+1)/geme(k)
        correcs(k) = correcs(k+1) + gemb(k+1) - geme(k)
        test(j) = test(j) + abs(correcs(j))
     endfor
  endif
  if (j LT 11) then begin
      for k=j+1,11 do begin
        correcf(k) = correcf(k-1)*geme(k-1)/gemb(k)
        correcs(k) = correcs(k-1) + geme(k-1) - gemb(k)
        test(j) = test(j) + abs(correcs(j))
      endfor
  endif
  tjop(j) = total(correcf)/12
  if ( refer NE -1 ) then  begin
       for jk=0,11 do begin
         print,'band ',band(jk),' krijgt de volgende factor correctie:',correcf(jk)
       endfor
         print,'gemiddeld =',total(correcf)/12
       print
       for jk=0,11 do begin
         print,'band ',band(jk),' krijgt de volgende schuif correctie:',correcs(jk)
         print,'Dat is ongeveer',100*correcs(jk)/((gemb(jk)+geme(jk))/2),'%'
       endfor
    if (apply eq 'F' or apply eq 'f') then begin
       for jk=0,11 do begin
          ador = aot_to_apband(band(jk))
          dets = band2det(ador(1))
          ndets = n_elements(dets)-1
          bool1 = (test_status(aar.data.status,aper=ador(0)) and $
 	    test_flag(aar.data.flag,order=ador(2)) and $
 	    test_flag(aar.data.flag,/nodata) eq 0 and $
 	    aar.data.det ge dets(0) and aar.data.det le dets(ndets))
          index = where(bool1 eq 1)
          i2 = where(aar.data(index).wave gt lowlim(jk) and aar.data(index).wave lt uplim(jk))
          aaruit.data(index(i2)).flux  = aartest.data(index(i2)).flux * correcf(jk)
        endfor
     endif                 
    if (apply eq 'S' or apply eq 's') then begin
       for jk=0,11 do begin
          ador = aot_to_apband(band(jk))
          dets = band2det(ador(1))
          ndets = n_elements(dets)-1
          bool1 = (test_status(aar.data.status,aper=ador(0)) and $
 	    test_flag(aar.data.flag,order=ador(2)) and $
 	    test_flag(aar.data.flag,/nodata) eq 0 and $
 	    aar.data.det ge dets(0) and aar.data.det le dets(ndets))
          index = where(bool1 eq 1)
          i2 = where(aar.data(index).wave gt lowlim(jk) and aar.data(index).wave lt uplim(jk))
          aaruit.data(index(i2)).flux  = aartest.data(index(i2)).flux + correcs(jk)
        endfor
     endif                 
  endif
 endif
endfor
   	
print
return,aaruit

end

