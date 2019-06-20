FUNCTION filtering,aar

  Flux=aar.data.flux
  n=n_elements(Flux)
  debruite=aar

  Y=fft(Flux,-1)
  Yf=alog10((abs(Y))^2)
  model=Yf[0:(n+1)/2]
  model[0]=Yf[0]
  for j=1,5 do model[j]=(total(Yf[j-1:j+1]))/3
  for j=6,20 do model[j]=(total(Yf[j-3:j+3]))/7
  for j=21,50 do model[j]=(total(Yf[j-5:j+5]))/11
  for j=51,(n+1)/2 do model[j]=(total(Yf[j-10:j+10]))/21
  modele=10^(model)
  btfs=10^(min(model))
  f=dblarr(n)

  f[0:(n+1)/2]=(modele-btfs)/modele
  if (n mod 2) eq 0 then f[((n+1)/2)+1:n-1]=f[-indgen((n-1)/2)+((n-1)/2)]
  if (n mod 2) ne 0 then f[(n/2)+1:n-1]=f[-indgen(n/2)+(n/2)]

  debruit=Y*f
  debruite.data.flux=float(fft(debruit,1))
 
return,debruite
end


PRO defilter,resultat2=resultat2,resutat3=resultat3,niste=niste

loop=0
for i=0,1 do begin
aar=read_faar(resultat2[loop])

aara=select(aar,aar.data.line eq 1)
aarau=select(aara,aara.data.sdir eq -1)
aarad=select(aara,aara.data.sdir eq 1)
aarauf=filtering(aarau)
aaradf=filtering(aarad)
aarafin=combine(aarauf,aaradf)

aarb=select(aar,aar.data.line eq 2)
aarbu=select(aarb,aarb.data.sdir eq -1)
aarbd=select(aarb,aarb.data.sdir eq 1)
aarbuf=filtering(aarbu)
aarbdf=filtering(aarbd)
aarbfin=combine(aarbuf,aarbdf)


aard=select(aar,aar.data.line eq 3)
aard=select(aard,aard.data.wave lt 3.8)
aardu=select(aard,aard.data.sdir eq -1)
aardd=select(aard,aard.data.sdir eq 1)
aarduf=filtering(aardu)
aarddf=filtering(aardd)
aardfin=combine(aarduf,aarddf)


C = STRMID(niste[loop],0,1)
  if (C eq '8') or (C eq '9') then begin
  aare=select(aar,aar.data.line eq 5)
  endif else aare=select(aar,aar.data.line eq 4)

aareu=select(aare,aare.data.sdir eq -1)
aared=select(aare,aare.data.sdir eq 1)
aareuf=filtering(aareu)
aaredf=filtering(aared)
aarefin=combine(aareuf,aaredf)

aarab=combine(aarafin,aarbfin)
aarde=combine(aardfin,aarefin)
aarae=combine(aarab,aarde)

write_fitstable,aarae,resultat3[loop]

loop=loop+1

endfor
end


