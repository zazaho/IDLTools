;; function that determines the slope of the ratios as a function of
;; intensity
function findoffset_totalslope,P

  COMMON common_findoffsets, mean250,mean350,mean500
  
  slope250 = (poly_fit(alog10(mean500),(mean250+p[0])/(mean500+p[2]),1))[1]
  slope350 = (poly_fit(alog10(mean500),(mean350+p[1])/(mean500+p[2]),1))[1]

  return,abs(slope250)+abs(slope350)
END  
  
function findoffsets,i250,i350,i500

  COMMON common_findoffsets, mean250,mean350,mean500
  ;; assume that all maps are already regridded etc

  ;; first build some surface brightness map, lets use 500 as a
  ;; reference for all
  
  itot = finitise(i250*(500./250.)^(-2) +  i350*(500./350.)^(-2) + i500)

  ;; make some bins of brightness
  maxi = 5.0
  mini = 0.1
  irange = maxi/mini

  nbins = 10
  bins = mini*10^(dindgen(nbins+1)*alog10(irange)/nbins)

  mean250 = make_array(nbins,val=0d0)
  mean350 = mean250
  mean500 = mean250

  for i=0,nbins-1 do begin
     idx = where((itot gt bins[i]) and (itot le bins[i+1]),count)
     if count ne 0 then begin
        mean250[i] = mean(i250[idx])
        mean350[i] = mean(i350[idx])
        mean500[i] = mean(i500[idx])
     endif
  endfor

  R = AMOEBA(1.0d-8 , SCALE=2d-1, P0 = [0d0, 0d0,0d0], FUNCTION_NAME="findoffset_totalslope")  
  if n_elements(R) ne 3 then R = [0d0, 0d0,0d0]
  R = AMOEBA(1.0d-9 , SCALE=1d-1, P0 = R             , FUNCTION_NAME="findoffset_totalslope")  
  if n_elements(R) ne 3 then R = [0d0, 0d0,0d0]
  R = AMOEBA(1.0d-10, SCALE=5d-2, P0 = R             , FUNCTION_NAME="findoffset_totalslope")  
  if n_elements(R) ne 3 then R = [0d0, 0d0,0d0]
  
  IF N_ELEMENTS(R) EQ 1 THEN MESSAGE, 'AMOEBA failed to converge'  

  return,R

end
