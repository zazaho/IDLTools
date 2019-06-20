pro sh_writelines,data,filenaam
  if (strpos(filenaam,'.') eq -1) then filenaam = filenaam+'.ascii'
  openw,lun,filenaam,/get_lun
  n = n_elements(data)
  writeu,lun,'#gmaxx1         gmaxy2          gwidth3         ', $
    'gflux4          maxx5           maxy6           ', $
    'flux7           meancont8       starcont9       ', $
    'rest10          lo11    hi12    band13'
  printf,lun,''
  for i = 0,n-1 do begin
    writeu,lun,STRING( $
                       FORMAT='(T1,F10.7,T16,E11.5E2,T32,E11.5E2,'+ $
                       'T48,E11.5E2,T64,F10.7,T80,E11.5E2,T96,E11.5E2,'+ $
                       'T112,E11.5E2,T128,F10.7,T144,A)' $
                       ,data[i].gmaxx $
                       ,data[i].gmaxy $
                       ,data[i].gwidth $
                       ,data[i].gflux $
                       ,data[i].maxx $
                       ,data[i].maxy $
                       ,data[i].flux $
                       ,data[i].meancont $
                       ,data[i].starcont $
                       ,data[i].remark $
                     )
    printf,lun,''
  endfor
  close,lun
  free_lun,lun
end
