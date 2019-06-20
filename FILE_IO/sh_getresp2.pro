function sh_getresp2,band

  dets = band2det(band)

  resp = readcal('25_'+band)
  
  ;;now we create an aar with the require amount of elements
  npoints = n_elements(resp.data)
  ndets = n_elements(dets)
  
  out = define_aar(length=(npoints)*(ndets))
  ;; and fill it with lambda, respons, det and sigma values
  FOR i=0,ndets-1 DO BEGIN    
    out.data(i*npoints:(i+1)*npoints-1).det = dets(i)
    foo = execute('out.data(i*npoints:(i+1)*npoints-1).wave  = resp.data.lambda'+strtrim(string(format='(I2.2)',dets(i)),2))
    foo = execute('out.data(i*npoints:(i+1)*npoints-1).flux  = resp.data.respons'+strtrim(string(format='(I2.2)',dets(i)),2))
    foo = execute('out.data(i*npoints:(i+1)*npoints-1).stdev = resp.data.sigma'+strtrim(string(format='(I2.2)',dets(i)),2))
  ENDFOR
  
  ;throw out some bad points
  out = sh_select(out,out.data.wave NE 0)
  out = sh_select(out,out.data.flux NE 0)
  ;; and sort the shit
  idx = sort(out.data.wave)
  out.data.wave = out.data[idx].wave
  out.data.det  = out.data[idx].det 
  out.data.flux = out.data[idx].flux
  out.data.stdev= out.data[idx].stdev
  
  ;;reset the scan dir
  out.data.sdir = 1
  out.data.line = band2line(band)
  
  out.header=write_fits_key( out.header,'OBJECT','RSRF BAND:'+band,'S','',s)
  
  return,out
end
