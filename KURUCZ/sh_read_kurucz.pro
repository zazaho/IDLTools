FUNCTION sh_read_kurucz,file,temp=temp,logg=logg,pad=pad

  default,pad,'/usr/local/MoDust_2.00/data/Kurucz/fp00k2/'
  default,temp,'10000'
  default,logg,'10'
  
  IF NOT keyword_set(file) THEN BEGIN
      ttemp = n2s(temp)
      tlogg = n2s(logg)
      file = 'kur_t'+ttemp+'_g'+tlogg+'.dat'
  ENDIF 

;;  foo = findfile(pad+file,count=fcount)
  foo = file_search(pad+file,count=fcount)
  
  IF fcount EQ 1 THEN BEGIN
      filename = pad+file
  ENDIF ELSE BEGIN 
;;      allmodels = findfile(pad)
      allmodels = file_search(pad)
      idx = where(strpos(allmodels,'_t') NE -1)
      allmodels = allmodels[idx]

      starttemp = strpos(allmodels,'_t')+2
      endtemp   = strpos(allmodels,'_g')
      lentemp = endtemp-starttemp
      ;; removed the end
      alltemp = (strmid(allmodels,starttemp,lentemp))[0,*]
      alltemp = long(reform(alltemp,n_elements(alltemp)))
      temps = sh_uniq(alltemp)
      print,temps
      print,'Choose a temperature:'
      temp_wanted = 0L
      read,temp_wanted
      
      idx = where(alltemp EQ temp_wanted) 
      allmodels = allmodels[idx]

      startlogg = strpos(allmodels,'_g')+2
      endlogg   = strpos(allmodels,'.dat')
      lenlogg = endlogg-startlogg
      ;; removed the end
      alllogg = (strmid(allmodels,startlogg,lenlogg))[0,*]
      alllogg = double(reform(alllogg,n_elements(alllogg)))/1d1
      loggs = sh_uniq(alllogg)
      print,loggs
      print,'Choose a logg:'
      logg_wanted = 0d0
      read,logg_wanted

      idx = where(alltemp EQ temp_wanted) 
      model = (allmodels[where(alllogg EQ logg_wanted)])[0]
      filename = pad+model
  ENDELSE
  out = read_fdat(filename,data_start=5)
  out.data.wave = out.data.wave*1d-3
  out.data.flux = 4d0*!dpi*out.data.flux*1d23
  return,out
END
;;      out = read_fdat(pad+file
