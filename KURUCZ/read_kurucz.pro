FUNCTION read_hnu,num
  
  ;; The number of lines with fnu data
  datalines=153
  ;; Know the first model starts at 3748
  modstart = 12973L
  ;; Each model takes 7642 bytes
  modlength = 26928L
  
  modpos = modstart+(num-1)*modlength

  out = make_array(1221,value=0d0)

  openr,lun,shell_expand('~/IA_TOOL/KURUCZ/kurucz.dat'),/get_lun

  point_lun,lun,modpos

  A = ''

  FOR i = 0,datalines-2 DO BEGIN
      readf,lun,f1,f2,f3,f4,f5,f6,f7,f8,format='(8E11.4)'
;;      readf,lun,A
;;      stop
;;      print,A
      out[i*8:7+i*8] = [f1,f2,f3,f4,f5,f6,f7,f8]
  ENDFOR 
  readf,lun,f1,f2,f3,f4,f5,format='(5E11.4)'
  out[1216:1220] = [f1,f2,f3,f4,f5]

  close,lun
  free_lun,lun
  
  return,out[0:1220]
END 


FUNCTION read_kurucz,t,g,z,teff=teff,logg=logg,abund=abund,models=models, $
                     num=num,allwave=allwave
  
  IF NOT keyword_set(num) THEN BEGIN
      default,teff,7000
      default,logg,1.0
      default,abun,-1.0
      default,t,teff
      default,g,logg
      default,z,abun

      ;; For the layout of the main file
      datastart = 50
      linespermodel  = 99
      
      IF n_elements(models) EQ 0 THEN BEGIN
          ;; first read the model params:
          models = (read_ascii(shell_expand('~/IA_TOOL/KURUCZ/models.dat'))).field1
      ENDIF
      nums   = models[0,*]
      teffs  = models[1,*]
      loggs  = models[2,*]
      abuns  = models[3,*]
  
      ;; Now select the model we ask:
      idx = where(((teffs EQ t) AND (loggs EQ g) AND (abuns EQ z)),count)
      CASE count OF
          0: BEGIN
              print,'READ_KURUCZ: model with these params does not exist'
              return,0
          END 
          1: BEGIN
          END
          ELSE: BEGIN
              print,'READ_KURUCZ: multiple models with these params'
              print,'READ_KURUCZ: choosing the last one'
              idx = min(idx)
          END 
      ENDCASE
      num = (nums[idx])[0]
  ENDIF
  wave = (read_ascii(shell_expand('~/IA_TOOL/KURUCZ/kurucz.dat'), $
                     data_start=22,num_records=153)).field1
  ;; Convert to micrometer
  wave = reform(wave[0:1220],1221)*1d-3

  Hnu=read_hnu(num)
  ; convert to Fnu
  Fnu = 4*Hnu*1d26
  IF NOT keyword_set(allwave) THEN BEGIN
      idx = where(Fnu NE 0d0,count)
  ENDIF ELSE BEGIN
      idx = indgen(1221)
      count=1221
  ENDELSE  
  out = sh_define_aar(len=count)
  out.data.wave = wave[idx]
  out.data.flux = Fnu[idx]
  return,out
END 
