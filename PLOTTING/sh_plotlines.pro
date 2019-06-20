PRO sh_plotlines,YRANGE=yr,xrange=xr,LOWLIMIT=low,UPPLIMIT=up,$
                 LINESTYLE=linestyle,FILENAME=filename,model=model, $
                 infinity=inf,series=series,wave=wave, $
		 trans=trans,_extra=_extra

  default,linestyle,1
  IF !x.type EQ 0 THEN def_xr = !x.crange ELSE def_xr = 10d0^!x.crange
  IF !y.type EQ 0 THEN def_yr = !y.crange ELSE def_yr = 10d0^!y.crange
  default,xr,def_xr
  default,yr,def_yr
  default,low,10
  default,up ,40
  
  low = floor(low)
  up = floor(up)
  IF up LT 2 THEN up=2
  IF low GE up THEN low=up-1
  
  CASE 1 OF
      keyword_set(model): BEGIN 
          ;; we just use the formula:
          ;; 1/lambda = R*(1/low^2-1/up^2) with
          ;; R the rydberg constant
          R = 10.9677583 ;; mum^-1
          ;; now fill the levels
          ;; we make a matrix with low and up values
          mtrx = lindgen(low,up)
          mtrx_low = mtrx MOD low
          mtrx_up  = mtrx  /  low
          ;; which are the transitions we want ?
          idx = where(mtrx_up GT mtrx_low,count)
          llow = reform(mtrx_low[idx],count)+1
          lup =  reform(mtrx_up[idx],count)+1
          wave = 1d0/(R*(1d0/llow^2d0-1d0/lup^2d0))
          ;; Now select the lines that will be plotted
          idx = where((wave GE xr[0]) AND $
                      (wave LE xr[1]),count)
      END
      keyword_set(inf): BEGIN 
          ;; we just use the formula:
          ;; lambda_low_inf = low^2/R with
          ;; R the rydberg constant
          R = 10.9677583 ;; mum^-1
          ;; now fill the levels
          ;; we make an array low
          llow = indgen(low)+1
          lup = make_array(low,value='inf')
          wave = llow^2d0/R
          ;; Now select the lines that will be plotted
          idx = where((wave GE xr[0]) AND $
                      (wave LE xr[1]),count)
      END 
      ELSE: BEGIN
          default,filename,shell_expand('${HOME}/IA_FILES/hlijnen_wave_sorted.txt')
          lines = read_ascii(filename)
          wave = lines.field1[0,*]
          llow = lines.field1[1,*]
          lup  = lines.field1[2,*]
          ;; Now select the lines that will be plotted
          idx = where((wave GE xr[0]) AND $
                      (wave LE xr[1]) AND $
                      (llow LE low  ) AND $
                      (lup  LE up   ),count)
      END
  ENDCASE  
      
  IF count GT 0 THEN BEGIN
      wave = wave[idx]
      llow = llow[idx]
      lup  = lup[idx]
      trans = f2s(lup,0)+'-'+f2s(llow,0)

      sort_llow = llow[sort(llow)]
      uniq_low = sort_llow[uniq(sort_llow)]
      n_uniq_low = n_elements(uniq_low)
      IF keyword_set(series) THEN BEGIN
          cols=[!P.COLOR,!D.TABLE_SIZE*(.1+0.06*[8,13,5,1,11,6,2,10,3,14,9,4,12,7])]
          scolor = [cols,cols,cols,cols,cols,cols,cols,cols,cols]
      ENDIF ELSE BEGIN
          scolor = make_array(low+1,value=(!D.TABLE_SIZE-1))
      ENDELSE 

      FOR i=0L,count-1L DO BEGIN
          oplot,[wave[i],wave[i]],yr,linestyle=linestyle, $
            color=scolor[llow[i]],_extra=_extra
      ENDFOR
  ENDIF
  
END
