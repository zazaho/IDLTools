;; Simple wrapper to make psfiles with standard sizes
PRO shps,f,filename=filename,font=font,aa=aa, $
          a4port=a4port,a4land=a4land,nott=nott,loadtable=loadtable, $
          fixbb=fixbb,thick=thick,_extra=_extra
  
  COMMON COMMON_shps, old_r, old_g, old_b, table_loaded
  
;; This toggles if the device is open close  
  IF !d.name eq 'PS' THEN BEGIN
      f = (fstat(!d.unit)).name
      device,/close_file
      print,'SHPS: Closed file: '+f
      IF keyword_set(fixbb) THEN BEGIN
          
          ext_pos = strpos(f,'.ps')
          spawn,'psfix '+f
          spawn,'psfixbb -ef '+f
          IF ext_pos NE -1 THEN f = strmid(f,0,ext_pos)+'.eps'
          
      ENDIF
      set_plot,'x'
      IF table_loaded THEN BEGIN
          tvlct,old_r,old_g,old_b
      ENDIF
      cleanplot
      return
  ENDIF
  
  IF n_params() EQ 0 THEN BEGIN
      IF keyword_set(filename) THEN BEGIN
          f = filename
      ENDIF ELSE begin
          print,'SHPS: No filename given using default filename: idl.ps'
          f = 'idl.ps'
      ENDELSE
  ENDIF
  
  IF NOT keyword_set(font) THEN font = 'Times'
  
  CASE 1 OF
      keyword_set(a4port):suffix=',xsize=19.0,ysize=26.0,yoffset= 2.0,xoffset=1.0,/portrait'
      keyword_set(a4land):suffix=',xsize=26.0,ysize=19.0,yoffset= 28.0,xoffset=1.0,/landscape'
      keyword_set(aa): BEGIN
          suffix   =',xsize= 8.8,ysize= 8.8,yoffset=10.0,xoffset=5.0,/portrait'
          !p.region=[0.01,0.01,.99,.99]
          IF !p.charsize THEN tpchar = !p.charsize ELSE tpchar = 1.0
          IF !x.charsize THEN txchar = !x.charsize ELSE txchar = 1.0
          IF !y.charsize THEN tychar = !y.charsize ELSE tychar = 1.0
          !x.margin=[1.1*3*tpchar*txchar,0]
          !y.margin=[1.1*3*tpchar*tychar,0]
      END
      ELSE: suffix = ''
  ENDCASE
  
  IF NOT keyword_set(nott) THEN BEGIN
      !P.FONT = 0
      suffix = suffix+',set_font=font,/TT_FONT'
  ENDIF
  
  set_plot, 'ps'
  foo = execute('device,filename=f'+suffix+',_extra=_extra')
  print,'SHPS: Opened file: '+f
  
  IF keyword_set(loadtable) THEN BEGIN
      tvlct,old_r,old_g,old_b,/get
      loadct,39,/silent
      table_loaded = 1
  ENDIF ELSE BEGIN
      table_loaded = 0
  ENDELSE
  
  IF keyword_set(thick) THEN BEGIN
     !p.thick=thick
     !x.thick=thick
     !y.thick=thick
  ENDIF
  
END
