; function sh_cleanaar
; 
; Function to clean bad points from an aar.
; Just present all bands, up and down seperate and let user select
; bad points 
; xmode can be set to 1 and then the box is taken to be two vertical lines
; between which everything is selected
; ymode can be set to 1 and then the box is taken to be two horizontal lines 
; between every thing is selected
; scan can be 'up' or 'down' ( in wavelength )

FUNCTION sh_cleanaar,aar,band=band,scan=scan,xmode=xmode, $
                     ymode=ymode,name=name,_extra=_extra

  device,set_graphics=3
  
; Check for valid input
  IF NOT is_aar(aar) THEN error,'F','No valid AAR structure specified!'
  
  out = aar
  
  IF NOT keyword_set(band) THEN BEGIN
    band = ['1a','1b','1d','1e','2a','2b','2c',$
             '3a','3c','3d','3e','4']
  ENDIF ELSE BEGIN
    IF (band EQ '1') THEN band = ['1a','1b','1d','1e']
    IF (band EQ '2') THEN band = ['2a','2b','2c']
    IF (band EQ '3') THEN band = ['3a','3c','3d','3e']
  ENDELSE	
  
  ;; Now we take only bands requested AND present in the aar
  availablebands=sh_whichband(aar)
  foo = sh_comparray(availablebands,band,which=bnd,where=wherebnd)
  line = (sh_uniq(aar.data.line))[wherebnd]
  
  IF n_elements(bnd) EQ 0 THEN BEGIN
    print,'SH_CLEANAAR: No bands to be done'
    return,aar
  ENDIF
    ;; The line numbers belonging to the bnds array
  line = line(wherebnd)

;check which scans are present and wanted
  default,scan,['down','up']
  dodown = ((where(out.data.sdir EQ  1))[0] NE -1) AND sh_comparray('down',scan)
  doup   = ((where(out.data.sdir EQ -1))[0] NE -1) AND sh_comparray('up',scan)
  CASE 1 OF
    doup AND dodown: BEGIN
      scan=['down','up']
      sdir=[1,-1]
    END
    dodown: begin 
      scan=['down']
      sdir=[1]
    END
    doup: BEGIN
      scan=['up']
      sdir=[-1]
    END
    ; This is of course nonsense but anyhow:
    ELSE: BEGIN
      print,'SH_CLEANAAR: No valid scan'
      return,aar
    END
  ENDCASE
  
  IF keyword_set(xmode) THEN use_y = 0 ELSE use_y = 1
  IF keyword_set(ymode) THEN use_x = 0 ELSE use_x = 1
  
  FOR i = 0,n_elements(bnd)-1 DO BEGIN
    FOR j = 0,n_elements(scan)-1 DO BEGIN
      dets = sh_uniq((sh_band(out,bnd[i])).data.det)
      FOR k = 0L,n_elements(dets)-1 DO BEGIN
        jumpback1:
        pl,out,aband=line[i],ascan=scan[j],adet=dets[k],title=bnd[i]+','+scan[j]+','+n2s(dets[k]), $
          /nodata,_extra=_extra
        pl,out,aband=line[i],ascan=scan[j],ndet=dets[k],psym=0,/d,col=!p.color*.3,/oplot
        pl,out,aband=line[i],ascan=scan[j],adet=dets[k],psym=-1,thi=3,/w,/oplot
        retry:
        print,'Now select bad data by dragging a box,middle to skip detector,right to skip scan'
        p = select_in_plot(/data,button=btn)
        ; The right button pressed so skip scan
        IF btn EQ 4 THEN k = n_elements(dets)-1
        IF btn EQ 1 THEN BEGIN
          IF (n_elements(p) NE 4) THEN GOTO,retry
          x0 = min(p(0,*),max=x1)
          y0 = min(p(1,*),max=y1)
          tmp = sh_select(out, $
                          (out.data.line NE line[i]) OR $
                          (out.data.sdir NE sdir[j]) OR $
                          (out.data.det NE dets[k]) OR $
                          ((out.data.wave LT x0) AND (use_x EQ 1)) OR $
                          ((out.data.wave GT x1) AND (use_x EQ 1)) OR $
                          ((out.data.flux LT y0) AND (use_y EQ 1)) OR $
                          ((out.data.flux GT y1) AND (use_y EQ 1)))
          pl,tmp,aband=line[i],ascan=scan[j],adet=dets[k],psym=-1,/opl
          print,'left=accept,middle=decline,right=next detector'
          cursor,x,y,2,/down
          IF !err EQ 1 THEN BEGIN
            out = tmp
          ENDIF
          IF !err NE 4 THEN GOTO,jumpback1
        ENDIF
      ENDFOR 
    ENDFOR
  ENDFOR
  
  sh_aardiff,aar,out,name=name
  return,out
END
