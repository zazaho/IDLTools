FUNCTION linear,arr
  return,reform(arr,n_elements(arr))
END

FUNCTION running_mean,x,y,width
  mean_y = y
  ny = n_elements(y)-1
  hw = (width-1.0)/2.0
  
  FOR i=0,ny DO BEGIN
      idx = where( (x GT x[i]-hw) AND (x LT x[i]+hw) )
      mean_y[i] = mean(y[idx])
  ENDFOR 
  return,mean_y
END

;--------------------------------------------------------------------
;
;  Purpose:  sh_disp_raster_plot
;
PRO sh_disp_raster_plot, sInfo
  
  WIDGET_CONTROL, sInfo.wXdetSlider, GET_VALUE=Xdet
  WIDGET_CONTROL, sInfo.wYdetSlider, GET_VALUE=Ydet
  cube_det = linear(sInfo.cube[Xdet,Ydet,*])
  mask_det = linear(sInfo.mask[Xdet,Ydet,*])

  unmasked = where((mask_det AND 1) NE 1)
  masked = where((mask_det AND 1) EQ 1)
  all = indgen(n_elements(cube_det))
  
  IF unmasked[0] EQ -1 THEN return

  IF !d.name EQ 'X' THEN BEGIN 
      wset,sInfo.PlotWindowID
  ENDIF 
  
  IF NOT sInfo.PlotOplot THEN BEGIN
      CASE sInfo.Ymode OF
          0: BEGIN
              yrange = [0,0]
          END
          1: BEGIN
              WIDGET_CONTROL, sInfo.wYminText, GET_VALUE=Ymin
              WIDGET_CONTROL, sInfo.wYmaxText, GET_VALUE=Ymax
              yrange = [Ymin,Ymax]
          END 
      ENDCASE 
      
      CASE sInfo.Xmode OF
          0: BEGIN
              xrange = [0,0]
          END
          1: BEGIN
              WIDGET_CONTROL, sInfo.wXminText, GET_VALUE=Xmin
              WIDGET_CONTROL, sInfo.wXmaxText, GET_VALUE=Xmax
              xrange = [Xmin,Xmax]
          END 
      ENDCASE

      pl,unmasked,cube_det[unmasked],xr=xrange,yr=yrange, $
        xtit='time',ytit='detector value', $
        title='Time history of pixel ('+n2s(Xdet)+','+n2s(Ydet)+')!C' $
        ,/nodata,ymargin=[4,4],/ystyle

      pl,[1,1],/nodata,/noclip,/oplot

      iframe = where( (sInfo.image_time GT !x.crange[0]) AND $
                      (sInfo.image_time LT !x.crange[1]) )
      
      nframe = n_elements(iframe)
      IF nframe GT 15 THEN BEGIN
          show_every_n = floor(nframe/10.)
          iframe = iframe[ show_every_n * indgen(nframe / show_every_n) + round((nframe MOD show_every_n)/2.)]
      ENDIF 
  
      xyouts,sInfo.image_time[iframe], iframe*0d0+1.05*!y.crange[1], $
        (strcompress(sindgen(n_elements(sInfo.image_time)),/remove_all))[iframe], $
        align=0.5
  ENDIF 
  
  IF sInfo.plotUnMask THEN BEGIN
      pl,unmasked,cube_det[unmasked],ps=1,/oplot
  ENDIF 
  
  IF sInfo.plotMask THEN BEGIN
      IF masked[0] NE -1 THEN pl,masked,cube_det[masked],ps=4,/opl
  ENDIF 

  IF sInfo.plotMean THEN BEGIN
      rm = running_mean(unmasked,cube_det[unmasked],20)
      pl,unmasked,rm,ps=0,/opl
  ENDIF

  IF sInfo.plotMeanAll THEN BEGIN
      rm = running_mean(all,cube_det,20)
      pl,all,rm,ps=0,/oplot,linestyle=2
  ENDIF 

  IF sInfo.plotImage THEN BEGIN
      image_det = linear(sInfo.image[Xdet,Ydet,*])
      pl,sInfo.image_time,image_det,ps=0,/opl
  ENDIF

  IF sInfo.plotMedian THEN BEGIN
      pl,!x.crange,median(cube_det[unmasked])*[1,1],/opl,ps=0,thi=2
  ENDIF 

  IF sInfo.plotBorders THEN BEGIN
      to = sInfo.image_to
      iframe = where( (sInfo.image_to GT !x.crange[0]) AND $
                      (sInfo.image_to LT !x.crange[1]) )
      
      FOR ii=0,n_elements(iframe)-1 DO BEGIN
          oplot,sInfo.image_to[iframe[ii]]*[1.,1.],!y.crange,linestyle=2
      ENDFOR 
  ENDIF 
  
  WIDGET_CONTROL, sInfo.wXminText, SET_VALUE=!x.crange[0]
  WIDGET_CONTROL, sInfo.wXmaxText, SET_VALUE=!x.crange[1]
  WIDGET_CONTROL, sInfo.wYminText, SET_VALUE=!y.crange[0]
  WIDGET_CONTROL, sInfo.wYmaxText, SET_VALUE=!y.crange[1]
END                             ;   of sh_disp_raster_DisplayNew

;--------------------------------------------------------------------
;
PRO sh_disp_raster_Cleanup, wTopBase
    WIDGET_CONTROL, wTopBase, GET_UVALUE=sInfo,/NO_COPY
    ;  Restore the previous plot font.
;    !P.FONT = sInfo.plotFont
;    !P.CHARSIZE = sInfo.previousChar
END   ; of sh_disp_raster_Cleanup

;--------------------------------------------------------------------
;
PRO sh_disp_raster_Event, sEvent
  ;; Which event
  WIDGET_CONTROL, sEvent.id, GET_UVALUE=eventUValue
  IF eventUValue EQ 'QUIT' THEN BEGIN
      ;; If 'QUIT' we quit but be careful since sInfo is no longer
      ;; available after destroy
      WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo
      *sInfo.ptrNewMask = sInfo.OldMask
      WIDGET_CONTROL, sEvent.top, /DESTROY
      return
  ENDIF 
  IF eventUValue EQ 'APPLY' THEN BEGIN
      ;; If 'APPLY' we quit but be careful since sInfo is no longer
      ;; available after destroy
      WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo
      *sInfo.ptrNewMask = sInfo.Mask
      WIDGET_CONTROL, sEvent.top, /DESTROY
      return
  ENDIF 
  
  WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo,/NO_COPY
  CASE eventUValue OF
      'DRAWING' : BEGIN
          foo = convert_coord(sEvent.x,sEvent.y,/device,/to_data)
          x = foo[0]
          y = foo[1]
          CASE sEvent.Type OF 
              0: BEGIN
                  sInfo.xCursor = x
                  sInfo.yCursor = y
                  sInfo.ButtonPressed = (sEvent.Press AND 1) + $
                    (sEvent.Press AND 2) + (sEvent.Press AND 4)/4*3
                  CASE sInfo.ButtonPressed OF
                      1: oplot,linestyle=1,[x,x],!y.crange
                      2: oplot,linestyle=1,!x.crange,[y,y]
                      3: oplot,linestyle=2,[x,x],!y.crange
                  ENDCASE 
              END 
              1: BEGIN 
                  xmin = min([x,sInfo.xCursor],max=xmax)
                  xmin = round(xmin)>0
                  xmax = round(xmax)<n_elements(sInfo.cube[0,0,*])-1
                  ymin = min([y,sInfo.yCursor],max=ymax)
                  CASE sInfo.ButtonPressed OF
                      1: BEGIN
                          WIDGET_CONTROL, sInfo.wXminText, SET_VALUE=xmin
                          WIDGET_CONTROL, sInfo.wXmaxText, SET_VALUE=xmax
                          WIDGET_CONTROL, sInfo.wXmodeButton, SET_DROPLIST_SELECT=1
                          sInfo.xmode=1
                          sh_disp_raster_plot, sInfo
                      END
                      2: BEGIN
                          WIDGET_CONTROL, sInfo.wYminText, SET_VALUE=ymin
                          WIDGET_CONTROL, sInfo.wYmaxText, SET_VALUE=ymax
                          WIDGET_CONTROL, sInfo.wYmodeButton, SET_DROPLIST_SELECT=1
                          sInfo.ymode=1
                          sh_disp_raster_plot, sInfo
                      END
                      3: BEGIN
                          WIDGET_CONTROL, sInfo.wXdetSlider, GET_VALUE=Xdet
                          WIDGET_CONTROL, sInfo.wYdetSlider, GET_VALUE=Ydet
                          m_idx = sInfo.mask_db.n_masks
                          sInfo.mask_db.mask[m_idx].x = Xdet
                          sInfo.mask_db.mask[m_idx].y = ydet
                          sInfo.mask_db.mask[m_idx].first = xmin
                          sInfo.mask_db.mask[m_idx].last  = xmax
                          sInfo.mask_db.n_masks = m_idx+1
                          sInfo.mask[xdet,ydet,xmin:xmax] = sInfo.mask[xdet,ydet,xmin:xmax] OR 1 
                          WIDGET_CONTROL, sInfo.wUndoButton, sensitive=1
                          WIDGET_CONTROL, sInfo.wStoreButton, sensitive=1
                          WIDGET_CONTROL, sInfo.wApplyButton, sensitive=1
                          WIDGET_CONTROL, sInfo.wRestartButton, sensitive=1
                          sh_disp_raster_plot, sInfo
                      END 
                      ELSE: BEGIN
                      END  
                  ENDCASE
              END 
          ENDCASE
      END                       ;  of  DRAWING
      'YMODE': BEGIN
          sInfo.Ymode = sEvent.Index
          sh_disp_raster_plot, sInfo
      END
      'XMODE': BEGIN
          sInfo.Xmode = sEvent.Index
          sh_disp_raster_plot, sInfo
      END
      'REPAINT': BEGIN
          sh_disp_raster_plot, sInfo
      END
      'PLOTUNMASK': BEGIN
          sInfo.plotUnMask = NOT sInfo.plotUnMask
          sh_disp_raster_plot, sInfo
      END
      'PLOTMEANALL': BEGIN
          sInfo.plotMeanAll = NOT sInfo.plotMeanAll
          sh_disp_raster_plot, sInfo
      END
      'PLOTMEAN': BEGIN
          sInfo.plotMean = NOT sInfo.plotMean
          sh_disp_raster_plot, sInfo
      END
      'PLOTMEDIAN': BEGIN
          sInfo.plotMedian = NOT sInfo.plotMedian
          sh_disp_raster_plot, sInfo
      END
      'PLOTBORDERS': BEGIN
          sInfo.plotBorders = NOT sInfo.plotBorders
          sh_disp_raster_plot, sInfo
      END
      'PLOTMASK': BEGIN
          sInfo.plotMask = NOT sInfo.plotMask
          sh_disp_raster_plot, sInfo
      END
      'PLOTOPLOT': BEGIN
          sInfo.plotOplot = NOT sInfo.plotOplot
      END
      'PLOTIMAGE': BEGIN
          sInfo.plotImage = NOT sInfo.plotImage
          sh_disp_raster_plot, sInfo
      END
      'RESTART': BEGIN
          sInfo.mask = sInfo.oldmask
          sh_disp_raster_plot, sInfo
      END 
      'STORE': BEGIN
          file=DIALOG_PICKFILE(file='raster_mask.txt', $
                               TITLE='Give filename for MASK commands',/WRITE)
          IF file NE '' THEN BEGIN
              openw,lun,file,/get_lun
              FOR i = 0,sInfo.mask_db.n_masks-1 DO BEGIN
                  x = sInfo.mask_db.mask[i].x
                  y = sInfo.mask_db.mask[i].y
                  t0 = sInfo.mask_db.mask[i].first
                  t1 = sInfo.mask_db.mask[i].last
                  mask_string = 'raster.mask['+n2s(x)+','+n2s(y)+ $
                    ','+n2s(t0)+':'+n2s(t1)+']'
                  printf,lun,mask_string+'='+mask_string+' OR 1'
              ENDFOR 
              close,lun
              free_lun,lun
          ENDIF ELSE BEGIN
              FOR i = 0,sInfo.mask_db.n_masks-1 DO BEGIN
                  x = sInfo.mask_db.mask[i].x
                  y = sInfo.mask_db.mask[i].y
                  t0 = sInfo.mask_db.mask[i].first
                  t1 = sInfo.mask_db.mask[i].last
                  mask_string = 'raster.mask['+n2s(x)+','+n2s(y)+ $
                    ','+n2s(t0)+':'+n2s(t1)+']'
                  printf,mask_string+'='+mask_string+' OR 1'
              ENDFOR 
          ENDELSE 
      END
      'PS': BEGIN
          file=DIALOG_PICKFILE(file='sh_disp_raster.ps', filter='*.ps', $
                               TITLE='Give filename for hardcopy',/WRITE)
          IF file NE '' THEN BEGIN
              sh_ps,file,/a4land,/color,bits=8
              sh_disp_raster_plot, sInfo
              sh_ps
          ENDIF
      END
      'UNDO': BEGIN
          sInfo.mask_db.n_masks = sInfo.mask_db.n_masks -1
          sInfo.mask = sInfo.oldmask
          n_masks = sInfo.mask_db.n_masks
          IF n_masks GT 0 THEN BEGIN
              FOR i=0,n_masks-1 DO BEGIN
                  x = sInfo.mask_db.mask[i].x
                  y = sInfo.mask_db.mask[i].y
                  xmin = sInfo.mask_db.mask[i].first
                  xmax = sInfo.mask_db.mask[i].last
                  sInfo.mask[x,y,xmin:xmax] = sInfo.mask[x,y,xmin:xmax] OR 1 
              ENDFOR 
          ENDIF ELSE BEGIN
              WIDGET_CONTROL, sInfo.wUndoButton, sensitive=0
              WIDGET_CONTROL, sInfo.wStoreButton, sensitive=0
              WIDGET_CONTROL, sInfo.wApplyButton, sensitive=0
              WIDGET_CONTROL, sInfo.wRestartButton, sensitive=0
          ENDELSE
          sh_disp_raster_plot, sInfo
      END 
      ELSE: BEGIN
          ;;
      END 
  ENDCASE
  WIDGET_CONTROL, sEvent.top, SET_UVALUE=sInfo,/NO_COPY
END

;--------------------------------------------------------------------
;
;   PURPOSE: Display an image and allow the cuts to be set interactively
;   also allows for zooming and a different powerlaw index
;
;   OPTIONS:
;               absolute=1 convert the image to absolute values [0]
;               colortable=n set colortable to n [1]
;
PRO sh_disp_raster, InRaster, absolute=absolute, colortable=colortable, $
                    out=out,resize=resize,cube=InCube,mask=InMask, $
                    image=InImage,times=InTimes,no_block=no_block
  
  ;;  Save the font
  ;;
;  plotFont = !P.FONT
;  previousChar = !P.CHARSIZE
;  !P.CHARSIZE = 8.0 / !D.X_CH_SIZE
  default,no_block,0

  ;;  Use hardware-drawn font.
  ;;
;;  !P.FONT=0
  
  IF n_elements(InRaster) EQ 0 THEN BEGIN
      IF n_elements(InCube) EQ 0 THEN BEGIN
          print,'no cube data from raster or cube=cube specified'
          return
      END
      cube = InCube
      IF n_elements(InMask) EQ n_elements(InCube) THEN BEGIN
          Mask =InMask
      ENDIF ELSE BEGIN
          mask=byte(cube*0)
      ENDELSE 
      IF (n_elements(InImage) NE 0) AND $
        (n_elements(InImage) EQ n_elements(InTimes)) THEN BEGIN
          image = InImage
          times = InTimes
          doimage = 1
      ENDIF ELSE BEGIN
          image=0
          times=0
          doimage=0
      ENDELSE 
  ENDIF ELSE BEGIN
      cube = InRaster.Cube
      mask = InRaster.Mask
      image= InRaster.Image
      times= (InRaster.To+InRaster.From)/2.0
      doimage = 1
  ENDELSE

  wTopBase = WIDGET_BASE(TITLE="Display Raster", /COLUMN, MAP=0, MBAR=barBase)
  
  ;;  Create the menu bar items
  ;;
  wFileButton = WIDGET_BUTTON(barBase, VALUE='Actions')
  
  wPSButton = WIDGET_BUTTON(wFileButton, VALUE='Hardcopy', UVALUE='PS',sensitive=1)
  wRestartButton = WIDGET_BUTTON(wFileButton, VALUE='Restart', UVALUE='RESTART',sensitive=0)
  wStoreButton = WIDGET_BUTTON(wFileButton, VALUE='Store mask commands', UVALUE='STORE',sensitive=0)

  IF no_block EQ 0 THEN BEGIN
      wApplyButton = WIDGET_BUTTON(wFileButton, VALUE='Close (Apply mask)', UVALUE='APPLY',sensitive=0)
  ENDIF ELSE BEGIN
      wApplyButton=WIDGET_BASE(wTopBase)
  ENDELSE 
  
  wQuitButton = WIDGET_BUTTON(wFileButton, VALUE='Quit (Old mask)', UVALUE='QUIT')
  ;;  Create the left, center and right bases
  ;;
  wTopRowBase = WIDGET_BASE(wTopBase, COLUMN=2)
  
;; Left pane  ---------------
  wLeftBase = WIDGET_BASE(wTopRowBase, /COLUMN, /FRAME)

  wPixBase = WIDGET_BASE(wLeftBase, COLUMN=1,/FRAME)
  wPixLabel = WIDGET_LABEL(wPixBase, VALUE='Pixel')
  wXdetSlider = WIDGET_SLIDER(wPixBase, MINIMUM=0, MAXIMUM=31, $
                              VALUE=16, TITLE='X-Pixel',  $
                              UVALUE='REPAINT')
  wYdetSlider = WIDGET_SLIDER(wpixBase, MINIMUM=0, MAXIMUM=31, $
                              VALUE=16, TITLE='Y-Pixel',  $
                              UVALUE='REPAINT')

  wYBase = WIDGET_BASE(wLeftBase, COLUMN=1,/FRAME)
  wYLabel = WIDGET_LABEL(wYBase, VALUE='Y')
  wYmodeButton = WIDGET_DROPLIST(wYBase, $
                       VALUE=['Full Range','Fixed'], UVALUE='YMODE')
  wYminText = CW_FIELD(wYBase, /floating, VALUE=0, Title='min', $
                       UVALUE='REPAINT',xsize=10,/return_events)
  wYmaxText = CW_FIELD(wYBase, /floating, VALUE=0, Title='max', $
                       UVALUE='REPAINT', xsize=10,/return_events)

  wXBase = WIDGET_BASE(wLeftBase, COLUMN=1,/FRAME)
  wXLabel = WIDGET_LABEL(wXBase, VALUE='X')
  wXmodeButton = WIDGET_DROPLIST(wXBase, VALUE=['Full Range','Fixed'], $
                                 UVALUE='XMODE')
  wXminText = CW_FIELD(wXBase, /floating, VALUE=0, Title='min', $
                       UVALUE='REPAINT',xsize=10,/return_events)
  wXmaxText = CW_FIELD(wXBase, /floating, VALUE=0, Title='max', $
                       UVALUE='REPAINT', xsize=10,/return_events)
  ;; END Left pane  ---------------
  
;; Center pane  ---------------
  wCenterBase = WIDGET_BASE(wTopRowBase, /COLUMN)
  wCenterHiBase = WIDGET_BASE(wCenterBase, COLUMN=3, /FRAME)
  wCenterLoBase = WIDGET_BASE(wCenterBase, COLUMN=1, /FRAME)
  
  wPlotChecks = WIDGET_BASE(wCenterHiBase, column=10,/NONEXCLUSIVE)

  wPlotUnMaskButton = WIDGET_BUTTON(wPlotChecks, $
                                  VALUE='UnMasked', UVALUE='PLOTUNMASK')

  widget_control, wPlotUnMaskButton, SET_BUTTON=1

  wPlotMaskButton = WIDGET_BUTTON(wPlotChecks, $
                                      VALUE='Mask', UVALUE='PLOTMASK')
  wPlotMeanButton = WIDGET_BUTTON(wPlotChecks, $
                                  VALUE='Mean', UVALUE='PLOTMEAN')
  wPlotMeanButton = WIDGET_BUTTON(wPlotChecks, $
                                  VALUE='Mean All', UVALUE='PLOTMEANALL')
  IF doimage THEN BEGIN
      wPlotImageButton = WIDGET_BUTTON(wPlotChecks, $
                                       VALUE='Image', UVALUE='PLOTIMAGE')
  ENDIF 
  wPlotMedianButton = WIDGET_BUTTON(wPlotChecks, $
                                  VALUE='Median', UVALUE='PLOTMEDIAN')
  wPlotBordersButton = WIDGET_BUTTON(wPlotChecks, $
                                  VALUE='Borders', UVALUE='PLOTBORDERS')
  wPlotOplotButton = WIDGET_BUTTON(wPlotChecks, $
                                   VALUE='Overplot', UVALUE='PLOTOPLOT')

  wUndoButton = WIDGET_BUTTON(wCenterHiBase, $
                              VALUE='Undo Mask', UVALUE='UNDO', SENSITIVE=0)


  wAreaPlot = WIDGET_DRAW(wCenterLoBase, XSIZE=600, YSIZE=400,   $
                          UVALUE='DRAWING', /BUTTON)
  wActionLabel = WIDGET_LABEL(wCenterLoBase, VALUE='Btn-1:zoom X, Btn-2:zoom Y, Btn-3:mask')
  
;; END Center pane  ---------------
  
  ;;  Realize the widget hierarchy.
  ;;
  WIDGET_CONTROL, wTopBase, /REALIZE
  
  WIDGET_CONTROL, wAreaPlot, GET_VALUE=plotWindowID
;;  widget_CONTROL, wAreaPlot, /DRAW_BUTTON_EVENTS
  
  ;;  Create the info structure
  ;;
  mask_record = {mask_record,x:0,y:0,first:0,last:0}
  mask_db = {mask_db,n_masks:0,mask:replicate(mask_record,1000)}

  ptrNewMask = ptr_new(Mask)

  sInfo = { $
            cube:cube, $
            mask:mask, $
            oldmask:mask, $
            image:image, $
            image_time: times, $
            image_from: InRaster.From, $
            image_to: InRaster.To, $
            mask_db:mask_db, $
            PlotWindowID: plotWindowID, $ ; Window ID
            wTopBase:wTopBase, $
            wYmaxText:wYmaxText, $
            wYminText:wYminText, $
            wYmodeButton:wYmodeButton, $
            Ymode:0, $
            wXmodeButton:wXmodeButton, $
            Xmode:0, $  
            wXdetSlider:wXdetSlider, $
            wYdetSlider:wYdetSlider, $
            wAreaPlot:wAreaPlot, $
            wXminText:wXminText, $
            wXmaxText:wXmaxText, $
            wUndoButton:wUndoButton, $
            wStoreButton:wStoreButton, $
            wApplyButton:wApplyButton, $
            wRestartButton:wRestartButton, $
            plotMean:0, $
            plotMeanAll:0, $
            plotMedian:0, $
            plotBorders:0, $
            plotUnMask:1, $
            plotMask:0, $
            plotOplot:0, $
            plotImage:0, $
            xCursor: 0.0, $
            yCursor: 0.0, $
            ButtonPressed:0, $
;;            previousChar: previousChar, $ ; Previous character size
;;            plotFont: plotFont, $ ; Font to trestore
            ptrNewMask:ptrNewMask $
          }
        
    sh_disp_raster_plot,sInfo

  ;;  Register the info structure in the user value
  ;;  of the top-level base.
  ;;
    WIDGET_CONTROL, wTopBase, SET_UVALUE=sInfo, /NO_COPY

    ;;  Map the top level base.
    ;;
    WIDGET_CONTROL, wTopBase, MAP=1
    
    ;; Register with the BIG GUY, XMANAGER!
    ;;
    XMANAGER, "sh_disp_raster_", wTopBase, $
        EVENT_HANDLER="sh_disp_raster_Event", $
        CLEANUP="Sh_Disp_Raster_Cleanup",no_block=no_block
    
    IF no_block EQ 0 THEN BEGIN 
        IF n_elements(InRaster) NE 0 THEN BEGIN 
            InRaster.Mask = *ptrNewMask
        ENDIF
        ptr_free,ptrNewMask
    ENDIF 
END   ; of Sh_Disp_Raster
