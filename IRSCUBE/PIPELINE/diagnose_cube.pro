;--------------------------------------------------------------
;
;  Purpose:  diagnose_cube_DisplayNew
;  Display the widget for the first time with the new images
PRO diagnose_cube_DisplayNew, sInfo
  
  ptr_free,sInfo.ptOImage
  if sInfo.idxplane eq -1 then begin
     sInfo.ptOImage = ptr_new(*sInfo.ptTotalCube)
  endif else begin
    idxmin = sInfo.idxplane-sInfo.channelwidth > 0
    idxmax = sInfo.idxplane+sInfo.channelwidth < (sInfo.nplanes-1)
    if idxmin eq idxmax then begin
       sInfo.ptOImage = ptr_new(reform((*sInfo.ptCube)[*,*,idxmin]))
    endif else begin
       sInfo.ptOImage = ptr_new(mean((*sInfo.ptCube)[*,*,idxmin:idxmax],dimension=3))
    endelse

    if sInfo.SubtractContinuum then begin
       ;; remove a "continuum" which is defined as the mean of the median
       ;; of the three planes before and the median of the three planes
       ;; after the window

       ;; if we are on the left edge no point to looking to the left
       if idxmin gt 0 then begin
          idxminmin = (idxmin-3) > 0
          idxminmax = (idxmin-1) > 0
          if idxminmin eq idxminmax then begin
             ileft = reform((*sInfo.ptCube)[*,*,idxminmin])
          endif else begin
             ileft = median((*sInfo.ptCube)[*,*,idxminmin:idxminmax],dimension=3)
          endelse
       endif else begin
          ileft = (*sInfo.ptOImage)*!values.d_nan
       endelse
       has_ileft = finite(ileft)

       ;; if we are on the right edge no point to looking to the right
       if idxmax lt (sInfo.nplanes-1) then begin
          idxmaxmin = (idxmax+1) < (sInfo.nplanes-1)
          idxmaxmax = (idxmax+3) < (sInfo.nplanes-1)

          if idxmaxmin eq idxmaxmax then begin
             iright = reform((*sInfo.ptCube)[*,*,idxmaxmin])
          endif else begin
             iright = median((*sInfo.ptCube)[*,*,idxmaxmin:idxmaxmax],dimension=3)
          endelse
          
       endif else begin
          iright = (*sInfo.ptOImage)*!values.d_nan
       endelse
       has_iright = finite(iright)

       icont = (ileft+iright)/(has_ileft+has_iright)
       icont[where(finite(icont) ne 1)] = 0d0

       isubtract = (*sInfo.ptOImage) - icont
       ptr_free,sInfo.ptOImage
       sInfo.ptOImage = ptr_new(isubtract)

    endif
    
 endelse                  

  OImage = (*sInfo.ptOImage)
  ;; Size the draw area to the size of the OImage
  sImage = size(OImage)
  ;;  Set up the drawing area size
  sInfo.XSize = sImage[1]
  sInfo.YSize = sImage[2]

  ;; deal with possible nans in the input
  if sInfo.RemoveNan eq 1 then begin
     GoodIdxImage = where(finite(OImage) EQ 1,cnt,complement=BadIdxImage,ncomplement=ncnt)
     if (cnt ne 0) and (ncnt ne 0) then begin
        OImage[BadIdxImage] = median(OImage[GoodIdxImage])
     endif
  endif

  ;; Allow to resize the draw areas
  IF sInfo.Resize EQ 0 THEN BEGIN
      sInfo.ResFact = 1
  ENDIF ELSE BEGIN
      IF sInfo.Resize EQ 1 THEN BEGIN
          ;; This is to make the image maximum 500 wide or high
          sInfo.ResFact = (5d2/sInfo.XSize < 5d2/sInfo.YSize)
      ENDIF ELSE BEGIN
          sInfo.ResFact = sInfo.Resize
      ENDELSE
      sInfo.XSize = sInfo.ResFact*sInfo.XSize
      sInfo.YSize = sInfo.ResFact*sInfo.YSize
      OImage = CONGRID(OImage,sInfo.XSize,sInfo.YSize,cubic=-0.5)
  ENDELSE

  ;; Now make sure we delete the old heap variables and simply
  ;; reassign them with the right size
  ptr_free,sInfo.ptOImage,sInfo.ptImage
  sInfo.ptOImage = ptr_new(OImage)
  sInfo.ptImage = ptr_new(OImage)

  ;; resize the draw areas
  WIDGET_CONTROL, sInfo.wAreaDraw, Draw_XSize=sInfo.XSize
  WIDGET_CONTROL, sInfo.wAreaDraw, Draw_YSize=sInfo.YSize
  
  ;; Load the colorTable
  diagnose_cube_LoadCT,sInfo
  
  ;; Now prepare the Image to be displayed properly
  IF sInfo.Absolute THEN (*sInfo.ptOImage)=abs((*sInfo.ptOImage))
  sInfo.MinImage = min((*sInfo.ptOImage),max=max)
  sInfo.MaxImage = max

  ;; And show the shit
  diagnose_cube_refresh,sInfo,/scale,/dimage
END                             ;   of diagnose_cube_DisplayNew

;--------------------------------------------------------------------
;
PRO diagnose_cube_Cleanup, wTopBase
    WIDGET_CONTROL, wTopBase, GET_UVALUE=sInfo,/NO_COPY
    ;  Restore the previous plot font.
    !P.FONT = sInfo.plotFont
    !P.CHARSIZE = sInfo.previousChar
    tvlct,sInfo.previousRed,sInfo.previousGreen,sInfo.previousBlue
    ptr_free,sInfo.ptOImage,sInfo.ptImage
END


FUNCTION diagnose_cube_scale, image
  
  simage= image[sort(image)]
  median = median(simage)
  ximage = dindgen(n_elements(simage))
  
  ;; now give weights which are inversely propotional to the
  ;; difference between the datavalue and the median
  
  errs = sqrt(abs(simage-median))
  ;; make sure there are no zero errors
  errs = errs>0.01*median(errs)
  ;; Still not good so fudge further
  IF total(errs NE 0) NE 0 THEN BEGIN
      errs = errs>min(errs[where(errs NE 0)])
  ENDIF 

  foo = poly_fit(ximage,simage,1,measure_errors=errs,/double,yband=stdev,yfit=yfit)
  ;; cut of the lowest and hightest parts
  zmin = min(yfit,max=zmax)
  ikeep = where( (simage GT zmin) AND (simage LT zmax))
  
  fit = poly_fit(ximage[ikeep],simage[ikeep],1,measure_errors=errs[ikeep],/double,yfit=yfit)
  
  zmin = fit[0]
  zmax = (fit[0]+n_elements(simage)*fit[1])>(zmin+1)
  return,[zmin,zmax]
END 

;--------------------------------------------------------------------
;
;  Purpose:  Do the pixel scaling
;
PRO diagnose_cube_MakeScaling, sInfo
  WIDGET_CONTROL, sInfo.wMinScalingSlider, GET_VALUE=minValue
  WIDGET_CONTROL, sInfo.wMaxScalingSlider, GET_VALUE=maxValue
  WIDGET_CONTROL, sInfo.wPowerLawText, GET_VALUE=powerLaw
  WIDGET_CONTROL, sInfo.wInvertSlider, GET_VALUE=Invert
  
  power = double(powerlaw[0])
  
  ;;  Get the cut values from the sliders AND the range OF the pixels
  ;;  in the image
  cutLo = minValue/255d0*(sInfo.maxImage-sInfo.minImage)+sInfo.minImage
  cutHi = maxValue/255d0*(sInfo.maxImage-sInfo.minImage)+sInfo.minImage
  
  ;; This is a complicated expression that does (ltr) the following:
  ;; set all point lower than cutLo to cutLo
  ;; set all point higher than cutHi to cutHi
  ;; subtract the cutLo (data now from 0 to cutHi-cutLo)
  ;; divide by the total range cutHi-cutLo (data 0-1)
  ;; raise to the power power (weighting function)
  ;; invert can be 0 or 1
  ;;    if 0 do nothing (multiply by 1 and add 0)
  ;;    if 1 flip the values (multiply by -1 and add 1)
  ;; finally multiply by the total number of colors available (highcolor)
  
  (*sInfo.ptImage)=sInfo.highColor*(((((*sInfo.ptOImage)>cutLo)<cutHi-cutLo)/ $
                                (cutHi-cutLo))^power*(1d0-2d0*Invert)+Invert)

  IF sInfo.Equalize THEN BEGIN
      hist = histogram((*sInfo.ptImage))
      ;; make a cummulative increasing scale
      FOR i = 1,n_elements(hist)-1 DO hist[i] = hist[i]+hist[i-1]
      ;; scale to the number of available colors
      hist = long(bytscl(hist, top = sInfo.highColor))
      ;; project the values on the equalized colors
      (*sInfo.ptImage) = interpol(hist,dindgen(n_elements(hist)),(*sInfo.ptImage))
  ENDIF  
  
END

;--------------------------------------------------------------------
;
;  Purpose:  Plot the image
;
PRO diagnose_cube_Display, WindowID, Image
  WSET, WindowID
  ERASE
  TV,image
END


;--------------------------------------------------------------------
;
;  Purpose:  Refresh the complete display
;
PRO diagnose_cube_refresh,sInfo,scale=scale,dimage=dimage
  IF keyword_set(scale) THEN diagnose_cube_MakeScaling,sInfo
  IF keyword_set(dimage) THEN diagnose_cube_Display,sInfo.drawWindowID,(*sInfo.ptImage)
  IF sInfo.ShowOverlay THEN diagnose_cube_DisplayOverlay,sInfo
END 

;--------------------------------------------------------------------
;
;  Purpose:  Plot the overlay points on top of the images
;
PRO diagnose_cube_DisplayOverlay, sInfo
  
  overlay = (*sInfo.ptOverlay)
  IF n_elements(overlay) EQ 0 THEN return

  WIDGET_CONTROL, sInfo.wOverlayPsymSlider, GET_VALUE=psym
  WIDGET_CONTROL, sInfo.wOverlayColorSlider, GET_VALUE=color

  if !d.name eq 'X' then begin
     WSET, sInfo.DrawWindowID
  endif

  IF psym NE 8 THEN BEGIN
     plots,(overlay[0,*]-sInfo.maskOffsetX)*sInfo.ResFact,(overlay[1,*]-sInfo.maskOffsetY)*sInfo.ResFact,ps=psym, $
           color=color,/device
  ENDIF ELSE BEGIN
     labels = (STRCOMPRESS(sindgen(n_elements(overlay[0,*])+1),/remove_all))[1:*]
     xyouts,(overlay[0,*]-sInfo.maskOffsetX)*sInfo.ResFact,(overlay[1,*]-sInfo.maskOffsetY)*sInfo.ResFact,labels, $
            color=color,/device,align=0.5
  ENDELSE 
END

;--------------------------------------------------------------------
;
;  Purpose:  load a new colortable
;
PRO diagnose_cube_LoadCT, sInfo
  WIDGET_CONTROL, sInfo.wColorTableSlider, GET_VALUE=colortable
  loadct,colortable,/silent
  sInfo.highColor = !D.TABLE_SIZE-1
END

;--------------------------------------------------------------------
;
;  Purpose:  plot the spectrum
;
PRO diagnose_cube_PlotSpectrum, sInfo

  if !d.name eq 'X' then begin
     WSET,sInfo.PlotWindowID
  endif

  x = sInfo.XSpectrum
  xfull = sInfo.XSpectrum + sInfo.maskOffsetX
  y = sInfo.YSpectrum
  yfull = sInfo.YSpectrum + sInfo.maskOffsetY
  
  wave = sInfo.wave
  flux = reform((*sInfo.ptCube)[x,y,*])

  if sInfo.ShowFivePixelMean then begin

     nx = n_elements((*sInfo.ptCube)[*,0,0])
     ny = n_elements((*sInfo.ptCube)[0,*,0])

     xx = x+[-1,1, 0,0]
     yy = y+[ 0,0,-1,1]
     nn = 1d0

     for iii=0,n_elements(xx) -1 do begin
        xxx = xx[iii]
        yyy = yy[iii]
        if xxx ge 0 and xxx le nx and yyy ge 0 and yyy le ny then begin
           flux = flux + reform((*sInfo.ptCube)[xxx,yyy,*])
           nn = nn+1d0
        endif
     endfor
     flux = flux/nn
  end

  error = flux*0d0
  order = sInfo.Order
  
  tmp = transpose([[wave],[flux],[error],[order]])

  WIDGET_CONTROL, sInfo.wPlotCommandText,GET_VALUE=command
  
  command=command[0]

  if command eq "" then command="pl"

  frmtstr='("Spectrum at: ",I3,",",I3)'
  command = command+",tmp,/bands,psym=0,xtit='wavelength',ytit='flux',title=string(format=frmtstr,xfull,yfull)"
  
  foo=execute(command)
  
  if sInfo.idxplane ne -1 then begin
     idxmin = sInfo.idxplane-sInfo.channelwidth > 0
     idxmax = sInfo.idxplane+sInfo.channelwidth < (sInfo.nplanes-1)
     oplot,wave[idxmin]*[1,1],!y.crange,thick=3,lines=2
     oplot,wave[idxmax]*[1,1],!y.crange,thick=3,lines=2
  endif

  ;; put some number in the figure to indicate from
  ;; which plane the different data points come
  if sInfo.ShowIndicators then begin
     nWave = n_elements(wave)
     nicepoints = round((dindgen(8)+1)/10d0*nWave)
     xyouts,wave[nicepoints],flux[nicepoints],strcompress(string(nicepoints),/remove_all),align=0.5
  endif

END

;--------------------------------------------------------------------
;
PRO diagnose_cube_Event, sEvent
  ;; Which event
  WIDGET_CONTROL, sEvent.id, GET_UVALUE=eventUValue
  IF eventUValue EQ 'QUIT' THEN BEGIN
     ;; If 'QUIT' we quit but be careful since sInfo is no longer
     ;; available after destroy
     WIDGET_CONTROL, sEvent.top, /DESTROY
  ENDIF ELSE BEGIN
     WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo,/NO_COPY
     CASE eventUValue OF
        'DRAWING' : BEGIN
           x = sEvent.x/double(sInfo.ResFact)
           xfull = sInfo.XSpectrum + sInfo.maskOffsetX
           y = sEvent.y/double(sInfo.ResFact)
           yfull = sInfo.YSpectrum + sInfo.maskOffsetY

           CASE sEvent.press OF 
              0:BEGIN
                 if not sInfo.plotLocked then begin
                    sInfo.XSpectrum = x
                    sInfo.YSpectrum = y
                    diagnose_cube_PlotSpectrum, sInfo
                 endif
              END
              1:BEGIN
                 IF sInfo.Recording THEN BEGIN
                    printf,sInfo.lunRecord, format= '(2F8.2,E11.3)', $
                           xfull,yfull
                    (*sInfo.ptOverlay)[0:1,sInfo.nRecordings] = [xfull,yfull]
                    sInfo.nRecordings = sInfo.nRecordings+1
                    diagnose_cube_refresh,sInfo,/scale,/dimage
                 ENDIF
              END
              ELSE: BEGIN
              END
           ENDCASE
           
           if sEvent.Release eq 1 and string(sEvent.ch) eq 'l' then sInfo.plotLocked = (sInfo.plotLocked ne 1)

        END
        
        'PLOTTING' : BEGIN
           CASE sEvent.press OF 
              1:BEGIN
                 x = sEvent.x
                 y = sEvent.y
                 c=convert_coord(x,y,/device,/to_data)
                 foo = min(abs(sInfo.wave-c[0]),idx_closestplane)
                 WIDGET_CONTROL, sInfo.wPlaneSlider, SET_VALUE=idx_closestplane
                 sInfo.idxplane = idx_closestplane
                 diagnose_cube_DisplayNew,sInfo
                 diagnose_cube_PlotSpectrum, sInfo
              END
              ELSE: BEGIN
              END
           ENDCASE
        END

        'MINSCALING' : BEGIN
          WIDGET_CONTROL, sInfo.wMinScalingSlider, GET_VALUE=minValue
          WIDGET_CONTROL, sInfo.wMaxScalingSlider, GET_VALUE=maxValue
          IF (maxValue LE minValue) THEN BEGIN
            maxValue = minValue + 1
            WIDGET_CONTROL, sInfo.wMaxScalingSlider, SET_VALUE=maxValue
          ENDIF
          diagnose_cube_refresh,sInfo,/scale,/dimage
        END
        
        'MAXSCALING' : BEGIN
          WIDGET_CONTROL, sInfo.wMinScalingSlider, GET_VALUE=minValue
          WIDGET_CONTROL, sInfo.wMaxScalingSlider, GET_VALUE=maxValue
          IF (minValue GE maxValue) THEN BEGIN
            minValue = maxValue - 1
            WIDGET_CONTROL, sInfo.wMinScalingSlider, SET_VALUE=minValue
          ENDIF
          diagnose_cube_refresh,sInfo,/scale,/dimage
        END
        
        'POWERLAW' : BEGIN
            diagnose_cube_refresh,sInfo,/scale,/dimage
        END
        
        'INVERT' : BEGIN
            diagnose_cube_refresh,sInfo,/scale,/dimage
        END

        'AUTOSCALE' : BEGIN
            sInfo.AutoScale = NOT sInfo.AutoScale
            IF sInfo.AutoScale THEN BEGIN
                AutoScale = diagnose_cube_scale((*sInfo.ptOImage))
                sInfo.minImage = AutoScale[0]
                sInfo.maxImage = AutoScale[1]
            ENDIF ELSE BEGIN
                sInfo.MinImage = min((*sInfo.ptOImage),max=max)
                sInfo.MaxImage = max
            ENDELSE
            diagnose_cube_refresh,sInfo,/scale,/dimage
        END
        
        'EQUALIZE' : BEGIN
            sInfo.Equalize = NOT sInfo.Equalize
            diagnose_cube_refresh,sInfo,/scale,/dimage
        END

        'COLORTABLE' : BEGIN
           diagnose_cube_loadct,sInfo
           diagnose_cube_refresh,sInfo,/scale,/dimage
        END
        
        'OVERLAY' : BEGIN
          file = dialog_pickfile(FILTER='*',/MUST_EXIST)
          IF file EQ '' THEN BEGIN
          END ELSE BEGIN
              overlay = read_ascii(file,comment='#')
              overlay = (overlay.(0))[0:1,*]
              IF ptr_valid(sInfo.ptOverlay) THEN ptr_free, sInfo.ptOverlay
              sInfo.ptOverlay = ptr_new(Overlay,/no_copy)
              ;; XXX
              sInfo.ShowOverlay = 1
              WIDGET_CONTROL,sInfo.wShowOverlayCheck,/set_button
              WIDGET_CONTROL,sInfo.wShowOverlayCheck,/sensitive
              diagnose_cube_refresh,sInfo,/dimage
          ENDELSE
        END
        
        'RECORD' : BEGIN
            IF sInfo.Recording THEN BEGIN
                close,sInfo.lunRecord
                free_lun,sInfo.lunRecord
                widget_control,sInfo.wRecordButton,set_value='Record Positions'
                sInfo.Recording = 0
            ENDIF ELSE BEGIN
                file = dialog_pickfile(FILTER='*',/overwrite,/write, $
                                       title='Filename for positions (Left click in Zoom)')
                IF file NE '' THEN BEGIN
                    openw,lun,file,/get_lun
                    sInfo.lunRecord = lun
                    widget_control,sInfo.wRecordButton,set_value='Stop recording'
                    sInfo.Recording = 1
                    
                    ;; put the recored points in the current overlay
                    ;; (remove the current overlay)
                    IF ptr_valid(sInfo.ptOverlay) THEN ptr_free, sInfo.ptOverlay
                    ;; make a large table to hold the overlay points
                    overlay = make_array(2,1000,value=0.0)
                    sInfo.nRecordings = 0
                    
                    sInfo.ptOverlay = ptr_new(Overlay,/no_copy)
                    sInfo.ShowOverlay = 1
                    WIDGET_CONTROL,sInfo.wShowOverlayCheck,/set_button
                    ;; dont switch off overlaying while recording
                    WIDGET_CONTROL,sInfo.wShowOverlayCheck,sensitive=0

                    diagnose_cube_refresh,sInfo,/dimage
                 ENDIF
            ENDELSE
        END
        
        'PSBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.ps')
          sh_ps,/color,outfile,/a4land,thick=3,charsize=1.5
          diagnose_cube_PlotSpectrum, sInfo
          sh_ps
        END
        
        'MAPPSBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.ps')
          a4port = (sInfo.XSize LT sInfo.YSize)
          a4land = NOT a4port
          sh_ps,/color,outfile,bits=8,a4port=a4port,a4land=a4land
          TV,(*sInfo.ptImage)
          if sInfo.ShowOverlay then diagnose_cube_DisplayOverlay,sInfo
          sh_ps
       END                      ;       of  PSBUTTON
        
        'FILEBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.dat')
          if outfile ne '' then begin
             tmp = transpose([ $
                   [sInfo.wave], $
                   [reform((*sInfo.ptCube)[sInfo.XSpectrum,sInfo.YSpectrum,*])], $
                   [sInfo.wave*0d0], $
                   [sInfo.Order]])
             openw,lun,outfile,/get_lun
             printf,lun,tmp,format="(4F)"
             close,lun
             free_lun,lun
          endif
        END

        'SHOWOVERLAY' : BEGIN
            sInfo.ShowOverlay = NOT sInfo.ShowOverlay
            diagnose_cube_refresh,sInfo,/dimage
        END
        
        'SHOWINDICATORS' : BEGIN
            sInfo.ShowIndicators = NOT sInfo.ShowIndicators
            diagnose_cube_PlotSpectrum, sInfo
        END
        
        'SHOWFIVEPIXELMEAN' : BEGIN
            sInfo.ShowFivePixelMean = NOT sInfo.ShowFivePixelMean
            diagnose_cube_PlotSpectrum, sInfo
        END
        
        'PLOTCOMMAND' : BEGIN
            diagnose_cube_PlotSpectrum, sInfo
        END
        
        'OVERLAYCOLOR' : BEGIN
            WIDGET_CONTROL, sInfo.wOverlayColorSlider, GET_VALUE=overlaycolor
            sInfo.OverlayColor = OverlayColor
            diagnose_cube_refresh,sInfo,/dimage
        END

        'OVERLAYPSYM' : BEGIN
            WIDGET_CONTROL, sInfo.wOverlayPsymSlider, GET_VALUE=overlaypsym
            sInfo.OverlayPsym = Overlaypsym
            diagnose_cube_refresh,sInfo,/dimage
        END

        'PLANESLIDER' : BEGIN
            WIDGET_CONTROL, sInfo.wPlaneSlider, GET_VALUE=idxplane
            if sInfo.idxplane ne idxplane then begin
               sInfo.idxplane = idxplane
               diagnose_cube_DisplayNew,sInfo
               diagnose_cube_PlotSpectrum, sInfo
            endif
        END

        'WIDTHSLIDER' : BEGIN
            WIDGET_CONTROL, sInfo.wWidthSlider, GET_VALUE=channelwidth
            if sInfo.channelwidth ne channelwidth then begin
               sInfo.channelwidth = channelwidth
               diagnose_cube_DisplayNew,sInfo
               diagnose_cube_PlotSpectrum, sInfo
            endif
        END

        'SUBTRACTCONTINUUM' : BEGIN
           sInfo.SubtractContinuum = NOT sInfo.SubtractContinuum
           diagnose_cube_DisplayNew,sInfo
           diagnose_cube_PlotSpectrum, sInfo
        END

        ELSE :                  ;  do nothing
      ENDCASE
      WIDGET_CONTROL, sEvent.top, SET_UVALUE=sInfo,/NO_COPY
    ENDELSE
  END
  
;--------------------------------------------------------------------
;
;   PURPOSE: Display an image and allow the cuts to be set interactively
;   also allows for zoom216ing and a different powerlaw index
;
;   OPTIONS:
;               absolute=1 convert the image to absolute values [0]
;               colortable=n set colortable to n [1]
;
PRO diagnose_cube, InStr, $
                   absolute=absolute, $
                   colortable=colortable, $
                   out=out, $
                   resize=resize, $
                   nan=nan, $
                   no_block=no_block , $
                   min=min, $
                   max=max, $
                   usemask=usemask, $
                   name=name
  
  
  absolute=keyword_set(absolute)
  IF NOT keyword_set(colortable) THEN colortable=27
  if keyword_set(nan) then RemoveNan=1 else RemoveNan=0
  IF n_elements(no_block) eq 0 then no_block=1
  IF NOT keyword_set(usemask) THEN usemask=0
  IF NOT keyword_set(name) THEN name=''

  ;;  Save the font
  ;;
  plotFont = !P.FONT
  previousChar = !P.CHARSIZE
  tvlct,/get,previousRed,previousGreen,previousBlue
  ;;!P.CHARSIZE = 8.0 / !D.X_CH_SIZE
  
  ;;  Use hardware-drawn font.
  ;;
  ;; !P.FONT=0
  
  IF n_elements(InStr) EQ 0 THEN BEGIN
     filename = dialog_pickfile(FILTER='*.fits',/MUST_EXIST)
     if filename eq '' then begin
        ;; graciously exit 
        message,/info,'No input cube given and none selected from the file system'
        message,/info,'Returning'
        return
     endif else begin
        InStr = readfitscube(filename)
        name=filename
     endelse
  ENDIF 

  if size(InStr,/tname) eq 'STRING' then begin
     Str = readfitscube(InStr)
     name=Instr
  endif else begin
     Str = InStr
  endelse
  
  if size(Str,/tname) ne 'STRUCT' then begin
     message,/info,'Str does not contain a structure, aborting'
     return
  endif
  
  cube = Str.cube
  wave = Str.wave
  if tag_exist(Str,'ORDER') then begin
     order = Str.order
  endif else begin
     order = wave*0
  endelse

  minXmask=0
  minYmask=0
  
  if usemask then begin
     if tag_exist(Str,'MASK') then begin
        mask = Str.mask
        alldatamask = mask eq max(mask)
        Xmask = where(total(alldatamask,2) ne 0)
        Ymask = where(total(alldatamask,1) ne 0)
        minXmask =min(xmask,max=maxXmask)
        minYmask =min(ymask,max=maxYmask)

        nx = n_elements(cube[*,0,0])
        ny = n_elements(cube[0,*,0])
        nw = n_elements(cube[0,0,*])
        cubemask = rebin(alldatamask,nx,ny,nw)

        cube = cube*cubemask

        cube=reform(cube[minXmask:maxXmask,minYmask:maxYmask,*])
     endif
  endif

  totalcube = total(cube,3)
  
  ;; Apply limits if asked for
  if n_elements(min) eq 1 then Cube = (Cube>min)
  if n_elements(max) eq 1 then Cube = (Cube<max)
  
  ;; allow for cube cubes as well and check for proper input
  sizecube = size(Cube)
  case sizecube[0] of
     3: begin
        idxplane = -1
        Image = totalcube
        havecube = 1
        nplanes = sizecube[3]
        channelwidth=0
     end
     else: begin
        message,/info,'The input does not appeat to be a cube'
        return
     end
  endcase
  
  Image = DOUBLE(Image)
  
  ;; to resize the image
  ;; if resize=1 element then make the largest 500 or do factor 
  ;; else resize by the factor given
  IF NOT keyword_set(resize) THEN resize = 0 ELSE resize = resize[0]

;; Now create the widget
  wTopBase = WIDGET_BASE(TITLE="Diagnose Cube "+name, /COLUMN, MAP=0, MBAR=barBase)
  
  ;;  Create the menu bar items
  ;;
  wFileButton = WIDGET_BUTTON(barBase, VALUE='File')
  
  wOverlayButton = WIDGET_BUTTON(wFileButton, VALUE='Overlay', UVALUE='OVERLAY')
  wRecordButton = WIDGET_BUTTON(wFileButton, VALUE='Record Positions', UVALUE='RECORD')
  wQuitButton = WIDGET_BUTTON(wFileButton, VALUE='Quit', UVALUE='QUIT',/separator)

  ;;  Create the left, center and right bases
  ;;
  wTopRowBase = WIDGET_BASE(wTopBase, COLUMN=3)
  
;; Left pane  ---------------
  wLeftBase = WIDGET_BASE(wTopRowBase, /COLUMN, /FRAME)

  wDrawLabel = WIDGET_LABEL(wLeftBase, VALUE='Cube slice (press l to lock/unlock the position of the spectrum)')

  wAreaDraw = WIDGET_DRAW(wLeftBase, XSIZE=XSize, YSIZE=YSize, RETAIN=2,  $
                          UVALUE='DRAWING', /MOTION, /BUTTON,/KEY)
  
  wPSButton = WIDGET_BUTTON(wLeftBase, UVALUE='MAPPSBUTTON', VALUE='.ps File')

  wScalingBase = WIDGET_BASE(wLeftBase,column=3)
  wPowerLawLabel = WIDGET_LABEL(wScalingBase, VALUE='Power Law:')
  
  wPowerLawText = WIDGET_TEXT(wScalingBase, /EDITABLE, VALUE='1.0', $
                              UVALUE='POWERLAW')
  
  wMinScalingSlider = WIDGET_SLIDER(wScalingBase, MINIMUM=0,  $
                                    MAXIMUM=254, VALUE=0, $
                                    TITLE='Minimum', UVALUE='MINSCALING')
  
  wMaxScalingSlider = WIDGET_SLIDER(wScalingBase, MINIMUM=1,  $
                                    MAXIMUM=255, VALUE=255, $
                                   TITLE='Maximum', UVALUE='MAXSCALING')

  wLeftMidBase = WIDGET_BASE(wLeftBase, COLUMN=3, /FRAME)
  wColorTableSlider = WIDGET_SLIDER(wLeftMidBase, MINIMUM=0, MAXIMUM=40, $
                                    VALUE=colortable, TITLE='Color table',  $
                                    UVALUE='COLORTABLE')
  wInvertSlider = WIDGET_SLIDER(wLeftMidBase, MINIMUM=0, MAXIMUM=1, $
                                    VALUE=0, TITLE='Invert',  $
                                    UVALUE='INVERT')

  wEqualizeBase = WIDGET_BASE(wLeftMidBase, /nonexclusive,/column)
  wEqualizeCheck = WIDGET_BUTTON(wEqualizeBase, $
                                    VALUE='Equalize',  $
                                    UVALUE='EQUALIZE')

  wAutoScaleCheck = WIDGET_BUTTON(wEqualizeBase, $
                                    VALUE='Auto Scale',  $
                                    UVALUE='AUTOSCALE')

  wOverlayBase = WIDGET_BASE(wLeftBase, COLUMN=4, /FRAME)
  wOverlayColorSlider = WIDGET_SLIDER(wOverlayBase, MINIMUM=0,  $
                                    MAXIMUM=254, VALUE=0, $
                                    TITLE='Overlay Color', UVALUE='OVERLAYCOLOR')
  wOverlayPsymSlider = WIDGET_SLIDER(wOverlayBase, MINIMUM=1,  $
                                    MAXIMUM=8, VALUE=1, $
                                    TITLE='Overlay Psym', UVALUE='OVERLAYPSYM')

  wShowOverlayBase = WIDGET_BASE(wOverlayBase, /nonexclusive,/column)
  wShowOverlayCheck = WIDGET_BUTTON(wShowOverlayBase, $
                                    VALUE='Show Overlay',  $
                                    UVALUE='SHOWOVERLAY',sensitive=0)
  ;;  Create a plane slider if a cube is given
  if havecube eq 1 then begin
     wShowSubtractContinuumCheck = WIDGET_BUTTON(wShowOverlayBase, $
                                                 VALUE='Subtract Continuum',  $
                                                 UVALUE='SUBTRACTCONTINUUM',sensitive=1)
     
     wPlaneSliderBase = WIDGET_BASE(wOverlayBase,column=2)
     wPlaneSlider = WIDGET_SLIDER(wPlaneSliderBase, MINIMUM=-1,  $
                                  MAXIMUM=nplanes-1, VALUE=-1, $
                                  TITLE='Plane', UVALUE='PLANESLIDER')
     wWidthSlider = WIDGET_SLIDER(wPlaneSliderBase, MINIMUM=0,  $
                                  MAXIMUM=nplanes/2, VALUE=0, $
                                  TITLE='Window', UVALUE='WIDTHSLIDER')
  endif else begin
     wPlaneSlider = -1
  endelse

;; END Left pane  ---------------
  
;; Center pane  ---------------
  wCenterBase = WIDGET_BASE(wTopRowBase, /COLUMN)
  wCenterHiBase = WIDGET_BASE(wCenterBase, /COLUMN, /FRAME)
  
  wDrawLabel = WIDGET_LABEL(wCenterHiBase, VALUE='Spectrum')
  
  wAreaPlot = WIDGET_DRAW(wCenterHiBase, XSIZE=600, YSIZE=400, RETAIN=2,  $
                          UVALUE='PLOTTING', /BUTTON)

  wPlotCommandText = WIDGET_TEXT(wCenterHiBase,/editable, value='pl,mark="pah",mstyle=2',uvalue='PLOTCOMMAND')

  wShowIndicatorBase = WIDGET_BASE(wCenterHiBase, /nonexclusive)

  wShowIndicatorsCheck = WIDGET_BUTTON(wShowIndicatorBase, $
                                       VALUE='Show Indicators',  $
                                       UVALUE='SHOWINDICATORS',sensitive=1)
  
  wShowFivePixelsCheck = WIDGET_BUTTON(wShowIndicatorBase, $
                                       VALUE='Show Five Pixel Mean',  $
                                       UVALUE='SHOWFIVEPIXELMEAN',sensitive=1)
  
  wPSButton = WIDGET_BUTTON(wCenterHiBase, UVALUE='PSBUTTON', VALUE='.ps File')

  wFileButton = WIDGET_BUTTON(wCenterHiBase, UVALUE='FILEBUTTON', $
                              VALUE='ascii File')
  
;; END Center pane  ---------------
  
  ;;  Realize the widget hierarchy.
  WIDGET_CONTROL, wTopBase, /REALIZE
  
  ;; Determine the window value of plot windows
  WIDGET_CONTROL, wAreaDraw, GET_VALUE=drawWindowID
  WIDGET_CONTROL, wAreaPlot, GET_VALUE=plotWindowID
  

  dummy_pointer = ptr_new()
  ptOImage = ptr_new(Image)
  ptImage = ptr_new(Image)
  ptCube = ptr_new(Cube)
  ptTotalCube = ptr_new(TotalCube)

  ;;  Create the info structure to pass all info between routines
  sInfo = { $
          DrawWindowID: drawWindowID, $   ; Window ID
          PlotWindowID: plotWindowID, $   ; Window ID
          WTopBase: wTopBase, $           ; Top level base ID
          WPowerLawText:wPowerLawText, $
          WAreaDraw: wAreaDraw, $ 
          WMinScalingSlider: wMinScalingSlider, $
          WMaxScalingSlider: wMaxScalingSlider, $
          WColorTableSlider: wColorTableSlider, $
          WInvertSlider: wInvertSlider, $
          WEqualizeCheck: wEqualizeCheck, $
          WAutoScaleCheck: wAutoScaleCheck, $
          wShowOverlayBase: wShowOverlayBase, $
          wShowOverlayCheck: wShowOverlayCheck, $
          wOverlayColorSlider: wOverlayColorSlider, $
          wOverlayPsymSlider: wOverlayPsymSlider, $
          wRecordButton: wRecordButton, $
          wPlotCommandText: wPlotCommandText, $
          ShowIndicators: 0, $
          ShowFivePixelMean: 0, $
          lunRecord: 0, $
          Recording: 0, $
          nRecordings: 0, $
          Equalize: 0, $
          AutoScale: 0, $
          ;; pointer to the images
          ptOImage: ptOImage, $ ; input image
          ptImage: ptImage, $   ; Scaled Image
          ptCube: ptCube, $   ; Cube
          ptTotalCube: ptTotalCube, $   ; Cube
          havecube: havecube, $ ; It the input a cube?
          wPlaneSlider: wPlaneSlider, $
          wWidthSlider: wWidthSlider, $
          SubtractContinuum: 0, $           ;;Flag to see if we want subtract the continuum;
          nplanes: nplanes, $   ; number of planes in the Cube
          wave: wave, $   ; number of planes in the Cube
          order: order, $   ; number of planes in the Cube
          idxplane: idxplane, $
          channelwidth: channelwidth, $
          ptOverlay: dummy_pointer, $ ; pointer to the overlay data
          ShowOverlay: 0, $           ;;Flag to see if we want overlay;
          OverlayCOlor: 0, $          ;;Flag to see if we want overlay;
          OverlayPsym: 1, $           ;;Flag to see if we want overlay;
          Resize: Resize, $           ;Input switch for resizing
          ResFact: 1d0, $             ;Factor for resizing.
          XSize: 0d0, $               ; Drawing area size
          YSize: 0d0, $
          plotLocked: 0, $
          XSpectrum: 0d0, $
          YSpectrum: 0d0, $
          maskOffsetX: minXmask, $
          maskOffsetY: minYmask , $
          xCursor: 0, $
          yCursor: 0, $
          Absolute: absolute, $
          HighColor: 0, $       ; Highest color index
          MaxImage: 0d0, $
          MinImage: 0d0, $
          RemoveNan: RemoveNan, $
          previousChar: previousChar, $   ; Previous character size
          previousRed: previousRed, $     ; Previous character size
          previousGreen: previousGreen, $ ; Previous character size
          previousBlue: previousBlue, $   ; Previous character size
          plotFont: plotFont $           ; Font to restore
          }
        
  diagnose_cube_DisplayNew,sInfo
  
  ;;  Register the info structure in the user value
  ;;  of the top-level base.
  ;;
    WIDGET_CONTROL, wTopBase, SET_UVALUE=sInfo, /NO_COPY

    ;;  Map the top level base.
    ;;
    WIDGET_CONTROL, wTopBase, MAP=1
    
    ;; Register with the BIG GUY, XMANAGER!
    ;;
    XMANAGER, "diagnose_cube_", wTopBase, $
        EVENT_HANDLER="diagnose_cube_Event", $
        CLEANUP="Diagnose_Cube_Cleanup", NO_BLOCK=no_block
END
