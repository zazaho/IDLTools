;; sub routine to create a fancy map with points sized according to a
;; given weight map. Note that this is not fully implemented yet
;; inside show_image. For the moment it is just a fudge. If a
;; weightmap is given when called it will be applied to the input map once.

;; also to should require resize
function show_image_fancy_map,map,weightmap,factor=factor
  
  ;; over sampling needed to be able to make circles smaller than the
  ;; original pixels
  default,factor,11d0

  ;; scale the map to bytes
  mapmin = min(map,max=mapmax,/nan)
  smap = (map-mapmin)/(mapmax-mapmin)*255.
  smap = finitise(smap)

  ;; scale the weightmap between 0 and 1
  weightmax = max(Weightmap,/nan)
  sWeightmap = (Weightmap)/weightmax
  sWeightmap = finitise(sWeightmap)

  ;; create a unit circle
  ndeg=48
  deg = dindgen(ndeg+1)*2d0*!dpi/double(ndeg)
  xc = cos(deg)
  yc = sin(deg)

  nx=n_elements(map[*,0])
  ny=n_elements(map[0,*])

  ;; plot the fancy map to the z buffer and use the Z buffer image as
  ;; the input map afterwards
  SET_PLOT,'Z'
  device,set_resolution=[nx,ny]*factor

  usersym,xc,yc,/fill
  for xx=0,nx-1 do begin
     for yy=0,ny-1 do begin
        plots,factor*xx*[1,1],factor*yy*[1,1],/device,psym=8, $
              color=smap[xx,yy], $
              symsize=sWeightmap[xx,yy]*factor/7d0
     endfor
  endfor

  ;; read the generate image from the Z buffer
  out=TVRD()
  ;; scale back to the original values
  out=(out/256d0)*(mapmax-mapmin) + mapmin 

  device,/close
  ;; always go back to X; we are a widget remember!
  SET_PLOT,'X'
  
  return,out

end

;--------------------------------------------------------------------
;
;  Purpose:  show_image_DisplayNew
;  Display the widget for the first time with the new images
PRO show_image_DisplayNew, sInfo
  
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
          ;; This is to make the image maximum 400 wide or high
          sInfo.ResFact = (4d2/sInfo.XSize < 4d2/sInfo.YSize)
      ENDIF ELSE BEGIN
          sInfo.ResFact = sInfo.Resize
      ENDELSE
      sInfo.XSize = sInfo.ResFact*sInfo.XSize
      sInfo.YSize = sInfo.ResFact*sInfo.YSize
      ;; keep the old min and max. We dont want the congrid to
      ;; introduce new min or max values
      Omin = min(OImage,max=Omax)
      OImage = CONGRID(OImage,sInfo.XSize,sInfo.YSize,cubic=-0.5)
      OImage = (OImage>Omin)<Omax
  ENDELSE

  ;; Now make sure we delete the old heap variables and simply
  ;; reassign them with the right size
  ptr_free,sInfo.ptOImage,sInfo.ptImage,sInfo.ptOZoomImage,sInfo.ptZoomImage,sInfo.ptOverlay
  sInfo.ptOImage = ptr_new(OImage)
  sInfo.ptImage = ptr_new(OImage)
  sInfo.ptOZoomImage = ptr_new(OImage)
  sInfo.ptZoomImage = ptr_new(OImage)

  ;; resize the draw areas
  WIDGET_CONTROL, sInfo.wAreaDraw, Draw_XSize=sInfo.XSize
  WIDGET_CONTROL, sInfo.wAreaDraw, Draw_YSize=sInfo.YSize
  WIDGET_CONTROL, sInfo.wAreaZoom, Draw_XSize=sInfo.XSize
  WIDGET_CONTROL, sInfo.wAreaZoom, Draw_YSize=sInfo.YSize
  
  ;; Load the colorTable
  show_image_LoadCT,sInfo
  
  ;; Now prepare the Image to be displayed properly
  IF sInfo.Absolute THEN (*sInfo.ptOImage)=abs((*sInfo.ptOImage))
  sInfo.MinImage = min((*sInfo.ptOImage),max=max)
  sInfo.MaxImage = max

  ;; And show the shit
  show_image_refresh,sInfo,/scale,/zoom,/dimage,/dzoom
END                             ;   of show_image_DisplayNew

;--------------------------------------------------------------------
;
PRO show_image_Cleanup, wTopBase
    WIDGET_CONTROL, wTopBase, GET_UVALUE=sInfo,/NO_COPY
    if n_elements(sInfo) ne 0 then begin
                                ;  Restore the previous plot font.
       !P.FONT = sInfo.plotFont
       !P.CHARSIZE = sInfo.previousChar
       ptr_free,sInfo.ptOImage,sInfo.ptImage,sInfo.ptOZoomImage,sInfo.ptZoomImage
    endif
END   ; of show_image_Cleanup


FUNCTION show_image_scale, image
  
  simage= image[sort(image)]
  median = median(simage)
  ximage = dindgen(n_elements(simage))
  
  ;; now give Weights which are inversely propotional to the
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

;
;  Purpose:  do the Zooming
;
PRO show_image_MakeZooming, sInfo
  
  WIDGET_CONTROL, sInfo.wZoomFactorSlider, GET_VALUE=zoomFactor
  ;; Determine the number of pixels left and right from the center
  ;; to use for zooming
  Npix_x = floor(sInfo.XSize/zoomFactor/2)
  Npix_y = floor(sInfo.YSize/zoomFactor/2)
  
  ;; If we are to close to the edge we want to take a point closer to
  ;; the center
  centerZ_x = (sInfo.xCursor>Npix_x) < (sInfo.XSize-Npix_x-1)
  centerZ_y = (sInfo.yCursor>Npix_y) < (sInfo.YSize-Npix_y-1)
  
  ;; Now we make a zoom
  (*sInfo.ptOZoomImage)=CONGRID((*sInfo.ptOImage)[centerZ_x-Npix_x+1: $
                                        centerZ_x+Npix_x, $
                                        centerZ_y-Npix_y+1: $
                                        centerZ_y+Npix_y], $
                           sInfo.XSize,sInfo.YSize,cubic=-0.5)
  
;  The transformation from zoom pixel scale to original pixels:
  sInfo.ZoomOffset_X  =  centerZ_x-Npix_x+1
  sInfo.ZoomOffset_Y  =  centerZ_y-Npix_y+1
  sInfo.ZoomOffset_dX =  double(2*Npix_x-1)/double(sInfo.XSize-1)
  sInfo.ZoomOffset_dY =  double(2*Npix_y-1)/double(sInfo.YSize-1)
END                             ;  of show_image_MakeZooming


;--------------------------------------------------------------------
;
;  Purpose:  Do the pixel scaling
;
PRO show_image_MakeScaling, sInfo
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
  ;; raise to the power power (Weighting function)
  ;; invert can be 0 or 1
  ;;    if 0 do nothing (multiply by 1 and add 0)
  ;;    if 1 flip the values (multiply by -1 and add 1)
  ;; finally multiply by the total number of colors available (highcolor)
  
  (*sInfo.ptImage)=sInfo.highColor*(((((*sInfo.ptOImage)>cutLo)<cutHi-cutLo)/ $
                                (cutHi-cutLo))^power*(1d0-2d0*Invert)+Invert)
  (*sInfo.ptZoomImage)=sInfo.highColor*(((((*sInfo.ptOZoomImage)>cutLo)<cutHi-cutLo)/ $
                                (cutHi-cutLo))^power*(1d0-2d0*Invert)+Invert)
  
  IF sInfo.Equalize THEN BEGIN
      hist = histogram((*sInfo.ptImage))
      ;; make a cummulative increasing scale
      FOR i = 1,n_elements(hist)-1 DO hist[i] = hist[i]+hist[i-1]
      ;; scale to the number of available colors
      hist = long(bytscl(hist, top = sInfo.highColor))
      ;; project the values on the equalized colors
      (*sInfo.ptImage) = interpol(hist,dindgen(n_elements(hist)),(*sInfo.ptImage))
      (*sInfo.ptZoomImage)=interpol(hist,dindgen(n_elements(hist)),(*sInfo.ptZoomImage))
  ENDIF  
  
END                             ;   of show_image_MakeScaling

;--------------------------------------------------------------------
;
;  Purpose:  Plot the image
;
PRO show_image_Display, WindowID, Image
  WSET, WindowID
  ERASE
  TV,image
END                             ;   of show_image_Display


;--------------------------------------------------------------------
;
;  Purpose:  Refresh the complete display
;
PRO show_image_refresh,sInfo,scale=scale,zoom=zoom,dimage=dimage, $
                       dzoom=dzoom
  IF keyword_set(zoom) THEN show_image_MakeZooming,sInfo
  IF keyword_set(scale) THEN show_image_MakeScaling,sInfo
  IF keyword_set(dimage) THEN show_image_Display,sInfo.drawWindowID,(*sInfo.ptImage)
  IF keyword_set(dzoom) THEN show_image_Display,sInfo.zoomWindowID,(*sInfo.ptZoomImage)
  IF sInfo.ShowOverlay THEN show_image_DisplayOverlay,sInfo
END 

;--------------------------------------------------------------------
;
;  Purpose:  Plot the overlay points on top of the images
;
PRO show_image_DisplayOverlay, sInfo
  
  overlay = (*sInfo.ptOverlay)
  IF n_elements(overlay) EQ 0 THEN return

  WIDGET_CONTROL, sInfo.wOverlayPsymSlider, GET_VALUE=psym
  WIDGET_CONTROL, sInfo.wOverlayColorSlider, GET_VALUE=color

  IF psym NE 8 THEN BEGIN
      WSET, sInfo.DrawWindowID
      plots,overlay[0,*]*sInfo.ResFact,overlay[1,*]*sInfo.ResFact,ps=psym, $
        color=color,/device

      WSET, sInfo.ZoomWindowID
      plots,(sInfo.ResFact*overlay[0,*]-sInfo.ZoomOffset_X)/sInfo.ZoomOffset_dX, $
        (sInfo.ResFact*overlay[1,*]-sInfo.ZoomOffset_Y)/sInfo.ZoomOffset_dY, $
        ps=psym,color=color,/device
  ENDIF ELSE BEGIN
      labels = (STRCOMPRESS(sindgen(n_elements(overlay[0,*]+1)),/remove_all))[1:*]
      
      WSET, sInfo.DrawWindowID
      xyouts,overlay[0,*]*sInfo.ResFact,overlay[1,*]*sInfo.ResFact,labels, $
        color=color,/device,align=0.5
      
      WSET, sInfo.ZoomWindowID
      xyouts,(sInfo.ResFact*overlay[0,*]-sInfo.ZoomOffset_X)/sInfo.ZoomOffset_dX, $
        (sInfo.ResFact*overlay[1,*]-sInfo.ZoomOffset_Y)/sInfo.ZoomOffset_dY, $
        labels,color=color,/device,align=0.5
  ENDELSE 
END                             ;   of show_image_DisplayOverlay

;--------------------------------------------------------------------
;
;  Purpose:  load a new colortable
;
PRO show_image_LoadCT, sInfo
  WIDGET_CONTROL, sInfo.wColorTableSlider, GET_VALUE=colortable
  loadct,colortable,/silent
  sInfo.highColor = !D.TABLE_SIZE-1
END                             ;   of show_image_loadCT

;--------------------------------------------------------------------
;
PRO show_image_Event, sEvent
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
          IF sEvent.Press EQ 1 THEN BEGIN
            sInfo.xCursor = sEvent.x
            sInfo.yCursor = sEvent.y
            show_image_refresh,sInfo,/scale,/zoom,/dzoom
          ENDIF
        END                     ;  of  DRAWING
        
        'ZOOMING' : BEGIN
            ;; The coordinates in the zoom image  
          Zx = sEvent.x
          Zy = sEvent.y
          ;; The coordinates in the original image  
          Ox = sInfo.ZoomOffset_X+Zx*sInfo.ZoomOffset_dX
          Oy = sInfo.ZoomOffset_Y+Zy*sInfo.ZoomOffset_dY
          ;; The range of X and Y in the zoomed image
          Zxr = sInfo.ZoomOffset_X+findgen(sInfo.Xsize)*sInfo.ZoomOffset_dX
          Zyr = sInfo.ZoomOffset_Y+findgen(sInfo.Ysize)*sInfo.ZoomOffset_dY
          CASE sEvent.press OF 
              0:BEGIN
                  text=string(format= $
                              '("x:",F6.1," y:",F6.1," value: ",E8.1)', $
                              Ox/double(sInfo.ResFact*sInfo.FancyResFact),Oy/double(sInfo.ResFact*sInfo.FancyResFact), $
                              (*sInfo.ptOZoomImage)[(Zx>0)<(sInfo.XSize-1),(Zy>0)<(sInfo.YSize-1)])
                  WIDGET_CONTROL, sInfo.wStatusLabel, SET_VALUE=text
              END
              1:BEGIN
                  WSET,sInfo.PlotWindowID
                  plot,Zxr,(*sInfo.ptOZoomImage)[*,Zy],xmargin=[5,1], $
                    ymargin=[2,2], $
                    title='horizontal slice',xstyle=1,ystyle=1
                  oplot,[Ox,Ox],!y.crange,linestyle=1
                  IF sInfo.Recording THEN BEGIN
                      printf,sInfo.lunRecord, format= '(2F8.2,E11.3)', $
                        Ox/double(sInfo.ResFact),Oy/double(sInfo.ResFact), $
                        (*sInfo.ptOZoomImage)[Zx,Zy]
                  ENDIF
              END
              2: BEGIN
                  WIDGET_CONTROL, sInfo.wMinScalingSlider, GET_VALUE=minValue
                  WIDGET_CONTROL, sInfo.wMaxScalingSlider, GET_VALUE=maxValue
                
                cutLo = minValue/255d0*sInfo.highColor
                cutHi = maxValue/255d0*sInfo.highColor
                
                WSET,sInfo.PlotWindowID
                xvals = (minValue+(maxValue-minValue)* $
                         dindgen(256)/255.0)[indgen(254)+1]
                yvals = (histogram((*sInfo.ptZoomImage)))[indgen(254)+1]
                
                plot,xvals,yvals,xmargin=[5,1],ymargin=[2,2], $
                  title='histogram',xstyle=1,ystyle=1
            END
            4: BEGIN
              WSET,sInfo.PlotWindowID
              plot,Zyr,(*sInfo.ptOZoomImage)[Zx,*],xmargin=[5,1],ymargin=[2,2], $
                title='vertical slice',xstyle=1,ystyle=1
              oplot,[Oy,Oy],!y.crange,linestyle=1
            END
            ELSE: BEGIN
            END
          ENDCASE
        END                     ;  of  DRAWING
        
        'ZOOMFACTOR' : BEGIN
            show_image_refresh,sInfo,/zoom,/scale,/dzoom
        END                     ;       of  ZOOMFACTOR
        
        'MINSCALING' : BEGIN
          WIDGET_CONTROL, sInfo.wMinScalingSlider, GET_VALUE=minValue
          WIDGET_CONTROL, sInfo.wMaxScalingSlider, GET_VALUE=maxValue
          IF (maxValue LE minValue) THEN BEGIN
            maxValue = minValue + 1
            WIDGET_CONTROL, sInfo.wMaxScalingSlider, SET_VALUE=maxValue
          ENDIF
          show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  MINSCALING
        
        'MAXSCALING' : BEGIN
          WIDGET_CONTROL, sInfo.wMinScalingSlider, GET_VALUE=minValue
          WIDGET_CONTROL, sInfo.wMaxScalingSlider, GET_VALUE=maxValue
          IF (minValue GE maxValue) THEN BEGIN
            minValue = maxValue - 1
            WIDGET_CONTROL, sInfo.wMinScalingSlider, SET_VALUE=minValue
          ENDIF
          show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  MAXSCALING
        
        'POWERLAW' : BEGIN
            show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  POWERLAW
        
        'INVERT' : BEGIN
            show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  ZOOMPSBUTTON

        'AUTOSCALE' : BEGIN
            sInfo.AutoScale = NOT sInfo.AutoScale
            IF sInfo.AutoScale THEN BEGIN
                AutoScale = show_image_scale((*sInfo.ptOImage))
                sInfo.minImage = AutoScale[0]
                sInfo.maxImage = AutoScale[1]
            ENDIF ELSE BEGIN
                sInfo.MinImage = min((*sInfo.ptOImage),max=max)
                sInfo.MaxImage = max
            ENDELSE
            show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  ZOOMPSBUTTON
        
        'EQUALIZE' : BEGIN
            sInfo.Equalize = NOT sInfo.Equalize
            show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  ZOOMPSBUTTON

        'COLORTABLE' : BEGIN
           show_image_loadct,sInfo
           show_image_refresh,sInfo,/scale,/dimage,/dzoom
        END                     ;       of  COLORTABLE
        
        'OPEN' : BEGIN
          file = dialog_pickfile(FILTER='*.fits',/MUST_EXIST)
          IF file EQ '' THEN BEGIN
          END ELSE BEGIN
            (*sInfo.ptOImage) = readfits(file)
            show_image_DisplayNew,sInfo
          ENDELSE
        END                     ;       of  OPEN
        
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
              show_image_refresh,sInfo,/dimage,/dzoom
          ENDELSE
        END                     ;       of  OVERLAY
        
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
                ENDIF
            ENDELSE
        END                     ;       of  RECORD
        
        'PSBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.ps')
          a4port = (sInfo.XSize LT sInfo.YSize)
          a4land = NOT a4port
          sh_ps,/color,outfile,bits=8,a4port=a4port,a4land=a4land
          TV,(*sInfo.ptImage)
          sh_ps
        END                     ;       of  PSBUTTON
        
        'ZOOMPSBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.ps')
          a4port = (sInfo.XSize LT sInfo.YSize)
          a4land = NOT a4port
          sh_ps,/color,outfile,bits=8,a4port=a4port,a4land=a4land
          TV,(*sInfo.ptZoomImage)
          sh_ps
        END                     ;       of  ZOOMPSBUTTON
        
        'FILEBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.fits')
          if outfile ne '' then begin
              writefits,outfile,long((*sInfo.ptImage))
          endif
        END                     ;       of  ZOOMFILEBUTTON

        'ZOOMFILEBUTTON' : BEGIN
          outfile = dialog_pickfile(/write,filter='*.fits')
          if outfile ne '' then begin
              writefits,outfile,long((*sInfo.ptZoomImage))
          endif
        END                     ;       of  ZOOMFILEBUTTON
        
        'SHOWOVERLAY' : BEGIN
            sInfo.ShowOverlay = NOT sInfo.ShowOverlay
            show_image_refresh,sInfo,/dimage,/dzoom
        END                     ;       of  ZOOMPSBUTTON
        
        'OVERLAYCOLOR' : BEGIN
            WIDGET_CONTROL, sInfo.wOverlayColorSlider, GET_VALUE=overlaycolor
            sInfo.OverlayColor = OverlayColor
            show_image_refresh,sInfo,/dimage,/dzoom
        END                     ;       of  ZOOMPSBUTTON

        'OVERLAYPSYM' : BEGIN
            WIDGET_CONTROL, sInfo.wOverlayPsymSlider, GET_VALUE=overlaypsym
            sInfo.OverlayPsym = Overlaypsym
            show_image_refresh,sInfo,/dimage,/dzoom
        END                     ;       

        'PLANESLIDER' : BEGIN
            WIDGET_CONTROL, sInfo.wPlaneSlider, GET_VALUE=idxplane
            if sInfo.idxplane ne idxplane then begin
               sInfo.idxplane = idxplane
               ptr_free,sInfo.ptOImage
               sInfo.ptOImage = ptr_new(reform((*sInfo.ptCube)[*,*,idxplane]))
               show_image_DisplayNew,sInfo
            endif
        END                     ;       

        ELSE :                  ;  do nothing
      ENDCASE
      WIDGET_CONTROL, sEvent.top, SET_UVALUE=sInfo,/NO_COPY
    ENDELSE
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
PRO show_image, InImage, $
                absolute=absolute, $
                colortable=colortable, $
                out=out, $
                resize=resize, $
                nan=nan, $
                no_block=no_block , $
                min=min, $
                max=max, $
                WeightImage=WeightImage, $
                Mask=Mask
  
  absolute=keyword_set(absolute)
  IF NOT keyword_set(colortable) THEN colortable=3
  if keyword_set(nan) then RemoveNan=1 else RemoveNan=0
  IF n_elements(no_block) eq 0 then no_block=1

  ;;  Save the font
  ;;
  plotFont = !P.FONT
  previousChar = !P.CHARSIZE
  ;;!P.CHARSIZE = 8.0 / !D.X_CH_SIZE
  
  ;;  Use hardware-drawn font.
  ;;
  ;;!P.FONT=0
  
  IF n_elements(InImage) EQ 0 THEN BEGIN
     filename = dialog_pickfile(FILTER='*.fits',/MUST_EXIST)
     if filename eq '' then begin
        ;; graciously exit 
        message,/info,'No input image given and none selected from the file system'
        message,/info,'Returning'
        return
     endif else begin
        InImage = readfits(filename)
     endelse
  ENDIF 

  Image=InImage

  ;; Apply limits if asked for
  if n_elements(min) eq 1 then Image = (Image>min)
  if n_elements(max) eq 1 then Image = (Image<max)

  ;; allow for image cubes as well and check for proper input
  sizeimage = size(Image)
  case sizeimage[0] of
     1: begin
        message,/info,'The input is not an image it has only one dimension'
        return
     end
     2: begin
        ;; what we expect do nothing
        havecube = 0
        nplanes = 1
        idxplane = 0
     end
     3: begin
        ;; empty last dimension
        if sizeimage[3] eq 1 then begin
           Image = reform(Image)
           havecube = 0
           nplanes = 1
           idxplane = 0
        endif else begin
           Cube = Image
           idxplane = 0
           Image = reform(Cube[*,*,idxplane])
           havecube = 1
           nplanes = sizeimage[3]
        endelse
     end
     else: begin
        message,/info,'The input does not appeat to be an image or cube'
        return
     end
  endcase
  
  Image = DOUBLE(Image)

  if n_elements(mask) ne 0 then begin
     alldatamask = mask eq 0
     Xmask = where(total(alldatamask,2) ne 0)
     Ymask = where(total(alldatamask,1) ne 0)
     minXmask =min(xmask,max=maxXmask)
     minYmask =min(ymask,max=maxYmask)
     Image = Image[minXmask:maxXmask,minYmask:maxYmask]
     if havecube then begin
        Cube = Cube[minXmask:maxXmask,minYmask:maxYmask,*]
     endif
  endif     
  
  IF n_elements(WeightImage) ne 0 then begin
     if n_elements(mask) ne 0 then begin
        WeightImage = WeightImage[minXmask:maxXmask,minYmask:maxYmask]
     endif
     FancyResFact = 11d0
     Image = show_image_fancy_map(Image,WeightImage,factor=FancyResFact)
     ;; resize is needed because of oversampled maps
     resize = 1
  endif else begin
     FancyResFact = 1d0
  endelse

  ;; to resize the image
  ;; if resize=1 element then make the largest 400 or do factor 
  ;; else resize by the factor given
  IF NOT keyword_set(resize) THEN resize = 0 ELSE resize = resize[0]

;; Now create the widget
  wTopBase = WIDGET_BASE(TITLE="Show Image", /COLUMN, MAP=0, MBAR=barBase)
  
  ;;  Create the menu bar items
  ;;
  wFileButton = WIDGET_BUTTON(barBase, VALUE='File')
  
  wOpenButton = WIDGET_BUTTON(wFileButton, VALUE='Open', UVALUE='OPEN')
  wOverlayButton = WIDGET_BUTTON(wFileButton, VALUE='Overlay', UVALUE='OVERLAY')
  wRecordButton = WIDGET_BUTTON(wFileButton, VALUE='Record Positions', UVALUE='RECORD')
  wQuitButton = WIDGET_BUTTON(wFileButton, VALUE='Quit', UVALUE='QUIT',/separator)

  ;;  Create the left, center and right bases
  ;;
  wTopRowBase = WIDGET_BASE(wTopBase, COLUMN=3)
  
;; Left pane  ---------------
  wLeftBase = WIDGET_BASE(wTopRowBase, /COLUMN, /FRAME)

  wPowerLawLabel = WIDGET_LABEL(wLeftBase, VALUE='Power Law:')
  
  wPowerLawText = WIDGET_TEXT(wLeftBase, /EDITABLE, VALUE='1.0', $
                              UVALUE='POWERLAW')
  
  wMinScalingSlider = WIDGET_SLIDER(wLeftBase, MINIMUM=0,  $
                                    MAXIMUM=254, VALUE=0, $
                                    TITLE='Minimum', UVALUE='MINSCALING')
  
  wMaxScalingSlider = WIDGET_SLIDER(wLeftBase, MINIMUM=1,  $
                                    MAXIMUM=255, VALUE=255, $
                                   TITLE='Maximum', UVALUE='MAXSCALING')
  wAreaPlot = WIDGET_DRAW(wLeftBase, XSIZE=200, YSIZE=200, RETAIN=2,  $
                          UVALUE='PLOTTING')

  wOverlayBase = WIDGET_BASE(wLeftBase, COLUMN=1, /FRAME)
  wShowOverlayBase = WIDGET_BASE(wOverlayBase, /nonexclusive,/column)
  wShowOverlayCheck = WIDGET_BUTTON(wShowOverlayBase, $
                                    VALUE='Show Overlay',  $
                                    UVALUE='SHOWOVERLAY',sensitive=0)
  wOverlayColorSlider = WIDGET_SLIDER(wOverlayBase, MINIMUM=0,  $
                                    MAXIMUM=254, VALUE=0, $
                                    TITLE='Overlay Color', UVALUE='OVERLAYCOLOR')
  wOverlayPsymSlider = WIDGET_SLIDER(wOverlayBase, MINIMUM=1,  $
                                    MAXIMUM=8, VALUE=1, $
                                    TITLE='Overlay Psym', UVALUE='OVERLAYPSYM')
;; END Left pane  ---------------
  
;; Center pane  ---------------
  wCenterBase = WIDGET_BASE(wTopRowBase, /COLUMN)
  wCenterHiBase = WIDGET_BASE(wCenterBase, /COLUMN, /FRAME)
  wCenterLoBase = WIDGET_BASE(wCenterBase, COLUMN=3, /FRAME)
  
  wDrawLabel = WIDGET_LABEL(wCenterHiBase, VALUE='Image')
  
  wAreaDraw = WIDGET_DRAW(wCenterHiBase, XSIZE=XSize, YSIZE=YSize, RETAIN=2,  $
                          UVALUE='DRAWING', /MOTION, /BUTTON)
  
  wPSButton = WIDGET_BUTTON(wCenterHiBase, UVALUE='PSBUTTON', VALUE='.ps File')

  wFileButton = WIDGET_BUTTON(wCenterHiBase, UVALUE='FILEBUTTON', $
                              VALUE='.fits File')
  
  wColorTableSlider = WIDGET_SLIDER(wCenterLoBase, MINIMUM=0, MAXIMUM=40, $
                                    VALUE=colortable, TITLE='Color table',  $
                                    UVALUE='COLORTABLE')
  wInvertSlider = WIDGET_SLIDER(wCenterLoBase, MINIMUM=0, MAXIMUM=1, $
                                    VALUE=0, TITLE='Invert',  $
                                    UVALUE='INVERT')

  wEqualizeBase = WIDGET_BASE(wCenterLoBase, /nonexclusive,/column)
  wEqualizeCheck = WIDGET_BUTTON(wEqualizeBase, $
                                    VALUE='Equalize',  $
                                    UVALUE='EQUALIZE')

  wAutoScaleCheck = WIDGET_BUTTON(wEqualizeBase, $
                                    VALUE='Auto Scale',  $
                                    UVALUE='AUTOSCALE')
  ;;  Create a plane slider if a cube is given
  if havecube eq 1 then begin
     wPlaneSliderBase = WIDGET_BASE(wCenterBase)
     wPlaneSlider = WIDGET_SLIDER(wPlaneSliderBase, MINIMUM=0,  $
                                    MAXIMUM=nplanes-1, VALUE=0, $
                                    TITLE='Plane', UVALUE='PLANESLIDER')
  endif else begin
     wPlaneSlider = -1
  endelse
  
;; END Center pane  ---------------
  
;; Right pane  ---------------
  wRightBase = WIDGET_BASE(wTopRowBase, /COLUMN)
  wRightHiBase = WIDGET_BASE(wRightBase, /COLUMN, /FRAME)
  wRightLoBase = WIDGET_BASE(wRightBase, /COLUMN, /FRAME)
  
  wZoomLabel = WIDGET_LABEL(wRightHiBase, VALUE='Zoom')
  
  wAreaZoom = WIDGET_DRAW(wRightHiBase, XSIZE=XSize, YSIZE=YSize, RETAIN=2,  $
                          UVALUE='ZOOMING', UNAME='show_image:zoom', /MOTION, $
                          /BUTTON)
  
  wZoomPSButton = WIDGET_BUTTON(wRightHiBase, UVALUE='ZOOMPSBUTTON',  $
                                VALUE='.ps File')
  wZoomFileButton = WIDGET_BUTTON(wRightHiBase, UVALUE='ZOOMFILEBUTTON',  $
                                VALUE='.fits File')
  
  wZoomFactorSlider = WIDGET_SLIDER(wRightLoBase, MINIMUM=1, MAXIMUM=20, $
                                    VALUE=4, TITLE='Zoom factor',  $
                                    UVALUE='ZOOMFACTOR')
  wStatusLabel = WIDGET_LABEL(wRightBase, FRAME=1, XSIZE=300, VALUE='Statusbar')
;; END Right pane  ---------------
  
  ;;  Realize the widget hierarchy.
  WIDGET_CONTROL, wTopBase, /REALIZE
  
  ;; Determine the window value of plot windows
  WIDGET_CONTROL, wAreaDraw, GET_VALUE=drawWindowID
  WIDGET_CONTROL, wAreaZoom, GET_VALUE=zoomWindowID
  WIDGET_CONTROL, wAreaPlot, GET_VALUE=plotWindowID
  

  dummy_pointer = ptr_new()
  ptOImage = ptr_new(Image)
  ptOZoomImage = ptr_new(Image)
  ptImage = ptr_new(Image)
  ptZoomImage = ptr_new(Image)
  ptCube = ptr_new(Cube)

  ;;  Create the info structure to pass all info between routines
  sInfo = { $
          DrawWindowID: drawWindowID, $   ; Window ID
          ZoomWindowID: zoomWindowID, $   ; Window ID
          PlotWindowID: plotWindowID, $   ; Window ID
          WTopBase: wTopBase, $           ; Top level base ID
          WPowerLawText:wPowerLawText, $
          WAreaDraw: wAreaDraw, $ 
          WAreaZoom: wAreaZoom, $
          WMinScalingSlider: wMinScalingSlider, $
          WMaxScalingSlider: wMaxScalingSlider, $
          WZoomFactorSlider: wZoomFactorSlider, $
          WColorTableSlider: wColorTableSlider, $
          WInvertSlider: wInvertSlider, $
          WEqualizeCheck: wEqualizeCheck, $
          WAutoScaleCheck: wAutoScaleCheck, $
          wShowOverlayBase: wShowOverlayBase, $
          wShowOverlayCheck: wShowOverlayCheck, $
          wOverlayColorSlider: wOverlayColorSlider, $
          wOverlayPsymSlider: wOverlayPsymSlider, $
          wRecordButton: wRecordButton, $
          lunRecord: 0, $
          Recording: 0, $
          Equalize: 0, $
          AutoScale: 0, $
          WStatusLabel: wStatusLabel, $
          ;; pointer to the images
          ptOImage: ptOImage, $ ; input image
          ptImage: ptImage, $   ; Scaled Image
          ptCube: ptCube, $   ; Cube
          havecube: havecube, $ ; It the input a cube?
          wPlaneSlider: wPlaneSlider, $
          nplanes: nplanes, $   ; number of planes in the Cube
          idxplane: idxplane, $
          ptOZoomImage: ptOZoomImage, $ ; Zoom image
          ptZoomImage: ptZoomImage, $   ; Zoomed scaled image
          ZoomOffset_X: 0, $
          ZoomOffset_Y: 0, $
          ZoomOffset_dX: 0d0, $
          ZoomOffset_dY: 0d0, $
          ptOverlay: dummy_pointer, $ ; pointer to the overlay data
          ShowOverlay: 0, $           ;;Flag to see if we want overlay;
          OverlayCOlor: 0, $          ;;Flag to see if we want overlay;
          OverlayPsym: 1, $           ;;Flag to see if we want overlay;
          Resize: Resize, $           ;Input switch for resizing
          ResFact: 1d0, $             ;Factor for resizing.
          FancyResFact: FancyResFact, $             ;Factor for resizing.
          XSize: 0d0, $               ; Drawing area size
          YSize: 0d0, $
          xCursor: 0, $
          yCursor: 0, $
          Absolute: absolute, $
          HighColor: 0, $       ; Highest color index
          MaxImage: 0d0, $
          MinImage: 0d0, $
          RemoveNan: RemoveNan, $
          previousChar : previousChar, $ ; Previous character size
          plotFont: plotFont $           ; Font to restore
          }
        
  show_image_DisplayNew,sInfo
  
  ;;  Register the info structure in the user value
  ;;  of the top-level base.
  ;;
    WIDGET_CONTROL, wTopBase, SET_UVALUE=sInfo, /NO_COPY

    ;;  Map the top level base.
    ;;
    WIDGET_CONTROL, wTopBase, MAP=1
    
    ;; Register with the BIG GUY, XMANAGER!
    ;;
    XMANAGER, "show_image_", wTopBase, $
        EVENT_HANDLER="show_image_Event", $
        CLEANUP="Show_Image_Cleanup", NO_BLOCK=no_block
END   ; of Show_Image
