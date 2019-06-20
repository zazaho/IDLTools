FUNCTION read_lnk_read_data,filename,prefix=prefix

  IF !version.release LT '5.5' THEN BEGIN
      pathsep = '/';;path_sep()
  ENDIF ELSE BEGIN
      pathsep = path_sep()
  ENDELSE

  IF keyword_set(prefix) THEN BEGIN
      IF strmid(prefix,0,1,/reverse_offset) NE pathsep THEN BEGIN
          prefix = prefix+pathsep
      ENDIF
      f = prefix+filename
  ENDIF ELSE BEGIN
      f = filename
  ENDELSE
  return,(read_ascii(shell_expand(f))).(0)
END

;; The function that does all the work inside the widget.
;; Every event enter here and it branches depending on the eventUValue
PRO read_lnk_Event, sEvent
  
  ;; Which event?
  WIDGET_CONTROL, sEvent.id, GET_UVALUE=eventUValue
  
  ;; get the info structure from the top
  WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo,/NO_COPY

  pathsep = path_sep()
  CASE eventUValue OF
      
      'CONSTRAINT': BEGIN
          
          ;;remember the oldcriterium for the parameter
          currentfilename = sInfo.CurrentFilename
          
          ;; Get the info from the boxes
          WIDGET_CONTROL, sInfo.wMaterialText,GET_VALUE=material
          WIDGET_CONTROL, sInfo.wFilenameText,GET_VALUE=filename
          WIDGET_CONTROL, sInfo.wCommentText,GET_VALUE =comment
          source_idx = WIDGET_INFO(sInfo.wSourceDrop,/DROPLIST_SELECT)
          WIDGET_CONTROL, sInfo.wLambda1Text,GET_VALUE =lambda1
          WIDGET_CONTROL, sInfo.wLambda2Text,GET_VALUE =lambda2

          material = (strtrim(material,2))[0]
          filename = (strtrim(filename,2))[0]
          comment  = (strtrim(comment ,2))[0]
          lambda1  = (strtrim(lambda1 ,2))[0]
          lambda2  = (strtrim(lambda2 ,2))[0]
          
          idx = make_array(n_elements(sInfo.Lnk_Headers.filename),value=1)
          
          IF filename NE '' THEN BEGIN
              idx = idx AND stregex(sInfo.Lnk_Headers.Filename,'.*'+filename+'.*',/boolean,/fold_case)
          ENDIF
          
          IF material NE '' THEN BEGIN
              idx = idx AND stregex(sInfo.Lnk_Headers.Material,'.*'+material+'.*',/boolean,/fold_case)
          ENDIF
          
          IF comment NE '' THEN BEGIN
              idx = idx AND stregex(sInfo.Lnk_Headers.Comment,'.*'+comment+'.*',/boolean,/fold_case)
          ENDIF

          IF source_idx NE 0 THEN BEGIN
              source = sInfo.Sources[source_idx]
              idx = idx AND stregex(sInfo.Lnk_Headers.Source,'.*'+Source+'.*',/boolean,/fold_case)
          ENDIF
          
          IF lambda1 NE '' THEN BEGIN
              lambda1=lambda1[0]
              lambda1 = float(lambda1)
              IF lambda1 GT 0.0 THEN BEGIN
                  idx = idx AND (sInfo.Lnk_Headers.Minwave LE lambda1)  AND (sInfo.Lnk_Headers.Maxwave GE lambda1)
              ENDIF
          ENDIF
          
          IF lambda2 NE '' THEN BEGIN
              lambda2=lambda2[0]
              lambda2 = float(lambda2)
              IF lambda2 GT 0.0 THEN BEGIN
                  idx = idx AND (sInfo.Lnk_Headers.Minwave LE lambda2)  AND (sInfo.Lnk_Headers.Maxwave GE lambda2)
              ENDIF
          ENDIF
          
          sInfo.IdxDisplayed = idx
          
          keep = where(idx NE 0,cnt)
          IF cnt NE 0 THEN begin
              WIDGET_CONTROL,sInfo.wMaterialList,set_value=sInfo.Lnk_Headers.filename[keep]
              WIDGET_CONTROL,sInfo.wCommentList,set_value=['']
              WIDGET_CONTROL,sInfo.wFullPathText,set_value=''
              
              newidx = where(sInfo.Lnk_Headers.Filename[keep] EQ currentfilename,cnt)
              IF cnt NE 0 THEN BEGIN
                  WIDGET_CONTROL,sInfo.wMaterialList,SET_LIST_SELECT=newidx
                  newidx = LONG(newidx[0])
                  WIDGET_CONTROL,sInfo.wMaterialList, $
                    SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:newidx, CLICKS:1L}
              ENDIF 
          ENDIF ELSE BEGIN
              WIDGET_CONTROL,sInfo.wMaterialList,set_value=['']
              WIDGET_CONTROL,sInfo.wCommentList,set_value=['']
              WIDGET_CONTROL,sInfo.wFullPathText,set_value=''
          ENDELSE
      END
      
      'LIST': BEGIN
          keep = where(sInfo.IdxDisplayed NE 0,cnt)
          IF cnt NE 0 THEN BEGIN
              IF sEvent.Clicks EQ 2 THEN BEGIN
                  IF sInfo.no_block EQ 0 THEN BEGIN
                      *sInfo.ptrToFormData ={cancel:0,SelectedIdx:keep[sEvent.Index]}
                      Widget_Control, sEvent.top, /Destroy
                      return
                  ENDIF
              ENDIF ELSE BEGIN
                  ;; Update the CommentList to show the dirs i.e. the comment
                  WIDGET_CONTROL,sInfo.wCommentList,SET_VALUE= $
                    [(sInfo.Lnk_Headers.Material[keep])[sEvent.Index], $
                     '---------------------------------', $
                     strsplit((sInfo.Lnk_Headers.Comment[keep])[sEvent.Index],string(10B),/extract)]
                  WIDGET_CONTROL,sInfo.wFullPathText, $
                    SET_VALUE=(sInfo.Lnk_Headers.Path[keep])[sEvent.Index]+pathsep+(sInfo.Lnk_Headers.Filename[keep])[sEvent.Index]
                  
                  ;; Execute a command if command edit is not empty
                  WIDGET_CONTROL, sInfo.wCommandText,GET_VALUE=command
                  
                  ;; this seems needed because otherwise we'd get an array
                  ;; of strings
                  command=strtrim(command[0],2)
                  IF command NE '' THEN BEGIN
                      data = read_lnk_read_data((sInfo.Lnk_Headers.Filename[keep])[sEvent.Index], $
                                                  prefix=(sInfo.Lnk_Headers.Path[keep])[sEvent.Index])
                      dpos = strpos(command,'%d')
                      WHILE dpos NE -1 DO BEGIN
                          command = strmid(command,0,dpos)+'data'+strmid(command,dpos+2)
                          dpos = strpos(command,'%d')
                      ENDWHILE
                      ;; just in case it is a plot command set the
                      ;; proper windows
                      wset,sInfo.PlotID
                      foo = execute(command[0])
                  ENDIF
              ENDELSE
          ENDIF
      END
      
      'COMMAND' : BEGIN
          ;; We have a new plot command. Check to see if any data was
          ;; selected in the list. If so execute to command by
          ;; simulting a click 
          idx=WIDGET_INFO(sInfo.wMaterialList,/LIST_SELECT)
          IF idx NE -1 THEN BEGIN
              WIDGET_CONTROL,sInfo.wMaterialList, $
                SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:idx, CLICKS:1L}
          ENDIF
      END             
      
      'READDATA' : BEGIN
          idx=WIDGET_INFO(sInfo.wMaterialList,/LIST_SELECT)
          IF idx NE -1 THEN BEGIN
              keep = where(sInfo.IdxDisplayed NE 0,cnt)
              *sInfo.ptrToFormData ={cancel:0,SelectedIdx:keep[idx]}
          ENDIF ELSE BEGIN
              *sInfo.ptrToFormData ={cancel:1,SelectedIdx:-1}
          ENDELSE
          Widget_Control, sEvent.top, /Destroy
          return
      END 

      'CLOSE' : BEGIN
          WIDGET_CONTROL, sEvent.top, /DESTROY
          return
      END             
      
      'HELP' : BEGIN
          htxt = 'Display and/or read lnk data from local database'
          htxt = [htxt,'Use the list in the middle to display absorption values of the materials']
          htxt = [htxt,'The column on the left (constraints) can be used to limit the materials']
          htxt = [htxt,'Select a source to only show the data comming from that particular database']
          htxt = [htxt,'Material are either in descriptive form (amorphous olivine) or']
          htxt = [htxt,'  in chemical notation (Al2O3), depending on the database.']
          htxt = [htxt,'Filename selects on the name of the .lnk file like Ag.lnk']
          htxt = [htxt,'Comments is most general and can be used to select data from a certain'] 
          htxt = [htxt,'  reference (jaeger) or with a certain keyword (temperature)']
          htxt = [htxt,'Wavelength 1/2 can be used to select only data that bracket those numbers']
          htxt = [htxt,'  in micrometer. Use both to ensure a range to be present (e.g. 2,45)'] 
          htxt = [htxt,'  reference (jaeger) or with a certain keyword (temperature)']
          htxt = [htxt,'']
          htxt = [htxt,'In anycase a certain familiarity with the type of data is needed']
          htxt = [htxt,'The fullpath box shows the place where the data can be read from.']
          htxt = [htxt,'The plotting command can be adapted to taste but probable plq is most useful.']
          htxt = [htxt,'Useful options are /CDE,/NEEDLE,/DISK to choose a shape (distibution)']
          htxt = [htxt,'                   TEMP=1000,RADIUS=0.1']
          htxt = [htxt,'Use /help for general plotting options; output in the IDL window.']
          htxt = [htxt,'']
          htxt = [htxt,'By Double clicking a material the widget is exited and the clicked data is']
          htxt = [htxt,'returned (in read_lnk mode).']

          display_text_widget,htxt,title='Read LNK Help'
     
      END ;; help

      ELSE:                     ;  do nothing
  ENDCASE

  WIDGET_CONTROL, sEvent.top, SET_UVALUE=sInfo,/NO_COPY
  
END


FUNCTION read_lnk,filename,command=command,separator=separator, $
                    no_block=no_block,plotwindow=plotwindow

;; if this is set then do not block the widget but instead return
;; right after creating the widget
  default,no_block,0

;; if this is set then create an draw widget to use for the plotting
;; routing instead of using the standard plotting device if any
  plotwindow = keyword_set(plotwindow)
  
;; First check if the $OPTICALS_PATH variable is set:
  opticals_path = getenv('OPTICALS_PATH')
;; If not print this warning and assume it is the current path
  IF (opticals_path EQ '') THEN BEGIN
      message,/info,'The $OPTICALS_PATH environment variable is not set'
      message,/info,'This routine depends on it.'
      message,/info,'Make sure the variable is defined before calling read_lnk or display_lnk'
      message,/info,'Setting it to the lnk_headers.xdr directory'
  ENDIF

  default,filename,shell_expand('$OPTICALS_PATH/lnk_headers.xdr')
  IF NOT file_test(filename,/read) THEN BEGIN
      filename = dialog_pickfile(FILTER='*.*',/MUST_EXIST)
  ENDIF

  pathsep = path_sep()
  IF (opticals_path EQ '') THEN BEGIN
      split = strsplit(filename,pathsep,/extract)
      nsplit = n_elements(split)

      lnk_headers_path = strjoin(split[0:nsplit-2],pathsep)

      ;; append a / if the original path had one
      IF strmid(filename,0,1) EQ pathsep THEN $
        lnk_headers_path=pathsep+lnk_headers_path

      setenv,'OPTICALS_PATH='+strtrim(lnk_headers_path,2)
  ENDIF
  
  ;; now read the file
  restore,filename

  sources = lnk_headers.source[sort(lnk_headers.source)]
  sources = ['**Any**',sources[uniq(sources)]]
  
;; now create the widget
  wbase = widget_base(title="read lnk", /column, map=0, mbar=barbase)
  
;;  WIDGET_CONTROL, /MANAGED, wBase
  
;; change the default font
;  if not is_gdl() then begin
;     widget_control, wbase, default_font="-adobe-helvetica-bold-r-normal-*-14-100-*-*-*-*-*"
;  endif

;; the menu structure
  wfilebutton = widget_button(barbase, value='File')
  whelpbutton = widget_button(barbase, value='Help')
  whelpbutton = widget_button(whelpbutton, value='Display help', uvalue='HELP')
  IF no_block EQ 0 THEN BEGIN
      wReadbutton = widget_button(wfilebutton, value='Read Selected', uvalue='READDATA',/separator)
  ENDIF 
  wquitbutton = widget_button(wfilebutton, value='Close', uvalue='CLOSE',/separator)

;; The complete layout for the widget is in a row
  wmainbase = widget_base(wbase, /column)
  
  wtopbase = widget_base(wmainbase, /row)

  wtopleftbase = widget_base(wtopbase, /column)
  wtoprightbase = widget_base(wtopbase)
  wtoptopleftbase = widget_base(wtopleftbase, /row)
  wbottomtopleftbase = widget_base(wtopleftbase,/column)

  IF plotwindow THEN BEGIN
      wbottombase = widget_base(wmainbase, /row,/frame)
  ENDIF ELSE BEGIN
      wbottombase = widget_base(wmainbase, /column,/frame)
  ENDELSE 
  
  wbottomleftbase = widget_base(wbottombase, /column)
  wbottomrightbase = widget_base(wbottombase,/column)

  ;;create a framed base to hold all the param buttons
  wParambase = widget_base(wtoptopleftbase,/column,/frame)
  
  wlabel = widget_label(wparambase, value='CONSTRAINTS: test')

  wlabel = widget_label(wparambase, value='Source of data')
  wsourcedrop = widget_droplist(wparambase, value=sources, uvalue='CONSTRAINT')

  wlabel = widget_label(wparambase, value='Material name')
  wmaterialtext = widget_text(wparambase, /editable, value='', uvalue='CONSTRAINT',/all_events)

  wlabel = widget_label(wparambase, value='Filename')
  wfilenametext = widget_text(wparambase, /editable, value='', uvalue='CONSTRAINT',/all_events)

  wlabel = widget_label(wparambase, value='Comment')
  wcommenttext = widget_text(wparambase, /editable, value='', uvalue='CONSTRAINT',/all_events)

  wlabel = widget_label(wparambase, value='Lambda 1')
  wlambda1text = widget_text(wparambase, /editable, value='', uvalue='CONSTRAINT')

  wlabel = widget_label(wparambase, value='Lambda 2')
  wlambda2text = widget_text(wparambase, /editable, value='', uvalue='CONSTRAINT')

  wMaterialList = widget_list(wtoptopleftbase, value=lnk_headers.filename,ysize=20, uvalue='LIST')

  geometry_wMaterialList = widget_info(wMaterialList,/geometry)
  geometry_wParamBase    = widget_info(wParamBase,/geometry)

  wlabel = widget_label(wbottomtopleftbase, value='Comments')
  wCommentlist = widget_list(wbottomtopleftbase, value=[''],ysize=10, $
                             scr_xsize=geometry_wMaterialList.scr_xsize+geometry_wParambase.scr_xsize, $
                             uvalue='COMMENTLIST')
  
  IF plotwindow THEN BEGIN
      geometry = widget_info(wtopleftbase,/geometry)
      wPlot = widget_draw(wtoprightbase,scr_ysize=geometry.scr_ysize,scr_xsize=geometry.scr_ysize)
  ENDIF
  
  geometry_wTopLeftBase    = widget_info(wTopLeftBase,/geometry)

  wFullPathLabel = widget_label(wbottomleftbase, value='Full path of the select data')
  wFullpathtext = widget_text(wbottomleftbase, value='',scr_xsize= geometry_wTopLeftBase.scr_xsize)

  geometry_wTopRightBase    = widget_info(wTopRightBase,/geometry)

  IF no_block EQ 0 THEN BEGIN
      wtemp1base =  widget_base(wbottomrightbase,/row)
      wtemp2base =  widget_base(wtemp1base,/column)

      wcommandlabel = widget_label(wtemp2base, value='Command to execute. %d will be replaced by data')
      wcommandtext = widget_text(wtemp2base, /editable, value='plq,%d', $
                                 uvalue='COMMAND',scr_xsize= geometry_wTopRightBase.scr_xsize)
      wReadbutton = widget_button(wtemp1base, value='Read Selected', uvalue='READDATA')
  ENDIF ELSE BEGIN
      wcommandlabel = widget_label(wbottomrightbase, value='Command to execute. %d will be replaced by data')
      wcommandtext = widget_text(wbottomrightbase, /editable, value='plq,%d', $
                                 uvalue='COMMAND',scr_xsize= geometry_wTopRightBase.scr_xsize)
  ENDELSE
      
  ;;  realize the widget hierarchy.
  widget_control, wbase, /realize
  
  IF plotwindow THEN BEGIN
      WIDGET_CONTROL, wPlot, GET_VALUE=PlotID
  ENDIF ELSE BEGIN
      PlotID = !D.window>0
  ENDELSE

  ;; create structure to get info back if used as a function
  IF no_block EQ 0 THEN BEGIN
      ;; create pointer to get the return data from the user after the
      ;; widget is closed and the sinfo structure is not available any longer.
      ptrtoformdata = ptr_new({cancel:1, SelectedIdx:0})
  ENDIF ELSE BEGIN
      ptrtoformdata = 0
  ENDELSE

  ;;  create the info structure to pass all info between routines
  sinfo = { $
            CurrentFilename : '' , $
            Lnk_Headers     : lnk_headers, $
            IdxDisplayed    : make_array(n_elements(lnk_headers.filename),value=1), $
            PlotID          : PlotID, $
            no_block        : no_block, $
            ptrToFormData   : ptrtoformdata, $
            wCommandText    : wCommandText, $
            wFilenameText   : wFilenameText, $
            wCommentText    : wCommentText, $
            wSourceDrop     : wSourceDrop, $
            Sources         : Sources, $
            wLambda1Text    : wLambda1Text, $
            wLambda2Text    : wLambda2Text, $
            wMaterialList   : wMaterialList, $
            wMaterialText   : wMaterialText, $
            wCommentList    : wCommentList, $
            wFullPathText   : wFullPathText, $
            dummy:'' $
          }
  
;; store the sinfo in the uvalue of the top widget
  widget_control, wbase, set_uvalue=sinfo, /no_copy

  ;;  map the top level base.
  widget_control, wbase, map=1
  
  ;; register with the big guy, xmanager!
  xmanager, "read_lnk", wbase, $
            event_handler="read_lnk_event", $
            no_block=no_block
  
  IF no_block EQ 0 THEN BEGIN
      ;; things to get back out of the widget
      formdata = *ptrtoformdata

      out = 0
      IF n_elements(formdata) EQ 0 THEN begin
          ptr_free, ptrtoformdata
      ENDIF ELSE BEGIN
          ;; Did the user cancel out of the form? If so, return a 0.
          IF formdata.cancel EQ 1 THEN BEGIN
              Ptr_Free, ptrToFormData
          ENDIF ELSE BEGIN
              Idx = formdata.Selectedidx
              Ptr_Free, ptrToFormData
              out = read_lnk_read_data(lnk_headers.filename[idx],prefix=lnk_headers.path[idx])
          ENDELSE
      ENDELSE
      return,out
  ENDIF
  
END                             ; of Read_Lnk
