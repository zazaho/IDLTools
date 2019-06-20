;; Simply widget to display a bit of text. Used to show the script and
;; also the help. First the event function
PRO query_mr_disp_text_evt, event
  
  WIDGET_CONTROL, event.top, GET_UVALUE=state
  CASE TAG_NAMES(event, /STRUCTURE_NAME) OF
      'WIDGET_BASE': BEGIN
          WIDGET_CONTROL, event.top, /MAP, /UPDATE
          RETURN
      END
      'WIDGET_KILL_REQUEST': retval = "EXIT"
      ELSE: WIDGET_CONTROL, event.id, GET_UVALUE = retval
  ENDCASE
  
  CASE retval OF
      "EXIT": BEGIN
          WIDGET_CONTROL, event.top, /DESTROY
          IF (WIDGET_INFO(state.ourGroup, /VALID)) THEN $
            WIDGET_CONTROL, state.ourGroup, /DESTROY
      END
      ELSE:
  ENDCASE
END

;; The actual routine to create the text display
PRO query_mr_disp_text, Text, Title=inTitle

  HEIGHT = 24 
  WIDTH = 80  
  IF keyword_set(inTitle) THEN BEGIN
      TITLE = 'query_mr:'+inTitle
  ENDIF ELSE BEGIN
      Title = 'query_mr'
  ENDELSE 

  ourGroup = 0L
  
  filebase = WIDGET_BASE(TITLE = TITLE, $
                         /TLB_KILL_REQUEST_EVENTS, /TLB_SIZE_EVENTS, $
                         /BASE_ALIGN_LEFT, /COLUMN, MBAR=menu_bar, $
                         GROUP_LEADER = GROUP)
  
  IF (!VERSION.OS_FAMILY EQ 'Windows') THEN extra = '&' ELSE extra = ''
  menu_bar = WIDGET_BUTTON(menu_bar, VALUE=extra+'File', /MENU)
  

; Default Done button name:
  done_button = "Quit"
  
  filequit = WIDGET_BUTTON(menu_bar, $ ;create a Done Button
                           SEPARATOR=editable, $
                           VALUE = extra+done_button, UVALUE = "EXIT")
  
  filetext = WIDGET_TEXT(filebase, $ ;create a text widget
                         XSIZE = WIDTH, $ ;to display the file's
                         YSIZE = HEIGHT, $ ;contents
                         UVALUE='TEXT', $
                         /SCROLL, FONT = font, $
                         VALUE = Text)
  
  WIDGET_CONTROL, filebase, /REALIZE ;instantiate the widget
  
  state={ourGroup:ourGroup}
  WIDGET_CONTROL, filebase, SET_UVALUE = state
  
  
  Xmanager, "query_mr_disp_text", $ ;register it with the
            filebase, $         ;widget manager
            GROUP_LEADER = GROUP, $
            EVENT_HANDLER = "query_mr_disp_text_evt", /NO_BLOCK
  
END

;; Make the changes to the original structure permanent
PRO query_mr_apply_changes
  
  COMMON COMMON_query_mr,sInfo,struct,sColInfo
  
  struct.name = sColInfo.Name
  struct.unit = sColInfo.Unit
  struct.description = sColInfo.Desc
  ;; Did some columns gat deleted than do the actual removal
  IF n_elements(sColInfo.ActiveIdx) NE n_elements(struct.Name) THEN BEGIN
      struct = select_col_mr(struct,sColInfo.ActiveIdx)
  ENDIF
  
  sColInfo = { $
             NAME:STRUCT.NAME, $
             UNIT:STRUCT.UNIT, $
             DESC:STRUCT.DESCRIPTION, $
             ACTIVEIDX:indgen(n_elements(struct.name)) $
             }
END      


;--------------------------------------------------------------------
; Stupid routine to select the first item for the first display of the widget
PRO query_mr_index_zero, wHandle
  query_mr_set_list_index, 0
END      

;--------------------------------------------------------------------
;
PRO query_mr_show_sample, wHandle
  COMMON COMMON_query_mr,sInfo,struct,sColInfo
  data = struct.data.(sColInfo.ActiveIdx[sInfo.CurrentIndex])
  WIDGET_CONTROL, wHandle, SET_VALUE=string(data[0:(n_elements(data)-1) < 100])
END      

;--------------------------------------------------------------------
;
PRO query_mr_set_list_index, Index
  
  COMMON COMMON_query_mr,sInfo,struct,sColInfo

  ;; Put the value of 'event.index' into the variable
  ;; 'CurrentIndex':
  sInfo.CurrentIndex = Index
  
  WIDGET_CONTROL, sInfo.wList, SET_VALUE='Col'+ $
                  strtrim(sindgen(n_elements(sColInfo.Name)),2)+': - '+sColInfo.Name
  
  ;; Make sure that the right elements is selected
  WIDGET_CONTROL, sInfo.wList, SET_LIST_SELECT=Index
  
  ;; And Update the edit displays
  WIDGET_CONTROL, sInfo.wNameText, SET_VALUE=sColInfo.Name[Index]
  WIDGET_CONTROL, sInfo.wUnitText, SET_VALUE=sColInfo.Unit[Index]
  WIDGET_CONTROL, sInfo.wDescText, SET_VALUE=sColInfo.Desc[Index]
  
  ;; Also show the according sample data
  query_mr_show_sample, sInfo.wSample
END      

;--------------------------------------------------------------------
;
PRO query_mr_store_edit_data
  COMMON COMMON_query_mr,sInfo,struct,sColInfo

  ;; Get the current values from the edit boxes
  WIDGET_CONTROL, sInfo.wNameText, GET_VALUE=Name
  WIDGET_CONTROL, sInfo.wUnitText, GET_VALUE=Unit
  WIDGET_CONTROL, sInfo.wDescText, GET_VALUE=Desc
  
  ;; Save the data in the appropriate arrays
  sColInfo.Name[sInfo.CurrentIndex] = Name
  sColInfo.Unit[sInfo.CurrentIndex] = Unit
  sColInfo.Desc[sInfo.CurrentIndex] = Desc
  
END      

PRO query_mr_Event, sEvent
  
  COMMON COMMON_query_mr,sInfo,struct,sColInfo

  ;; Which event
  WIDGET_CONTROL, sEvent.id, GET_UVALUE=eventUValue
  CASE eventUValue OF
      'LIST': BEGIN
          ;; Remember what was entered
          query_mr_store_edit_data 
          
          ;; Update the display with the new selected column
          query_mr_set_list_index, sEvent.Index
      END
      
      'CANCEL' : BEGIN
          sInfo.Apply = 0
          WIDGET_CONTROL, sEvent.top, /DESTROY
      END             
      
      'APPLY' : BEGIN
          ;; Remember what was entered
          query_mr_store_edit_data 
          
          sInfo.Apply = 1
          WIDGET_CONTROL, sEvent.top, /DESTROY
      END             
      
      'REVERT' : BEGIN
          
          ;; Put the original data back
          sColInfo = { $
                     NAME:STRUCT.NAME, $
                     UNIT:STRUCT.UNIT, $
                     DESC:STRUCT.DESCRIPTION, $
                     ACTIVEIDX:indgen(n_elements(struct.name)) $
                     }

          ;; Update the display with the new selected column
          query_mr_set_list_index, 0
          
      END             

      'OPEN' : BEGIN
          file = read_fmr()
          IF is_mr(file) THEN BEGIN
              struct = file

              sColInfo = { $
                         NAME:STRUCT.NAME, $
                         UNIT:STRUCT.UNIT, $
                         DESC:STRUCT.DESCRIPTION, $
                         ACTIVEIDX:indgen(n_elements(struct.name)) $
                         }
              
;; Update the display with the new selected column
              query_mr_set_list_index, 0
          ENDIF
      END                       ;       of  OPEN
      
      'DELETE' : BEGIN
          ;; Simply remove the selected columns from the display
          ;; by removing it from the ActiveIdx
          nactive = n_elements(sColInfo.ActiveIdx)
          
          ;; Do not remove the column if none will be left
          IF nactive GT 1 THEN BEGIN
              remainidx = where(indgen(nactive) NE sInfo.CurrentIndex)
              
              sColInfo = { $
                         Name:SColInfo.Name[remainidx], $
                         UNIT:sColInfo.Unit[remainidx], $
                         DESC:SColInfo.Desc[remainidx], $
                         Activeidx:sColInfo.ActiveIdx[remainidx] $
                         }
              query_mr_set_list_index, sInfo.CurrentIndex < (nactive-2)
          ENDIF
      END 
      
      'SAVE' : BEGIN

          query_mr_store_edit_data 
          outfile = dialog_pickfile(/write,filter='*.dat')
          
          IF outfile NE '' THEN BEGIN
              query_mr_apply_changes
              write_fmr,struct,file=outfile
          ENDIF
      END     
      
      ;; this is to get a list of commands that will preform the
      ;; same commands as done now in the interactive session from
      ;; a non interactive session
      'SCRIPT' : BEGIN
          ;; Make sure any modifications in the edit boxes are kept
          query_mr_store_edit_data 

          ;; Read the required structure name
          WIDGET_CONTROL, sInfo.wScriptText, GET_VALUE=sname
          sname = strtrim(sname,2)
          IF sname EQ '' THEN sname = 'struct'

          scripttxt = ''
          
          ;; First figure out what has changed.
          newname = sColInfo.name
          newunit = sColInfo.unit
          newdesc = sColInfo.desc

          oldname = struct.name[sColInfo.ActiveIdx]
          oldunit = struct.unit[sColInfo.ActiveIdx]
          olddesc = struct.description[sColInfo.ActiveIdx]
          
          ;; We do everything by col_name which should make it more
          ;; robust
          
          ;; First select only those columns that are left
          ;; Did some columns gat deleted than do the actual removal

          IF n_elements(sColInfo.ActiveIdx) NE n_elements(struct.Name) THEN BEGIN
              ;; do this with the old column names before renaming
              colstring = '["'+oldname[0]+'"'
              IF n_elements(sColInfo.ActiveIdx) GT 1 THEN BEGIN
                  FOR i =1,n_elements(sColInfo.ActiveIdx)-1 DO BEGIN
                      colstring = colstring+',"'+oldname[i]+'"'
                  ENDFOR
              ENDIF
              colstring = colstring+']'
              scripttxt = [scripttxt,sname+'=select_col_mr('+sname+','+colstring+')']
          ENDIF

          ;; Now see if any names should be changed
          changedidx = where(newname NE oldname,cnt)
          IF cnt NE 0 THEN BEGIN
              ;; do this with the old column names before renaming
              oldstring = '['+strtrim(string(changedidx[0]),2)
              newstring = '["'+newname[changedidx[0]]+'"'
              IF cnt GT 1 THEN BEGIN
                  FOR i =1,cnt-1 DO BEGIN
                      oldstring = oldstring+','+strtrim(string(changedidx[i]),2)
                      newstring = newstring+',"'+newname[changedidx[i]]+'"'
                  ENDFOR
              ENDIF
              oldstring = oldstring+']'
              newstring = newstring+']'
              scripttxt = [scripttxt,'rename_col_mr,'+sname+',oldname='+oldstring+',newname='+newstring]
          ENDIF

          ;; Now see if any units should be changed
          changedidx = where(newunit NE oldunit,cnt)
          IF cnt NE 0 THEN BEGIN
              ;; do this with the old column units before renaming
              oldstring = '['+strtrim(string(changedidx[0]),2)
              newstring = '["'+newunit[changedidx[0]]+'"'
              IF cnt GT 1 THEN BEGIN
                  FOR i =1,cnt-1 DO BEGIN
                      oldstring = oldstring+','+strtrim(string(changedidx[i]),2)
                      newstring = newstring+',"'+newunit[changedidx[i]]+'"'
                  ENDFOR
              ENDIF
              oldstring = oldstring+']'
              newstring = newstring+']'
              scripttxt = [scripttxt,'rename_col_mr,'+sname+',oldunit='+oldstring+',newunit='+newstring]
          ENDIF

          ;; Now see if any descs should be changed
          changedidx = where(newdesc NE olddesc,cnt)
          IF cnt NE 0 THEN BEGIN
              ;; do this with the old column descs before renaming
              oldstring = '['+strtrim(string(changedidx[0]),2)
              newstring = '["'+newdesc[changedidx[0]]+'"'
              IF cnt GT 1 THEN BEGIN
                  FOR i =1,cnt-1 DO BEGIN
                      oldstring = oldstring+','+strtrim(string(changedidx[i]),2)
                      newstring = newstring+',"'+newdesc[changedidx[i]]+'"'
                  ENDFOR
              ENDIF
              oldstring = oldstring+']'
              newstring = newstring+']'
              scripttxt = [scripttxt,'rename_col_mr,'+sname+',olddesc='+oldstring+',newdesc='+newstring]
          ENDIF
          IF n_elements(scripttxt) GT 1 THEN begin
              query_mr_disp_text,scripttxt[1:*],title='script'
          ENDIF
      END

      'HELP' : BEGIN
          htxt = "Manage Machine readable structures."
          htxt = [htxt,"This Tools display the tags that are present in a machine readable"]
          htxt = [htxt,"data table and allows you to rename columns or change units and"]
          htxt = [htxt,"descriptions. Besides this is can remove unwanted columns. There is"]
          htxt = [htxt,"limited support is available for casting multi dimensional arrays and"]
          htxt = [htxt,"other structures into MR tables."]
          htxt = [htxt,""]
          htxt = [htxt,"Usage instructions:"]
          htxt = [htxt,"restructured = query_mr(struct)"]
          htxt = [htxt,""]
          htxt = [htxt,"where struct is a machine readable table or a structure with equal"]
          htxt = [htxt,"length tags (e.g.: struct = {wavelength:indgen(10),flux:dindgen(10)}) or a multi"]
          htxt = [htxt,"dimensional array (e.g.: struct = make_array(3,100,value=0.0))"]
          htxt = [htxt,""]
          htxt = [htxt,"You will see a window that represents the structure. On the left"]
          htxt = [htxt,"are the columns that are available. You can select a different column"]
          htxt = [htxt,"with the mouse or the up/down keys. In the middle panel you see the name,"]
          htxt = [htxt,"unit and description of the selected column. You can change these by"]
          htxt = [htxt,"typing. The column on the right shows you a sample of the data that"]
          htxt = [htxt,"are present in this column. This is particularly useful for columns"]
          htxt = [htxt,"that lack a descriptive name."]
          htxt = [htxt,""]
          htxt = [htxt,"The three buttons at the bottom allow you to (from left to right):"]
          htxt = [htxt,"   Quit without applying the changes"]
          htxt = [htxt,"   Undo any changes you have made and start over."]
          htxt = [htxt,"   Quit after applying any changes"]
          htxt = [htxt,""]
          htxt = [htxt,"In the menu you can choose to open a new file. Note that your current"]
          htxt = [htxt,"structure and any possible changes will be lost if you open a new"]
          htxt = [htxt,"file. Or save the current structure to a file on disk"]
          htxt = [htxt,""]
          htxt = [htxt,"There is also an option to display a script that will yield the same."]
          htxt = [htxt,"result at the IDL prompt as the interactively obtained restructuring"]
          query_mr_disp_text,htxt,title='Help'
      END
      
      ELSE:                     ;  do nothing
  ENDCASE
END

;;;;


PRO query_mr_apply_changes
  
  COMMON COMMON_query_mr,sInfo,struct,sColInfo
  
  struct.name = sColInfo.Name
  struct.unit = sColInfo.Unit
  struct.description = sColInfo.Desc
  ;; Did some columns gat deleted than do the actual removal
  IF n_elements(sColInfo.ActiveIdx) NE n_elements(struct.Name) THEN BEGIN
      struct = select_col_mr(struct,sColInfo.ActiveIdx)
  ENDIF
   
  sColInfo = { $
             NAME:STRUCT.NAME, $
             UNIT:STRUCT.UNIT, $
             DESC:STRUCT.DESCRIPTION, $
             ACTIVEIDX:indgen(n_elements(struct.name)) $
             }
END      
          
FUNCTION query_mr,instruct,help=help
  
  COMMON COMMON_query_mr,sInfo,struct,sColInfo

  ;; This is a routine to query mr structures in a graphical
  ;; environment. It's main aim is to have an interactive tool
  ;; to select rows from a table

  ;; Protect the input from being changed
  IF NOT keyword_set(instruct) THEN BEGIN
      file = read_fmr()
      IF is_mr(file) THEN BEGIN
          struct = file
      ENDIF ELSE BEGIN
          message,'This routine requires an machine readable data structure as input.'
          return,0
      ENDELSE
  ENDIF ELSE BEGIN
      struct = instruct
  ENDELSE
      
  IF NOT is_mr(struct) THEN BEGIN

      ;; These are the only allowed type on the rest skip the column
      allowed = ['BYTE','INT','UINT','LONG','ULONG','FLOAT','DOUBLE','STRING']
      ;; Convert the above types to:
      convtype = ['INT','INT','INT','LONG','LONG','DOUBLE','DOUBLE','STRING']
      
      IF size(struct,/tname) EQ 'STRUCTURE' THEN BEGIN
          tagnames = tag_names(struct)
          
          ;; Use the following to remember somethings about the useful columns
          valid = [-1]
          names = [""]
          lngth = [0]
          types = [0]
          
          FOR i=0,n_elements(tagnames) DO BEGIN
              coltype = size(struct.(i),/tname)
              IF ( total(coltype EQ allowed) NE 0) THEN BEGIN
                  valid = [valid,i]
                  names   = [names,tagnames[i]]
                  lngth = [lngth,n_elements(struct.(i))]
                  types = [types,convtype[where(coltype EQ allowed)]]
              ENDIF ELSE BEGIN
                  message,'The input structure has a column with incompatible data type: '+ $
                          tagnames[i],/informational
              ENDELSE
          ENDFOR
          
          IF n_elements(valid) GT 1 THEN BEGIN
              valid = valid[1:*]
              names = names[1:*]
              lngth = lngth[1:*]
              types = types[1:*]
          ENDIF ELSE BEGIN
              message,'The input structure has no columns with compatible data.',/error
              return,0
          ENDELSE
          
          ;; Sanity check
          IF total( lngth EQ lngth[0]) NE n_elements(lngth) THEN BEGIN
              message,'The input structure has a data columns of different lengths.',/error
              return,0
          ENDIF
          
          tmp = make_mr(lngth[0],fields=types,names=names)
          ;; Fill the structure with the accepted data columns
          FOR i =0,n_elements(valid)-1 DO tmp.data.(i) = struct.(valid[i])

          struct = tmp
          
      ENDIF ELSE BEGIN
          ;; So it is not a structure perhaps it is a simple array or matrix of values
          coltype = size(struct,/tname)
          IF (total(coltype EQ allowed) NE 0) THEN BEGIN
              CASE size(struct,/n_dimensions) OF
                  1: BEGIN
                      ncols = 1
                      nrows = n_elements(struct[*])
                  END
                  
                  2: BEGIN
                      ;; assume that the first index gives the columns
                      ;; and the second the rows
                      ncols = n_elements(struct[*,0])
                      nrows = n_elements(struct[0,*])
                      ;; Now do a rough check if the number of columns
                      ;; is much larger than the number of row
                      ;; probably the rows and columns are reversed
                      IF ncols/nrows GT 10 THEN BEGIN
                          message,'The input array (struct[cols,rows]) has many more columns than rows.',/info
                          message,'Assuming that they should be inversed.',/info
                          struct = transpose(struct)
                          ncols = n_elements(struct[*,0])
                          nrows = n_elements(struct[0,*])
                      ENDIF
                  END
                  
                  ELSE: BEGIN
                      message,'The input structure has a structure that cannot be cast into an mr structure.',/error
                      return,0
                  END
                  
              ENDCASE
              
              types = make_array(ncols,value=convtype[where(coltype EQ allowed)])
              
              tmp = make_mr(nrows,fields=types)
              ;; Fill the structure with the data
              IF ncols GT 1 THEN BEGIN
                  FOR i =0,ncols-1 DO tmp.data.(i) = reform(struct[i,*],nrows)
              ENDIF ELSE BEGIN
                  tmp.data.(0) = struct[*]
              ENDELSE
              
              struct = tmp
              
          ENDIF ELSE BEGIN
              message,'The input array has a data that type cannot be cast into an mr structure.'
              return,0
          ENDELSE
      ENDELSE
  ENDIF
  

;; So here we want to do some basic administration so that we them
;; available at all times. 
;; The main things are the column names, types


;;; Now create the widget to have the user define things

;; Now create the widget
  wTopBase = WIDGET_BASE(TITLE="Manage MR", /COLUMN, MAP=0, MBAR=barBase)
  
;; The menu structure
  wFileButton = WIDGET_BUTTON(barBase, VALUE='File')
  wHelpButton = WIDGET_BUTTON(barBase, VALUE='Help')
  wHelpButton = WIDGET_BUTTON(wHelpButton, VALUE='Display Help', UVALUE='HELP')
  wOpenButton = WIDGET_BUTTON(wFileButton, VALUE='Open file', UVALUE='OPEN')
  wSaveButton = WIDGET_BUTTON(wFileButton, VALUE='Save to file', UVALUE='SAVE')
  wQuitButton = WIDGET_BUTTON(wFileButton, VALUE='Cancel and Close', UVALUE='CANCEL',/separator)
  wQuitButton = WIDGET_BUTTON(wFileButton, VALUE='Apply and Close', UVALUE='APPLY')


  ;;  Create the left, center and right bases
  ;;
  wTopRowBase = WIDGET_BASE(wTopBase, COLUMN=3)
  
  wLeftBase = WIDGET_BASE(wTopRowBase, /COLUMN, /FRAME,XSIZE=300)

  wNameLabel = WIDGET_LABEL(wLeftBase, VALUE='Available Data Columns:')
  
  wList = WIDGET_LIST(wLeftBase, /FRAME, NOTIFY_REALIZE='query_mr_index_zero', $
                          VALUE='Col'+ $
                          strtrim(sindgen(n_elements(struct.name)),2)+': - '+ $
                          struct.name ,YSIZE=12, UVALUE='LIST')
  
  wDeleteButton = WIDGET_BUTTON(wLeftBase, UVALUE='DELETE', VALUE='Delete Column')

  wCenterBase = WIDGET_BASE(wTopRowBase, /COLUMN, /FRAME,XSIZE=300)

  wScriptBase = WIDGET_BASE(wCenterBase, /ROW,/FRAME)
  wScriptLabel = WIDGET_LABEL(wScriptBase, VALUE='Structure:')
  wScriptText  = WIDGET_TEXT( wScriptBase, /EDITABLE, ALL_EVENTS=0, VALUE='struct',XSIZE=16)
  wScriptButton = WIDGET_BUTTON(wScriptBase, VALUE='Generate Script', UVALUE='SCRIPT',/separator, $
                              tooltip='Generate a script to obtain the same result non-interactively')

  wNameLabel = WIDGET_LABEL(wCenterBase, VALUE='Column Info:')
  wNameLabel = WIDGET_LABEL(wCenterBase, VALUE='Name:')
  wNameText  = WIDGET_TEXT( wCenterBase, /EDITABLE, ALL_EVENTS=0, UVALUE='EDIT')
  wUnitLabel = WIDGET_LABEL(wCenterBase, VALUE='Unit:')
  wUnitText  = WIDGET_TEXT( wCenterBase, /EDITABLE, ALL_EVENTS=0, UVALUE='EDIT')
  wDescLabel = WIDGET_LABEL(wCenterBase, VALUE='Description:')
  wDescText  = WIDGET_TEXT( wCenterBase, /EDITABLE, ALL_EVENTS=0 , UVALUE='EDIT')

  wButtonBase = WIDGET_BASE(wCenterBase, /ROW)

  wCancelButton = WIDGET_BUTTON(wButtonBase, UVALUE='CANCEL', VALUE='&Cancel and Close')
  wRevertButton = WIDGET_BUTTON(wButtonBase, UVALUE='REVERT', VALUE='Revert', $
                              tooltip='Undo and changes and start over')
  wOkButton = WIDGET_BUTTON(wButtonBase, UVALUE='APPLY', VALUE='Apply and Close')

  wRightBase = WIDGET_BASE(wTopRowBase, /COLUMN, /FRAME,XSIZE=300)
  wNameLabel = WIDGET_LABEL(wRightBase, VALUE='Data sample:')
  wSample = WIDGET_LIST(wRightBase, /FRAME, NOTIFY_REALIZE='query_mr_show_sample', $
                        YSIZE=14, UVALUE='IGNORE')
  

  sColInfo = { $
             NAME:STRUCT.NAME, $
             UNIT:STRUCT.UNIT, $
             DESC:STRUCT.DESCRIPTION, $
             ACTIVEIDX:indgen(n_elements(struct.name)) $
             }
  
  ;;  Create the info structure to pass all info between routines
  sInfo = { $
          wNameText: wNameText  , $
          wUnitText: wUnitText  , $
          wDescText: wDescText  , $
          wScriptText: wScriptText  , $
          wList: wList, $
          wSample: wSample, $
          CurrentIndex:0, $
          Apply:0 $
          }
        
  ;;  Realize the widget hierarchy.
  WIDGET_CONTROL, wTopBase, /REALIZE
  
  ;;  Map the top level base.
  ;;
  WIDGET_CONTROL, wTopBase, MAP=1
  
  ;; Register with the BIG GUY, XMANAGER!
  ;;
  XMANAGER, "query_mr_", wTopBase, $
            EVENT_HANDLER="query_mr_Event"
  
  ;; Now did we apply or not
  IF sInfo.Apply EQ 1 THEN BEGIN
      query_mr_apply_changes
  ENDIF
  
  return,struct
  
END   ; of Query_Mr
