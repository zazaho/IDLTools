;; Simply widget to display a bit of text. Used to show the script and
;; also the help. First the event functino
PRO display_text_widget_evt, event
  
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
PRO display_text_widget, Text, Title=inTitle

  HEIGHT = 24 
  WIDTH = 80  
  IF keyword_set(inTitle) THEN BEGIN
      Title = inTitle
  ENDIF ELSE BEGIN
      Title = ''
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
  
  
  Xmanager, "display_text_widget", $ ;register it with the
            filebase, $         ;widget manager
            GROUP_LEADER = GROUP, $
            EVENT_HANDLER = "display_text_widget_evt", /NO_BLOCK
  
END
