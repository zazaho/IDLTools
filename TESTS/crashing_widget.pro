PRO non_crashing_widget_event, ev
  IF ev.SELECT THEN WIDGET_CONTROL, ev.TOP, /DESTROY
END
 
PRO non_crashing_widget
  base = WIDGET_BASE()
  button = WIDGET_BUTTON(base, value='Exit')
  WIDGET_CONTROL, base, /REALIZE
  XMANAGER,'crashing_widget',base
END

PRO crashing_widget_event, ev
  IF ev.SELECT THEN WIDGET_CONTROL, ev.TOP, /DESTROY
END
 
PRO crashing_widget
  base = WIDGET_BASE(MBAR=mbar)
  mbar = WIDGET_BUTTON(mbar,VALUE='File')
  bexit = WIDGET_BUTTON(mbar,VALUE='Exit')
  WIDGET_CONTROL, base, /REALIZE
  XMANAGER,'crashing_widget',base
END
