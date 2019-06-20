FUNCTION get_keywordfromextra,name,_extra=_extra
  IF n_elements(_extra) NE 0 THEN BEGIN
      vars = tag_names(_extra)
      idx = where(strcmp(vars, name,strlen(name),/fold_case) EQ 1)
      IF idx[0] NE -1 THEN BEGIN
          print,idx[0]
          return,_extra.(idx[0])
      ENDIF
  ENDIF 
END 
