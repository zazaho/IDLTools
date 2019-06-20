FUNCTION sh_select,in,cond
  
  CASE (n_params()) OF 
    0: BEGIN
      print,'supply aar and testing condition'
      return,0
    END
    1: BEGIN
      print,'Warning: no test supplied all data returned'
      return,in
    END
    ELSE: BEGIN
      idx = where(cond)
      IF (idx[0] NE -1) THEN BEGIN                   
        aar= {type   : 'SAAR'        ,$ ; structure type
              history: in.history     ,$ ; the given history
              header : in.header        ,$ ; the header string
              data   : in.data[idx] }   ; the given data  
        return,aar
      ENDIF ELSE BEGIN
        print,'SH_SELECT:Warning: No select data'
        return,0
      ENDELSE
    END 
  END
END
