FUNCTION object,a,set_object=set,get_object=get
  
  head= a.header

  CASE n_elements(head) OF
     0: BEGIN
        return,''
     END
     1: BEGIN 
        header = head
     END
     ELSE: BEGIN
        header = head[0]
        FOR i = 1, n_elements(head)-1 DO BEGIN
           header = header+' '+head[i]
        ENDFOR
     END
  ENDCASE

  IF NOT keyword_set(set) THEN BEGIN
    ;; We take the line from the header containing the object word
    ;; It looks like OBJECT  = 'SAO 96709'          /
    p1 = strpos(header,"OBJECT")
    ;; End of the line
    p2 = strpos(header,"/",p1)
    object = strmid(header,p1,p2-p1)
    p1 = strpos(object,"'")
    p2 = strpos(object,"'",p1+1)
    object= strtrim(strmid(object,p1+1,p2-p1-1),2)
    get = object
    return,object
  ENDIF ELSE BEGIN
    header = srf_write_fits_key( header, 'OBJECT', set ,'S','',sts)
    b = a
    b.header = header
    return,b
  ENDELSE
  
END
