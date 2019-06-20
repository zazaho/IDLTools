FUNCTION ff,in,ord,offset=offset,method=method,scaling=scaling,_extra=_extra
  IF (n_params() EQ 1 ) THEN ord = 0d0
;;  (SH Sep 17 1999)
                                ; method select the following
                                ; -1 no flatfielding
                                ; 0 offset
                                ; 1 scaling 
;default method is scaling
  mt = 1
; is the method specified ?  
  IF (n_elements(method) eq 1) THEN mt = method
  
; give /offset to do offset, but offset=0 to do scaling
; these keywords override the method keyword  
  IF (n_elements(offset) GT 0) THEN IF (offset NE 0) THEN mt = 0
  IF (n_elements(scaling) GT 0) THEN IF (scaling NE 0) THEN mt = 1
  
  CASE mt OF
    -1:   return, in
    0:    return,sws_flatfield(in,ord,/clip,/offset,_extra=_extra)
    ELSE: return,sws_flatfield(in,ord,/scaling,/clip,_extra=_extra)
  ENDCASE
END

