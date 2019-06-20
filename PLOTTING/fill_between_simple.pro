;+
; ------------------------------------------------------
;FILL_BETWEEN_SIMPLE: simple program to fill an area between to curves
;usage:
;fill_between_simple,x1,y1,x2,y2[,xrange=xrange,help=help]
;   x1,x2        -  x coordinates of the two curves
;   y1,y2        -  y coordinates of the two curves
;options:
;   xrange=[xx,xy]-  only show the points falling in this range
;   help         -  prints this message
; ------------------------------------------------------
;-
PRO fill_between_simple,x1_in,y1_in,x2_in,y2_in, $
                        xrange=xrange,help=help, $
                        _extra=_extra

  if keyword_set(help) then begin
     doc_library,'fill_between_simple'
     return
  endif

  ;; checks
  if n_elements(x1_in) eq 0 then message,/error,'x1 does not contain valid points'
  if n_elements(x2_in) eq 0 then message,/error,'x2 does not contain valid points'
  if n_elements(y1_in) ne n_elements(x1_in) then message,/error,'y1 does not contain the same number of points as x1'
  if n_elements(y2_in) ne n_elements(x2_in) then message,/error,'y2 does not contain the same number of points as x2'

  ;; sort the two arrays to get nice looking polygons
  y1 = y1_in[sort(x1_in)]
  x1 = x1_in[sort(x1_in)]
  y2 = y2_in[sort(x2_in)]
  x2 = x2_in[sort(x2_in)]
 
  ;; do we want a subrange to be plotted
  IF (n_elements(xrange) EQ 2) THEN BEGIN
     xmin = min(xrange,max=xmax)
     idx1 = where((x1 ge xmin) and (x1 le xmax), count)
     if count ne 0 then begin
        x1 = x1[idx1] 
        y1 = y1[idx1] 
     endif else message,/info,'no points x1 in given interval using full range'

     idx2 = where((x2 ge xmin) and (x2 le xmax), count)
     if count ne 0 then begin
        x2 = x2[idx2] 
        y2 = y2[idx2] 
     endif else message,/info,'no points x2 in given interval using full range'
  ENDIF

  ;; make the entire polygon to be filled
  allx = [x1,reverse(x2)]
  ally = [y1,reverse(y2)]
  
  polyfill,allx,ally,_extra=_extra
END
