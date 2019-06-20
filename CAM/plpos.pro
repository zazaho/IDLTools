PRO plpos,file=file, examine=examine,name=name,type=type,_extra=_extra

  default,file,'~/IA_TOOL/CAM/briceno.sav'
  ;; we expect a .sav file with .ra and .dec (in degrees) .name 
  ;; in variable called objects
  restore,file
  
  pl,objects.ra,objects.dec,/opl,psym=1,_extra=_extra
  
  IF keyword_set(name) THEN BEGIN
  xyouts,objects.ra,objects.dec,objects.name,_extra=_extra,noclip=0
  ENDIF ELSE BEGIN 
      IF keyword_set(type) THEN BEGIN
          xyouts,objects.ra,objects.dec,objects.type,_extra=_extra,noclip=0
      ENDIF 
  ENDELSE 
   
  IF keyword_set(examine) THEN BEGIN
      IF !d.name EQ 'X' THEN BEGIN
          ;; let user select a point and display the closest data from
          ;; the catalogue
          print,'select a source with the 1st mouse button'
          print,'3rd button quits'
          device,cursor_standard=2
          !err = 0
          Cursor,x,y,2,/down
          while !err ne 4 DO begin
              !err = 0
              print,x,y
              ;; find the closest match
              dist = sqrt((objects.ra-x)^2d0+(objects.dec-y)^2d0)
              IF ((where(keep EQ 0))[0] NE -1) THEN $
                dist[where(keep EQ 0)]=max(dist)
              min_dist = min(dist,idx_match)
              print,'=================================================='
              print,'The point you selected is at:'
              print,'RA:',ra2s(x),' DEC:',dec2s(y)
              print,'The closest source to the point you selected is at:'
              print,'RA:',objects.ra[idx_match],' DEC:',object.dec[idx_match]
              print,'RA:',ra2s(object.ra[idx_match]),' DEC:',dec2s(object.dec[idx_match])
              print,min_dist*3600,' arcsec away from the selected point'
              print,'=================================================='

              cursor,x,y,2,/down
          EndWhile
          device,/cursor_crosshair
      ENDIF 
  ENDIF 
END 

