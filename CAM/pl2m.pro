PRO pl2m,file=file, Jmin=Jmin,Jmax=Jmax,Hmin=Hmin,Hmax=Hmax,Kmin=Kmin, $
         Kmax=Kmax,Bmin=Bmin,Bmax=Bmax,Rmin=Rmin,Rmax=Rmax,max_dist=maxdist, $
         examine=examine,sed=sed,extended =extended,_extra=_extra

  default,file,'${HOME}/IA_TOOL/CAM/2mass.sav'
  default,Jmin,0
  default,Jmax,100
  default,Hmin,0
  default,Hmax,100
  default,Kmin,0
  default,Kmax,100
  default,Bmin,0
  default,Bmax,100
  default,Rmin,0
  default,Rmax,100
  default,max_dist,100
  default,sed,0

  IF keyword_set(extended) THEN BEGIN
      restore,'~/IA_TOOL/CAM/twomass_ext.sav'
      T = twomass
      keep = (T.J GE Jmin) AND (T.J LE Jmax) $
        AND (T.H GE Hmin) AND (T.H LE Hmax) $
        AND (T.K GE Kmin)
  ENDIF ELSE BEGIN 
      restore,file
      T = twomass
      
      keep = (T.J GE Jmin) AND (T.J LE Jmax) $
        AND (T.H GE Hmin) AND (T.H LE Hmax) $
        AND (T.K GE Kmin) AND (T.K LE Kmax) $
        AND (T.B GE Bmin) AND (T.B LE Bmax) $
        AND (T.R GE Rmin) AND (T.R LE Rmax) $
        AND (T.opt_distance LE max_dist)
  ENDELSE 

  idx = where(keep EQ 1)
  pl,T.ra[idx],T.dec[idx],/opl,psym=1,_extra=_extra
  
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
              dist = sqrt((T.ra-x)^2d0+(T.dec-y)^2d0)
              IF ((where(keep EQ 0))[0] NE -1) THEN $
                dist[where(keep EQ 0)]=max(dist)
              min_dist = min(dist,idx_match)
              print,'=================================================='
              print,'The closest source to the point you selected is at:'
              print,'RA:',T.ra[idx_match],' DEC:',T.dec[idx_match]
              print,'RA:',ra2s(T.ra[idx_match]),' DEC:',dec2s(T.dec[idx_match])
              print,min_dist*3600,' arcsec away from the selected point'
              IF NOT keyword_set(extended) THEN BEGIN 
                  IF T.B[idx_match] LT 99 THEN print,'B Magnitude:',T.B[idx_match]
                  IF T.R[idx_match] LT 99 THEN print,'R Magnitude:',T.R[idx_match]
              ENDIF  
              IF T.J[idx_match] LT 99 THEN print,'J Magnitude:',T.J[idx_match]
              IF T.H[idx_match] LT 99 THEN print,'H Magnitude:',T.H[idx_match]
              IF T.K[idx_match] LT 99 THEN print,'K Magnitude:',T.K[idx_match]
              print,'=================================================='
              IF sed THEN BEGIN
                  flx = mag2jy(B=T.B[idx_match],R=T.R[idx_match], $
                               J=T.J[idx_match],H=T.H[idx_match], $
                               K=T.K[idx_match],wave=wave)
                  w = !d.window
                  window,31
                  print,flx
                  pl,wave,flx,ps=1, $
                    xr=[0.4,20],/iras,/xl,ym=0.5d-4
                  wset,w
              ENDIF 
              cursor,x,y,2,/down
          EndWhile
          device,/cursor_crosshair
      ENDIF 
  ENDIF 
END 

