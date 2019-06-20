PRO sh_tv,inimg,out=out,examine=examine,zoom=zoom,_extra=_extra

  img = inimg
  IF keyword_set(zoom) THEN BEGIN

      xs = n_elements(img[*,0])
      ys = n_elements(img[0,*])

      ;; Nice big window 800x800
      xfact = floor(alog10(800./xs)/alog10(2))
      yfact = floor(alog10(800./ys)/alog10(2))
      ;; The right factor is 2^(the smallest value)
      winfact = 2d0^(xfact<yfact)
      xsw = xs*winfact
      ysw = ys*winfact
      window,!window,xsize=xsw,ysize=ysw
      
      ;; to draw a cirlce
      theta = 2*!dpi*dindgen(1001)/1d3
      xcirc = cos(theta)
      ycirc = sin(theta)
      radii = floor((dindgen(8)+1d0)/8d0*(xsw<ysw)/4d0)

      loop0:
      ;;init before the loop
      zoomfact=1
      minx=0
      miny=0
      numx = xs
      numy = ys
      tvscl,congrid(img,xsw,ysw,/interp),_extra=_extra

      loop1:
      print,'btn1=zoom,btn2=circles,btn3=exit'
      cursor,x,y,/down,/device
      ;; First we determine the position of x and y in the
      ;; image coordinates
      ximg = minx+double(x*numx)/double(xsw)
      yimg = miny+double(y*numy)/double(ysw)
      case !mouse.button OF
          1: BEGIN
              zoomfact=zoomfact*2
              numx = floor(xs/zoomfact)
              numy = floor(ys/zoomfact)
              minx = floor((0>(ximg-numx/2))<xs)
              miny = floor((0>(yimg-numy/2))<ys)
              zoom = congrid(img[minx:minx+numx-1,miny:miny+numy-1],xsw,ysw,/interp)
              tvscl,zoom,_extra=_extra
          END 
          2: BEGIN
              ;; Draw some circles
              ;; radii
              loop2:
              zoom = congrid(img[minx:minx+numx-1,miny:miny+numy-1],xsw,ysw,/interp)
              tvscl,zoom,_extra=_extra
              FOR i=0,n_elements(radii)-1 DO BEGIN
                  plots,x+radii[i]*xcirc,y+radii[i]*ycirc,/device
              ENDFOR
              print,string(format='("x=",f7.3,",y=",f7.3)',minx+double(x*numx)/double(xsw),miny+double(y*numy)/double(ysw))
              print,'btn1=restart,btn2=again,btn3=exit'
              cursor,x,y,/down,/device
              CASE !mouse.button OF
                  1: BEGIN
                      GOTO,loop0
                  END 
                  2: BEGIN
                      GOTO,loop2
                  END 
                  4: BEGIN
                      return
                  END 
              ENDCASE 
          END 
          4: BEGIN
              return
          END 
      ENDCASE 
      GOTO, loop1
  ENDIF ELSE BEGIN
      out = congrid(img,!d.x_size,!d.y_size,/interp)
      tvscl,out,_extra=_extra
      if keyword_set(examine) then begin
          rdpix,out
      ENDIF
  ENDELSE 
END 
