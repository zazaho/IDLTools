;+
; NAME:
; pointdensity
;
;
; PURPOSE:
; calculate the weighted density of points for plotting as a hess diagram
;
;
; CATEGORY:
; analysis
;
;
; CALLING SEQUENCE:
; map = pointdensity(x,y [,weigth=weight,xstdev=xstdev,xstdev=xstdev
;                         ,index=index
;                         ,nbins=nbins
;                         ,xmin=xmin,xmax=xmax,xrange=xrange
;                         ,ymin=ymin,ymax=ymax,yrange=yrange
;                         ,xlog=xlog,ylog=ylog,ll=ll])
;
;
; INPUTS:
; x: array of x values
; y: array of y values
;
; OPTIONAL INPUTS:
; xstdev: array of stdev values on the x values
; ystdev: array of stdev values on the y values
; weight: array of weight of each x,y pair
; index: array of index of points to take into account
;
; KEYWORD PARAMETERS:
; nbins: 1D or 2D array of dimensions of the output image
;        if 1D the output will be [nbins,nbins]
; xmin: minimum value of x to take into account
; xmax: maximum value of x to take into account
; xrange: range of values of x to take into account
; ymin: minimum value of y to take into account
; ymax: maximum value of y to take into account
; yrange: range of values of y to take into account
; xlog: if set then make steps in x in log-space
; ylog: if set then make steps in y in log-space
; ll:  if set then make steps in x and y in log-space
; nan:  if set then remove NaNs
; help: show this help
;
; OUTPUTS:
; structure which holds the output density map and the information on
; the centers and limits x and y coordinates
;
;
; MODIFICATION HISTORY:
; (SH Feb  9 2017): Initial version
;-

function pointdensity,x,y, $
                 weight=weight,xstdev=xstdev,ystdev=ystdev, $
                 index=index, $
                 nbins=nbins, $
                 xmin=xmin,xmax=xmax,xrange=xrange, $
                 ymin=ymin,ymax=ymax,yrange=yrange, $
                 xlog=xlog,ylog=ylog,ll=ll, $
                 nan=nan, $
                 help=help


  default,nbins,[21,21]
  if size(nbins,/n_dimension) lt 2 then nbins = [nbins,nbins]
  xnbins = nbins[0]
  ynbins = nbins[1]
  
  default,xlog,0
  default,ylog,0
  if keyword_set(ll) then begin
     xlog=1
     ylog=1
  endif

  if n_elements(xrange) eq 2 then begin
     xmin=xrange[0]
     xmax=xrange[1]
  endif
  
  if n_elements(yrange) eq 2 then begin
     ymin=yrange[0]
     ymax=yrange[1]
  endif
  
  haserror=0
  
;; checks
  if keyword_set(help) then begin
     haserror=1
     goto, doerror
  endif

  if n_params() ne 2 then begin
     message_info,/info,"Insufficient input parameters"
     goto, doerror
  endif

  nx = n_elements(x)
  ny = n_elements(y)
  if nx ne ny then begin
     message_info,/info,"Dimensions of x and y are different"
     haserror=1
     goto, doerror
  endif

  case n_elements(weight) of
     0: begin
        wght = make_array(nx,value=1d0)
     end
     1: begin
        wght = make_array(nx,value=weight)
     end
     nx: begin
        wght = double(weight)
     end
     else: begin
        message_info,/info,"Dimensions of weight differ from x and y"
        haserror=1
        goto, doerror
     end
  endcase

  case n_elements(xstdev) of
     0: begin
        have_xstdv = 0
        ;; dummy
        xstdv = make_array(nx,value=1)
     end
     1: begin
        have_xstdv = 1
        xstdv = make_array(nx,value=xstdev)
     end
     nx: begin
        have_xstdv = 1
        xstdv = double(xstdev)
     end
     else: begin
        message_info,/info,"Dimensions of xstdev differ from x"
        haserror=1
        goto, doerror
     end
  endcase

  case n_elements(ystdev) of
     0: begin
        have_ystdv = 0
        ;; dummy
        ystdv = make_array(ny,value=1)
     end
     1: begin
        have_ystdv = 1
        ystdv = make_array(ny,value=ystdev)
     end
     ny: begin
        have_ystdv = 1
        ystdv = double(ystdev)
     end
     else: begin
        message_info,/info,"Dimensions of ystdev differ from y"
        haserror=1
        goto, doerror
     end
  endcase

  case 1 of
     n_elements(index) eq 0: begin
        ;; do nothing
        xx=x
        yy=y
        xxstdv=xstdv
        yystdv=ystdv
     end
     ;; invalid indices
     min(index) lt 0 or max(index) gt nx-1: begin
        message_info,/info,"The values in index point outside of the array bounds"
        haserror=1
        goto, doerror
     end
     else: begin
        xx=x[index]
        yy=y[index]
        xxstdv=xstdv[index]
        yystdv=ystdv[index]
        wght=wght[index]
     end
  endcase

  if xlog eq 1 then begin
     ;; do naturals to make things simpler
     xxstdv = xxstdv/xx
     xx = alog(xx)
  endif
  
  if ylog eq 1 then begin
     ;; do naturals to make things simpler
     yystdv = yystdv/yy
     yy = alog(yy)
  endif

  if keyword_set(nan) then begin
     idx = where(finite(xx) and finite(yy),cnt)
     if cnt eq 0 then begin
        message_info,/info,"All (x,y) pairs are removed because of nans"
        haserror=1
        goto, doerror
     endif
     xx=xx[index]
     yy=yy[index]
     xxstdv=xxstdv[index]
     yystdv=yystdv[index]
     wght=wght[index]
  endif

  nxxyy=n_elements(xx)

  if n_elements(xmin) eq 1 then begin
     if xlog eq 1 then xxmn = alog(xmin) else xxmn=xmin
  end else begin
     xxmn  = min(xx)
  endelse
  
  if n_elements(xmax) eq 1 then begin
     if xlog eq 1 then xxmx = alog(xmax) else xxmx=xmax
  end else begin
     xxmx  = max(xx)
  endelse
  
  if n_elements(ymin) eq 1 then begin
     if ylog eq 1 then yymn = alog(ymin) else yymn=ymin
  end else begin
     yymn  = min(yy)
  endelse
  
  if n_elements(ymax) eq 1 then begin
     if ylog eq 1 then yymx = alog(ymax) else yymx=ymax
  end else begin
     yymx  = max(yy)
  endelse

  xbinsize = double(xxmx-xxmn)/double(xnbins)
  ybinsize = double(yymx-yymn)/double(ynbins)

  ;; create coordinate images of the centers of the pixels
  foo = lindgen(xnbins,ynbins)
  x_img = xxmn + xbinsize*(foo mod xnbins + 0.5)
  y_img = yymn + ybinsize*(foo  /  xnbins + 0.5)

  if have_xstdv eq 0 and have_ystdv eq 0 then begin
     densitymap = hist2d(xx,yy,min=[xxmn,yymn],max=[xxmx,yymx],bin=[xbinsize,ybinsize])
  endif else begin
     ;; this is more complicated because each point defines an
     ;; elliptical density blob. Do a stupid loop for now
     densitymap = make_array(xnbins,ynbins,value=0d0)

     case 1 of
        have_xstdv eq 1 and have_ystdv eq 1: begin
           for iii=0,nxxyy-1 do begin
              densitymap += xbinsize/(xxstdv[iii]*sqrt(2d0*!dpi))*exp(-1d0*(x_img-xx[iii])^2/(2*xxstdv[iii]^2)) * $
                               ybinsize/(yystdv[iii]*sqrt(2d0*!dpi))*exp(-1d0*(y_img-yy[iii])^2/(2*yystdv[iii]^2))
           endfor
        end
        have_xstdv eq 1 and have_ystdv eq 0: begin
           for iii=0,nxxyy-1 do begin
              foo = min(abs(y_img[0,*]-yy[iii]),idx_this_y)
              densitymap[*,idx_this_y] += xbinsize/(xxstdv[iii]*sqrt(2d0*!dpi))*exp(-1d0*(x_img[*,idx_this_y]-xx[iii])^2/(2*xxstdv[iii]^2))
           endfor
        end
        have_xstdv eq 0 and have_ystdv eq 1: begin
           for iii=0,nxxyy-1 do begin
              foo = min(abs(x_img[*,0]-xx[iii]),idx_this_x)
              densitymap[idx_this_x,*] += ybinsize/(yystdv[iii]*sqrt(2d0*!dpi))*exp(-1d0*(y_img[idx_this_x,*]-yy[iii])^2/(2*yystdv[iii]^2))
           endfor
        end
     endcase
     
  endelse

  ;; transform the values back to linear
  if xlog eq 1 then x_img=exp(x_img)
  if ylog eq 1 then y_img=exp(y_img)

  return,{x:reform(x_img[*,0]),y:reform(y_img[0,*]),ximg:x_img,yimg:y_img,densitymap:densitymap}

  doerror:
  if haserror eq 1 then begin
     doc_library,"pointdensity"
     return,!values_d_nan
  endif

end