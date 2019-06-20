pro ple,x,y, $
        x_errup=x_errup, $
        x_errdown=x_errdown, $
        y_errup=y_errup, $
        y_errdown=y_errdown, $
        help=help, $
        dolimits=dolimits, $
        omitlimits=omitlimits, $
        idx=idx, $
        symbol_width=symbol_width, $
        _extra=_extra

  npar = n_params()
  nx = n_elements(x)
  ny = n_elements(y)
  nder = (size(er,/n_dimensions)>1)

  want_help = keyword_set(help)
  use_upperlimits = keyword_set(dolimits)
  want_limitsonly = keyword_set(limitsonly)
  want_errorsonly = keyword_set(errorsonly)
  if keyword_set(omitlimits) then drawlimits=0 else drawlimits=1

  if n_elements(symbol_width) eq 0 then symbol_width = 0.01 ;; width of the symbols in normalised coordinates

  case 1 of
     want_help: begin
        wrong_input = 1
        message = 'not enough parameters given'
     end
     npar ne 2: begin
        wrong_input = 1
        message = 'not enough parameters given'
     end
     nx eq 0: begin
        wrong_input = 1
        message = 'no valid elements in the x array'
     end
     ny eq 0: begin
        wrong_input = 1
        message = 'no valid elements in the y array'
     end
     nx ne ny: begin
        wrong_input = 1
        message = 'the nx and ny array are not the same length'
     end
     else: begin
        wrong_input = 0
        message = 'oof no problems'
     end
  endcase

  if wrong_input then begin
     message,/info,message
     message,/info,'USAGE: ple,x,y[,x_errup=x_errup,x_errdown=x_errdown,y_errup=y_errup,y_errdown=y_errdown,idx=idx,_extra=_extra,help=help,/dolimits,/limitsonly,/errorsonly]'
     message,/info,'USAGE: idx can select which points to use'
     return
  endif

  xlimit = x*0d0
  if n_elements(x_errdown) eq nx then begin
     xmin = x-x_errdown
  endif else begin
     xmin = x
  endelse
  if n_elements(x_errup) eq nx then begin
     xmax = x+x_errup
  endif else begin
     xmax = x
  endelse
  idx_x_limits = where(xmin lt 0d0,nxlimits,complement=idx_x_errors,ncomplement=nxerrors)
  if nxlimits ne 0 then xlimit[idx_x_limits] = xmax[idx_x_limits]
  
  ylimit = y*0d0
  if n_elements(y_errdown) eq ny then begin
     ymin = y-y_errdown
  endif else begin
     ymin = y
  endelse
  if n_elements(y_errup) eq ny then begin
     ymax = y+y_errup
  endif else begin
     ymax = y
  endelse
  idx_y_limits = where(ymin lt 0d0,nylimits,complement=idx_y_errors,ncomplement=nyerrors)
  if nylimits ne 0 then ylimit[idx_y_limits] = ymax[idx_y_limits]

  case 1 of
     n_elements(idx) ne 0: begin
        ;; idx already defined, do nothing
     end
     else: begin
        idx = lindgen(nx)
     end
  endcase
  
  down_arrow_x_norm = [-1,1,0,0,-1,0,1]*symbol_width
  down_arrow_y_norm = [0,0,0,-5,-4,-5,-4]*symbol_width
  
  left_arrow_X_norm = [0,0,0,-5,-4,-5,-4]*symbol_width
  left_arrow_y_norm = [-1,1,0,0,-1,0,1]*symbol_width
  
  ;; no simply do a simple loop to plot each error symbol
  for i=0,nx-1 do begin
     if total(i eq idx) gt 0 then begin
        
        ;; x
        if use_upperlimits and (xlimit[i] ne 0) then begin
           thisx  = xlimit[i]
        endif else begin
           thisx  = x[i]
        endelse

        if use_upperlimits and (ylimit[i] ne 0) then begin
           thisy  = ylimit[i]
        endif else begin
           thisy  = y[i]
        endelse
        
        if drawlimits or ((xlimit[i] eq 0) and (ylimit[i] eq 0)) then begin
           if use_upperlimits and (xlimit[i] ne 0) then begin
              ;;draw arrow
              pnorm = convert_coord(thisx,thisy,/data,/to_normal)
              plots,pnorm[0]+left_arrow_x_norm,pnorm[1]+left_arrow_y_norm,/norm,_extra=_extra,noclip=0
           endif else begin
              ;;draw error symbol
              pnorm = convert_coord([xmin[i],xmax[i]],[thisy,thisy],/data,/to_normal)
              err_symbol_x_norm = [pnorm[0,0],pnorm[0,0],pnorm[0,0],pnorm[0,1],pnorm[0,1],pnorm[0,1]]
              err_symbol_y_norm = pnorm[1,0]+[-1,1,0,0,-1,1]*symbol_width
              plots,err_symbol_x_norm,err_symbol_y_norm,/norm,_extra=_extra,noclip=0
           endelse
           
           ;; y
           if use_upperlimits and (ylimit[i] ne 0) then begin
              ;;draw arrow
              pnorm = convert_coord(thisx,thisy,/data,/to_normal)
              plots,pnorm[0]+down_arrow_x_norm,pnorm[1]+down_arrow_y_norm,/norm,_extra=_extra,noclip=0
           endif else begin
              ;;draw error symbol
              pnorm = convert_coord([thisx,thisx],[ymin[i],ymax[i]],/data,/to_normal)
              err_symbol_x_norm = pnorm[0,0]+[-1,1,0,0,-1,1]*symbol_width
              err_symbol_y_norm = [pnorm[1,0],pnorm[1,0],pnorm[1,0],pnorm[1,1],pnorm[1,1],pnorm[1,1]]
              plots,err_symbol_x_norm,err_symbol_y_norm,/norm,_extra=_extra,noclip=0
           endelse
        endif
     endif
  endfor
  return
end
