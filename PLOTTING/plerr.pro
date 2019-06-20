pro plerr,x,y,er, $
          help=help, $
          dolimits=dolimits, $
          limitsonly=limitsonly, $
          errorsonly=errorsonly, $
          idx=idx, $
          symbol_width=symbol_width, $
          _extra=_extra

  npar = n_params()
  nx = n_elements(x)
  ny = n_elements(y)
  ner = n_elements(er)
  nder = (size(er,/n_dimensions)>1)

  want_help = keyword_set(help)
  want_upperlimits = keyword_set(dolimits)
  want_limitsonly = keyword_set(limitsonly)
  want_errorsonly = keyword_set(errorsonly)

  if n_elements(symbol_width) eq 0 then symbol_width = 0.01 ;; width of the symbols in normalised coordinates

  case 1 of
     want_help: begin
        wrong_input = 1
        message = 'not enough parameters given'
     end
     npar ne 3: begin
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
     nder gt 2: begin
        wrong_input = 1
        message = 'The err parameter contains more that two dimensions, what does it mean'
     end        
     ner/nder ne nx: begin
        wrong_input = 1
        message = 'the error array does not contain the right number of elements (nx or nx*2)'
     end
     else: begin
        wrong_input = 0
        message = 'oof no problems'
     end
  endcase

  if wrong_input then begin
     message,/info,message
     message,/info,'USAGE: plerr,x,y,er[,idx=idx,_extra=_extra,help=help,/dolimits,/limitsonly,/errorsonly]'
     message,/info,'USAGE: err can be 2d to have asymmetric values'
     message,/info,'USAGE: idx can select which points to use'
     return
  endif

  case nder of
     1: begin
        ymin = y-er
        ymax = y+er
        ylimit = y*0d0
        idx_limits = where(y lt er,nlimits,complement=idx_errors,ncomplement=nerrors)
        ;; check logic should this not be just 3 sigma (thanks diane)
        if nlimits ne 0 then ylimit[idx_limits] = ymax[idx_limits]
     end
     2: begin
        ermin = reform(er[0,*])
        ermax = reform(er[1,*])
        ymin = y-ermin
        ymax = y+ermax
        ylimit = y*0d0
        idx_limits = where(y lt er,nlimits,complement=idx_errors,ncomplement=nerrors)
        ;; check logic should this not be just 3 sigma (thanks diane)
        if nlimits ne 0 then ylimit[idx_limits] = ymax[idx_limits]
     end
  endcase

  case 1 of
     n_elements(idx) ne 0: begin
        ;; idx already defined, do nothing
     end
     want_limitsonly and (nlimits ne 0): begin
        idx = idx_limits
     end
     want_errorsonly and (nerrors ne 0): begin
        idx = idx_errors
     end
     else: begin
        idx = lindgen(nx)
     end
  endcase
  
  down_arrow_x_norm = [-1,1,0,0,-1,0,1]*symbol_width
  down_arrow_y_norm = [0,0,0,-5,-4,-5,-4]*symbol_width
  
  ;; no simply do a simple loop to plot each error symbol
  for i=0,nx-1 do begin
     if total(i eq idx) gt 0 then begin
        
        if want_upperlimits and (ylimit[i] ne 0) then begin
           ;;draw arrow
           pnorm = convert_coord(x[i],ylimit[i],/data,/to_normal)
           plots,pnorm[0]+down_arrow_x_norm,pnorm[1]+down_arrow_y_norm,/norm,_extra=_extra,noclip=0
        endif else begin
           ;;draw error symbol
           pnorm = convert_coord([x[i],x[i]],[ymin[i],ymax[i]],/data,/to_normal)
           err_symbol_x_norm = pnorm[0,0]+[-1,1,0,0,-1,1]*symbol_width
           err_symbol_y_norm = [pnorm[1,0],pnorm[1,0],pnorm[1,0],pnorm[1,1],pnorm[1,1],pnorm[1,1]]
           plots,err_symbol_x_norm,err_symbol_y_norm,/norm,_extra=_extra,noclip=0
        endelse
     endif
  endfor
  return
end
