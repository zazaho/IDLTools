PRO plbar, $
   range=range, $
   indexrange=indexrange, $
   position=position, $
   boxcolor=boxcolor, $
   smartlabels=smartlabels, $
   top=top, $
   _extra=_extra
  
  default,range,[0,1.]
  default,indexrange,[0,!d.table_size-1]
  default,position,[0.1,0.1,0.95,0.3]
  default,boxcolor,!p.color

  indices=bindgen(indexrange[1]-indexrange[0])+byte(indexrange[0])

  barimage=indices#[1B,1B]

  pl,indexrange,[0,1],color=boxcolor,position=position, $
     /nodata, $
     /noerase, $
     xticks=1,yticks=1, $
     xtickformat='(A1)',ytickformat='(A1)', $
     xtitle="",ytitle=""

  contour,barimage,double(indices),[0,1.], $
          /fill, $
          /overplot, $
          levels=indices, $
          c_colors=indices, $
          position=position, $
          _extra=_extra

  pl,range,[0,1],color=boxcolor,position=position, $
     /nodata, $
     /noerase, $
     /xticks,/yticks, $
     xtickformat='(A1)',ytickformat='(A1)', $
     xtitle="",ytitle=""

  if keyword_set(top) then xaxis=1 else xaxis=0

  if keyword_set(smartlabels) then begin
     ;; minimum 2 labels
     
     if smartlabels lt 2 then numlabels=3 else numlabels=smartlabels
     rough_positions=(dindgen(numlabels)+1)/(numlabels+1)*(range[1]-range[0])+range[0]

     ;; we would like to have N nice round labels and away from the
     ;; very edge of the bar
     rough_step=rough_positions[1]-rough_positions[0]
     rough_interval=rough_positions[numlabels-1]-rough_positions[0]
     ;; eg 2.123e-2 -> 2e-2
     order = 10d0^(floor(alog10(rough_step))) ;; 1e-2
     round_step = round(rough_interval/order)*order/double(numlabels-1.0)

     ;; the nearest nice label[0] given that order of the step is order
     ;4.12e-1 -> 4.1e-1
     round_position0 = round(rough_positions[0]/order)*order
     
     label_positions=round_position0+round_step*dindgen(numlabels)

     axis,xaxis=xaxis,color=boxcolor,/xstyle, $
          xticks=n_elements(label_positions)-1, $
          xtickv=label_positions, $
          xticklen=1.0
          _extra=_extra
  endif else begin
     axis,xaxis=xaxis,color=boxcolor,/xstyle,_extra=_extra
  endelse

END
