;#> sh_cont_fit.dc1
; Identifier   sh_cont_fit
;
; Purpose      Interactively select continuum points and fit
;              a continuum to these
;
; Synopsis     cont = sh_cont_fit( input
;                               [, sel]
;                               [, cont=cont_in]
;                               [, method=method]
;                               [, filename=filename] )
;
; Arguments    Name        I/O  Type:    Description:
; ---------------------------------------------------------------------------
;              cont         O   struct   Output AAR (identical to the input
;                                        AAR) containing the continuum
;              input        I   str/asc  Input AAR or a filename containing
;                                        x and y data in the first two columns
;                                        and line in the fifth (same structure
;                                        as the output file created by SH_CONT_FIT)
;              sel         I/O  str/asc  Input/Output AAR structure or a file
;                                        with the selected continuum points 
;              cont         I   struct   Input AAR (must be identical to AAR).
;                                        The bands fitted are updated the
;                                        rest remain unchanged.
;              method       I   string   Default method to use for continuum 
;                                        fit. 
;                                         'SPLINE'      : NR_SPLINT spline fit
;                                         'POLY.X'      : orthonormal polynomial
;                                                         of degree X
;                                         'POLYINT'     : Polynomial fit 
;                                                         (N-1 degree) 
;                                                         Astron lib : POLINT
;                                         'BLACKBODY.X' : blackbody with
;                                                         initial T estimate X
;                                        [Default: 'SPLINE']
;              filename     O   string   Ascii output file containing only
;                                        the data to which a continuum
;                                        has been fitted.
;                                        Written are wave,flux,stdev,cont,line
;                                        [Default: NO]
;
; Returns      Continuum AAR and (optionally) selected data-points
;
; Description  The user (de-)selects points in the main top plot.
;              The selection is done with the three mouse buttons
;              in the large top-left plot. In the other three plots
;              the user can use the mouse to change the x-and
;              y-ranges and activate the application of the continuum.
;              This has the same effect as the APPLY button and the 
;              editable X and Y range fields.
;
;              Mouse action
;              ------------
;
;                Main plot, top left:
;                   LEFT  : add a point
;                   MIDDLE: move a point, by clicking near/on a point 
;                           and moving the cursor while keeping the mouse
;                           button pressed. The new position is the
;                           position where the mouse button was released.
;                   RIGHT : remove a point (nearest in wavelength)
;                Small plot, top right
;                   LEFT   : - change the x-range for the large plots
;                              plot by pressing the mouse-button and moving
;                              the cursor while keeping the mouse-button
;                              pressed.
;                            - auto-scaling by clicking the mouse-button once
;                   MIDDLE : - change the y-range (only for selection plot)
;                              plot by pressing the mouse-button and moving
;                              the cursor while keeping the mouse-button
;                            - autoscaling by clicking the mouse-button once
;                   RIGHT  : fit a continuum to the selected points
;                Large plot, bottom left
;                    SAME AS SMALL TOP_RIGHT PLOT
;                Small plot, bottom right
;                    SAME AS SMALL TOP_RIGHT PLOT
;
;              PLOTS
;              -----
;
;                The selected points are plotted as red diamonds. 
;                De-selected points as green asterisks.
;                The fitted continuum is plotted as a green continuous line.
;                With vertical blue lines the AOT-band borders are 
;                plotted.
;
;                The plot-areas of the large plots are plotted into the
;                small plots with a red square.
;                          
;              WIDGET MENUS, BUTTONS AND VALUES
;              --------------------------------
;
;                BUTTONS:
;                --------
;                  CLEAR     : remove the selected points
;                  APPLY     : apply the current selection 
;                  EXIT      : end the session
;
;                MENUS:
;                ------
;                  METHOD     : method to use for fitting the continuum
;                                'DEFAULT'     : default method
;                                'SPLINE'      : NR_SPLINT spline fit
;                                'POLY.X'      : orthonormal polynomial
;                                                of degree X
;                                'POLYINT'     : ISAP POLYINT routine
;                                'BLACKBODY.X' : blackbody with initial 
;                                                temperature estimate X
;                               The default method is the SPLINE fit.
;                               The default method can also be given on input.
;                  CORRECTION : either an offset or a scaling correction
;                  AOT_BAND   : selects a single AOT-band of the bands present
;                               in the AAR or all data in the AAR
;                  LINE       : select a line (or all) from the lines present 
;                               in the current AOT-band
;
;                VALUES:
;                -------
;                  XRANGE     : set the x-range
;                               The x-range can also be set by using the cursor
;                               in the small plot on the right
;                  YRANGE(S)  : set the y range
;                               The y range can also be set by using the cursor
;                               in the small plot on the right
;                               Applied when the continuum is newly applied
;                               thus either by pressing the APPLY button or
;                               or pressing the right mouse button in one of
;                               three appropriate windows (see above).
;
; Examples     
;
; Comments    In V 1.2 a number of options are added:
;                - ascii input/output
;                - selectable offset or scaling correction
;                - LINE selection
;                - Only the AOT_bands present in the AAR can be selected
;                  (and also only LINES present in the selected AOT_band)
;
; Category     UTIL
;
; Filename     sh_cont_fit.pro
;
; Author       Fred Lahuis
;
; Version      1.3
;
; History      0.0  27-11-1997 FL  document created
;              1.0  04-12-1997 FL  inserted in IA
;              1.1  19-01-1997 FL  SPR_S0311, opfit1d -> opfit_1d
;              1.2  22-01-1997 FL  SPR_S0315, small bug fixes
;                                  SPR_S0316/018, options added
;              1.3  16-07-1998 EkW resolve ISAP dependency
;#<
;
; ver is not available on all machines, so just copied it in
;
PRO CLEAN_COMM

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO CLEAN_COMM'

;
; initialization/defaults
;
  offset = 0
  line = 0
  aot_band = 'ALL'
  bbtemp = fltarr(2)
  mbbtemp = fltarr(2)
  plot_index = intarr(4)

  BAD_RANGE = -9999.
  nselect = 0
  xrange = [BAD_RANGE,BAD_RANGE]
  yrange_sel = [BAD_RANGE,BAD_RANGE]
  yrange_cont = [BAD_RANGE,BAD_RANGE]
  aar_sel = 0
END

PRO CF_VER, x, device=device, linestyle=ls, color=clr, $
            normalized=norm, thickness=thk

error,'D','PRO CF_VER'
 
xx = x
if n_elements(ls) eq 0 then ls = !p.linestyle
if n_elements(clr) eq 0 then clr = !p.color
if n_elements(thk) eq 0 then thk = !p.thick
n = n_elements(xx)
 
if keyword_set(device) then begin
  yy = [0,!d.y_size-1]
  for i = 0, n-1 do begin
    plots,/device,[0,0]+xx(i),yy,linestyle=ls,color=clr,thick=thk
  endfor
endif else if keyword_set(norm) then begin
  yy = [0,1]
  for i = 0, n-1 do begin
    plots,/norm,[0,0]+xx(i),yy,linestyle=ls,color=clr,thick=thk
  endfor
endif else begin
  yy = [!y.range, !y.crange]
  for i = 0, n-1 do begin
    if !y.type eq 0 then begin
      oplot, [1.,1.]*xx(i),[min(yy),max(yy)],linestyle=ls,color=clr, $
	thick=thk
    endif else begin
      oplot,[1.,1.]*xx(i),10^[min(yy),max(yy)],linestyle=ls,color=clr, $
	thick=thk
    endelse
  endfor
endelse
return
END
;
; hot, idem
; 
PRO CF_HOR, y, device=device, linestyle=ls, color=clr, $
            thickness=thk, normalized=norm
 
error,'D','PRO CF_HOR'

yy = y
n = n_elements(yy)
if n_elements(ls) eq 0 then ls = !p.linestyle
if n_elements(clr) eq 0 then clr = !p.color
if n_elements(thk) eq 0 then thk = !p.thick

if keyword_set(device) then begin
  xx = [0,!d.x_size-1]
  for i = 0, n-1 do begin
    plots, /device,xx,[0,0]+yy(i),linestyle=ls,color=clr,thick=thk
  endfor
endif else if keyword_set(norm) then begin
  xx = [0,1]
  for i = 0, n-1 do begin
    plots, /norm,xx,[0,0]+yy(i),linestyle=ls,color=clr,thick=thk
  endfor
  endif else begin
  xx = [!x.range, !x.crange]
  for i = 0, n-1 do begin
    if !x.type eq 0 then begin
      oplot,[min(xx),max(xx)],[1.,1.]*yy(i),linestyle=ls,color=clr,$
	thick=thk
    endif else begin
      oplot,10^[min(xx),max(xx)],[1.,1.]*yy(i),linestyle=ls,color=clr,$
	thick=thk
    endelse
  endfor
endelse
return
end

FUNCTION READ_FILE, file

  error,'D','FUNCTION READ_FILE'

  file = string(file)
  openr,unit,file,/get_lun,error=status
  if ( status ne 0 ) then return,-1
  no = 0 & x = 0. &  y = 0. & l = 0 & s = 0.
  while not eof(unit) do begin
    a = ''
    readf,unit, a
    a = strtrim(strcompress(a),2)
    if ( strlen(a) gt 0 ) then begin
      n = 1
      for i=0,strlen(a)-1 do if ( strmid(a,i,1) eq ' ' ) then n=n+1
      if ( n ge 2 ) then begin
        case n of 
         2 : reads,a,f1,f2
         3 : reads,a,f1,f2,f3
         4 : reads,a,f1,f2,f3,f4
         5 : reads,a,f1,f2,f3,f4,f5
         else : reads,a,f1,f2,f3,f4,f5
        endcase
        x = [x,f1]
        y = [y,f2]
        if ( n ge 3 ) then s = [s,f3] else s = [s,0]
        if ( n ge 5 ) then l = [l,f5] else l = [l,0]
        no = no + 1
      endif
    endif
  endwhile
  close,unit
  if ( no ge 2 ) then begin
    aar = sh_define_aar(length=no)
    aar.data.wave  = x(1:no)
    aar.data.flux  = y(1:no)
    aar.data.stdev = s(1:no)
    aar.data.line  = l(1:no)
  endif else aar=0

return,aar
END

PRO PLOT_DATA,plot=do_plots

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def,mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO PLOT_DATA'

  aot_band = strupcase(aot_band(0))

  check_range

  if ( n_elements(do_plots) eq 0 ) then do_plots=[1,2,3,4]

  iscont = 0
  if ( is_aar(cont_band) ) then begin
    index = where(cont_band.data.tint eq 1, count)
    if ( count gt 0 ) then iscont = 1
  endif 

  for i=0,n_elements(do_plots)-1 do begin
    title = object
    if ( aot_band ne '' ) then title = title + ' AOT_BAND '+aot_band $
    else title = title + ' ALL BANDS'
    if ( line ne 0 ) then title = title + ' LINE '+strtrim(line,2)
    title = title + ', ' + method
    if ( method eq 'POLY' ) then title = title +'_'+strtrim(degree,2)
    title = title + ' FIT'
    if ( method eq 'BLACKBODY' and bbtemp(1) ne 0. ) then begin
      t = fix( bbtemp(1) + 0.5 - (bbtemp(1) LT 0) )
      title = title + ' ' + strtrim(t,2) + 'K'
    endif
    ;(SH Mar 24 1999)
    if ( method eq 'MODIFIED BLACKBODY' and mbbtemp(1) ne 0. ) then begin
      t = fix( mbbtemp(1) + 0.5 - (mbbtemp(1) LT 0) )
      title = title + ' ' + strtrim(t,2) + 'K'
    endif

    case do_plots(i) of
     1 : begin
           error,'D','PLOTTING SELECTION'
           if ( !d.window ne plot_index(0) ) then wset, plot_index(0)
           plot,aar_band.data.wave,aar_band.data.flux, $
                xrange=xrange,yrange=yrange_sel, $
                xtitle='Wavelength [microns]',title=title, $
                xstyle=1,ystyle=1,col=1
           cf_ver,band_def.data.lo_wavelength,col=4
           cf_ver,band_def.data.hi_wavelength,col=4
           if ( nselect gt 0 ) then begin
             oplot,cont_band.data.wave,cont_band.data.flux,col=3
             plots,wave_sel,flux_sel,ps=4,col=2,symsize=1.5
           endif
         end
     2 : begin
           error,'D','PLOTTING SELECTION OVERVIEW'
           if ( !d.window ne plot_index(1) ) then wset, plot_index(1)
           plot,aar_band.data.wave,aar_band.data.flux, $
                xtitle='Wavelength [microns]', $
                xstyle=1,ystyle=1,col=1
           cf_ver,band_def.data.lo_wavelength,col=4
           cf_ver,band_def.data.hi_wavelength,col=4
           if ( xrange(0) ne !x.crange(0) or $
                xrange(1) ne !x.crange(1) or $
                yrange_sel(0) ne !y.crange(0) or $
                yrange_sel(1) ne !y.crange(1) ) then $
             plots,[xrange(0),xrange(0),xrange(1),xrange(1),xrange(0)], $
                   [yrange_sel(0),yrange_sel(1),yrange_sel(1),$
                    yrange_sel(0),yrange_sel(0)], $
                   linest=2,col=2
           if ( nselect gt 0 ) then begin
             oplot,cont_band.data.wave,cont_band.data.flux,col=3
             plots,wave_sel,flux_sel,ps=4,col=2
           endif
         end
     3 : begin
           error,'D','PLOTTING CONTINUUM'
           if ( !d.window ne plot_index(2) ) then wset, plot_index(2)
           if ( nselect gt 0 and iscont ) then begin
             if ( offset ) then y = aar_band.data.flux - cont_band.data.flux $
             else y = aar_band.data.flux / cont_band.data.flux
             case n_elements(yrange_cont) of
              2 : if ( yrange_cont(0) eq 0 and yrange_cont(1) eq 0 ) then $
                    yrange_cont=0
              else : yrange_cont = 0
             endcase
             if ( n_elements(yrange_cont) ne 2 ) then begin
               index = where(aar_band.data.wave ge xrange(0) and $
                             aar_band.data.wave le xrange(1), count)
               if ( count eq 0 ) then yr =[min(y),max(y)] $
               else yr = [min(y(index)),max(y(index))]
             endif else yr = yrange_cont
             error,'D','YRANGE_CONT ' + string(yr(0)) + string(yr(1))
             plot,aar_band.data.wave,y,xrange=xrange,yrange=yr, $
                  xtitle='Wavelength [microns]',title=title, $
                  xstyle=1,ystyle=1,col=1
             cf_ver,band_def.data.lo_wavelength,col=4
             cf_ver,band_def.data.hi_wavelength,col=4
             if ( offset ) then cf_hor,0,col=3,lines=4 $
             else cf_hor,1,col=3,lines=4
             yrange_cont = !y.crange
           endif else begin
             plot,[0,1],[0,1],/nodata
             xyouts,0.5,0.5,alig=0.5,'NO CONTINUUM YET'
           endelse
         end
     4 : begin
           error,'D','PLOTTING CONTINUUM OVERVIEW'
           if ( !d.window ne plot_index(3) ) then wset, plot_index(3)
           if ( nselect gt 0 and iscont ) then begin
             if ( offset ) then y = aar_band.data.flux - cont_band.data.flux $
             else y = aar_band.data.flux / cont_band.data.flux
             plot,aar_band.data.wave,y, $
                  xtitle='Wavelength [microns]',xstyle=1,ystyle=1,col=1
             if ( offset ) then cf_hor,0,col=3,lines=4 $
             else cf_hor,1,col=3,lines=4
             cf_ver,band_def.data.lo_wavelength,col=4
             cf_ver,band_def.data.hi_wavelength,col=4
             if ( n_elements(yrange_cont) eq 2 ) then begin
               if ( xrange(0) ne !x.crange(0) or $
                    xrange(1) ne !x.crange(1) or $
                    yrange_cont(0) ne !y.crange(0) or $
                    yrange_cont(1) ne !y.crange(1) ) then $
                 plots,[xrange(0),xrange(0),xrange(1),xrange(1),xrange(0)], $
                       [yrange_cont(0),yrange_cont(1),yrange_cont(1), $
                        yrange_cont(0),yrange_cont(0)], $
                       linest=2,col=2
             endif
           endif else begin
             plot,[0,1],[0,1],/nodata
             xyouts,0.5,0.5,alig=0.5,'NO CONTINUUM YET'
           endelse
         end
     else:
    endcase
  endfor
  return
END

PRO SELECT_LINES, menu=MenuLINEpoly_int


  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO SELECT_LINES'

  lines = aar_band.data.line
  lines = lines(uniq(lines,sort(lines)))
  MenuLINE = [ { CW_PDMENU_S, 3, 'LINE' } ]
  for i=0,n_elements(lines)-1 do $
    MenuLINE = [ MenuLINE, { CW_PDMENU_S, 0, strtrim(lines(i),2) } ]
  MenuLINE = [ MenuLINE, { CW_PDMENU_S, 2, 'ALL' } ]

  if ( WIDGET_ID.LINE ne 0L ) then WIDGET_CONTROL, WIDGET_ID.LINE, /DESTROY
  WIDGET_ID.LINE = CW_PDMENU( WIDGET_ID.BASE2, $
                   MenuLINE, $
                   /RETURN_FULL_NAME, $
                   UVALUE='LINE')
  WIDGET_CONTROL, WIDGET_ID.LINE, /REALIZE
END

PRO AOT_BAND_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO AOT_BAND_Event'

  if ( n_params() ne 0 ) then begin
    aot_band = strupcase(strmid(Event.Value,9,strlen(Event.Value)-9))
    aot_band = aot_band(0)
  endif
  if ( aot_band eq 'ALL' ) then aot_band = ''

  error,'M','SELECTED AOT_BAND '+aot_band
;
; select the aot_band
;
  if ( aot_band eq '' ) then begin
    count = n_elements(aar.data)
    bool = replicate(1,count)
  endif else begin
    dets = band2det(aot_band)
    ador = aot_to_apband(aot_band)
    bool = aar.data.det ge min(dets) AND $
           aar.data.det le max(dets) AND $
           test_status(aar,aper=ador(0)) AND $
           test_flag(aar.data.flag,order=ador(2))
  endelse
;
; select the lines for this band
;
  aar_band = select(aar,bool)
  cont_band = select(cont,bool)
  select_lines
;
; select the actual line
;
  if ( line ne 0 ) then begin
    bool = temporary(bool) and aar.data.line eq line
    aar_band = select(aar,bool)
    cont_band = select(cont,bool)
  endif
  index_band = where(bool)
;
; reset the plot ranges
;
  xrange = [BAD_RANGE,BAD_RANGE]
  yrange_sel = [BAD_RANGE,BAD_RANGE]
  yrange_cont = [BAD_RANGE,BAD_RANGE]
  check_range
;
; select the continuum wavelength and flux if present
;
  nselect = 0
  if ( is_aar(aar_sel) ) then begin
    if ( aot_band eq '' ) then begin
      bool_sel = replicate(1,n_elements(aar_sel.data))
    endif else begin
      bool_sel = aar_sel.data.det ge min(dets) AND $
                 aar_sel.data.det le max(dets) AND $
                 test_status(aar_sel,aper=ador(0)) AND $
                 test_flag(aar_sel.data.flag,order=ador(2))
    endelse
    index_sel = where(bool_sel,nselect)
    if ( nselect gt 0 ) then begin
      if ( line ne 0 ) then begin
        bool = temporary(bool) and aar_sel.data.line eq line
        index = where(bool,count)
        if ( count gt 0 ) then begin
          bool_sel = bool
          index_sel = index
          nselect = count
        endif else aar_sel.data(index_sel).line = line
      endif
    endif
    if ( nselect eq 0 ) then begin
      bool_sel = aar_sel.data.wave ge xrange(0) and $
                 aar_sel.data.wave le xrange(1)
      index_sel = where(bool_sel,nselect)
    endif
;
; the tint tag is used as a flag to indicate if it is aot-band
; data or not
;
    aar_sel.data.tint = 0
    if ( nselect gt 0 ) then begin
      aar_sel.data(index_sel).tint = 1
      wave_sel = aar_sel.data(index_sel).wave
      flux_sel = aar_sel.data(index_sel).flux
    endif
  endif
  yrange_cont = 0
  widget_control,/hourglass
  plot_data

  return
END

PRO LINE_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO LINE_Event'

  line = strupcase(strmid(Event.Value,5,strlen(Event.Value)-5))
  if ( line eq 'ALL' ) then line = 0 
  line = fix(line)

  error,'M','SELECTED LINE '+strtrim(line,2)

  AOT_BAND_Event

END

PRO METHOD_Event, Event, method=method_in

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO METHOD_Event'

  if ( n_elements(method_in) eq 0 ) then begin
    method = strupcase(strmid(Event.Value,7,strlen(Event.Value)-7))
    method = method(0)
  endif else method = strupcase(strtrim(method_in,2))

  if ( method eq 'DEFAULT' ) then method = strupcase(method_def)
   
  error,'M','SELECTED METHOD '+method
  if ( strmid(method,0,4) eq 'POLY' and method ne 'POLYINT' ) then begin
    case strmid(method,4,1) of
     '.' : degree = fix(strmid(method,5,strlen(method)-5))
     '_' : degree = fix(strmid(method,5,strlen(method)-5))
     '-' : degree = fix(strmid(method,5,strlen(method)-5))
     else : degree = fix(strmid(method,4,strlen(method)-4))
    endcase
    method = 'POLY'
    error,'D','METHOD '+method
    error,'D','DEGREE '+string(degree)
  endif

  if ( strmid(method,0,9) eq 'BLACKBODY' ) then begin
    bbtemp = fltarr(2)
    if ( method eq 'BLACKBODY' ) then bbtemp(0) = 200 $
    else begin
      case strmid(method,9,1) of
       '.' : bbtemp(0) = float(strmid(method,10,strlen(method)-10))
       '_' : bbtemp(0) = float(strmid(method,10,strlen(method)-10))
       '-' : bbtemp(0) = float(strmid(method,10,strlen(method)-10))
       else : bbtemp(0) = float(strmid(method,9,strlen(method)-9))
      endcase
    endelse
    method = 'BLACKBODY'
    error,'D','METHOD '+method
    error,'D','BBTEMP '+string(bbtemp(0))
  endif
  
  if ( strmid(method,0,18) eq 'MODIFIED BLACKBODY' ) then begin
    mbbtemp = fltarr(2)
    if ( method eq 'MODIFIED BLACKBODY' ) then mbbtemp(0) = 200 $
    else begin
      case strmid(method,18,1) of
       '.' : mbbtemp(0) = float(strmid(method,19,strlen(method)-10))
       '_' : mbbtemp(0) = float(strmid(method,19,strlen(method)-10))
       '-' : mbbtemp(0) = float(strmid(method,19,strlen(method)-10))
       else : mbbtemp(0) = float(strmid(method,18,strlen(method)-9))
      endcase
    endelse
    method = 'MODIFIED BLACKBODY'
    error,'D','METHOD '+method
    error,'D','BBTEMP '+string(mbbtemp(0))
  endif
  
END

PRO CORRECTION_Event, Event, method=method_in

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO CORRECTION_Event'

  correction = strupcase(strmid(Event.Value,11,strlen(Event.Value)-11))
  error,'D',correction
  if ( correction eq 'OFFSET' ) then offset = 1 else offset = 0
  yrange_cont = [BAD_RANGE,BAD_RANGE]
  widget_control,/hourglass
  apply_cont

END

PRO XRANGE0_Event, Event, xrange0=xrange0

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO XRANGE0_Event'

  if ( n_elements(xrange0) ne 0 ) then xrange(0) = xrange0 $
  else begin
    if ( strtrim(Event.value,2) eq '' ) then xrange(0) = BAD_RANGE $
    else begin
      xrange(0) = float(Event.value)
      index = where(aar_band.data.wave ge xrange(0) and $
                    aar_band.data.wave le xrange(1), count)
      if ( count gt 1 ) then begin
        yrange_sel=[min(aar_band.data(index).flux), $
                    max(aar_band.data(index).flux)]
        widget_control,widget_id.yrange0_sel,set_value=strtrim(yrange_sel(0),2)
        widget_control,widget_id.yrange1_sel,set_value=strtrim(yrange_sel(1),2)
      endif
    endelse
  endelse
  yrange_cont = 0
END

PRO XRANGE1_Event, Event, xrange1=xrange1

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO XRANGE1_Event'

  if ( n_elements(xrange1) ne 0 ) then xrange(1) = xrange1 $
  else begin
    if ( strtrim(Event.value,2) eq '' ) then xrange(1) = BAD_RANGE $
    else begin
      xrange(1) = float(Event.value)
      index = where(aar_band.data.wave ge xrange(0) and $
                    aar_band.data.wave le xrange(1), count)
      if ( count gt 1 ) then begin
        yrange_sel=[min(aar_band.data(index).flux), $
                    max(aar_band.data(index).flux)]
        widget_control,widget_id.yrange0_sel,set_value=strtrim(yrange_sel(0),2)
        widget_control,widget_id.yrange1_sel,set_value=strtrim(yrange_sel(1),2)
      endif
    endelse
  endelse
  yrange_cont = 0
END

PRO YRANGE0_SEL_Event, Event, yrange=yrange

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO YRANGE0_SEL_Event'

  if ( n_elements(yrange) eq 0 ) then begin
    if ( strtrim(Event.value,2) eq '' ) then yrange_sel(0) = BAD_RANGE $
    else yrange_sel(0) = float(Event.value)
  endif else yrange_sel(0) = yrange
END

PRO YRANGE1_SEL_Event, Event, yrange=yrange

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','YRANGE1_SEL_Event'

  if ( n_elements(yrange) eq 0 ) then begin
    if ( strtrim(Event.value,2) eq '' ) then yrange_sel(1) = BAD_RANGE $
    else yrange_sel(1) = float(Event.value)
  endif else yrange_sel(1) = yrange
END

PRO YRANGE0_CONT_Event, Event, yrange=yrange

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO YRANGE0_CONT_Event'

  if ( n_elements(yrange) eq 0 ) then begin
    if ( strtrim(Event.value,2) eq '' ) then yrange_cont(0) = BAD_RANGE $
    else yrange_cont(0) = float(Event.value)
  endif else yrange_cont(0) = yrange
END

PRO YRANGE1_CONT_Event, Event, yrange=yrange

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO YRANGE1_CONT_Event'

  if ( n_elements(yrange) eq 0 ) then begin
    if ( strtrim(Event.value,2) eq '' ) then yrange_cont(1) = BAD_RANGE $
    else yrange_cont(1) = float(Event.value)
  endif else yrange_cont(1) = yrange
END

PRO ADD_POINT, coord

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO ADD_POINT'

  if ( nselect eq 0 ) then begin
    wave_sel = coord(0)
    flux_sel = coord(1)
  endif else begin
    wave_sel = [wave_sel,coord(0)]
    flux_sel = [flux_sel,coord(1)]
    srt = sort(wave_sel)
    wave_sel = wave_sel(srt)
    flux_sel = flux_sel(srt)
  endelse
  nselect = nselect+1

  plots,coord(0),coord(1),col=2,ps=4,symsize=1.5

END

PRO REM_POINT, coord

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO REM_POINT'

  diff = abs(wave_sel - coord(0))
  index = where( diff eq min(diff), count )
  if ( count gt 0 ) then plots,wave_sel(index),flux_sel(index),col=3,ps=2
  index = where( diff ne min(diff), nselect )
  if ( nselect gt 0 ) then begin
    wave_sel = wave_sel(index)
    flux_sel = flux_sel(index)
  endif

END

PRO PLOT_SEL1_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

;  error,'D','PRO PLOT_SEL1_Event'

  if ( !d.window ne plot_index(0) ) then begin
    widget_control,/hourglass
    plot_data,plot=1
  endif
  coord = convert_coord(event.x,event.y,0.,/device,/to_data)
  case event.press of
   1 : add_point,coord
   2 : rem_point,coord
   4 : rem_point,coord
   else :
  endcase
  case event.release of
   2 : add_point,coord
   else : 
  endcase

  RETURN
END

PRO DETERMINE_RANGE, Event, sel=do_sel, cont=do_cont
 
  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  common sh_cf_temp  , x0, x1, y0, y1, coord_x, coord_y, old_mode

;  error,'D','PRO DETERMINE_RANGE'

  if ( n_elements(do_sel) eq 0 ) then do_sel = 0
  if ( n_elements(do_cont) eq 0 ) then do_cont = 0
;
; if it not and Y action (i.e. not using the middle mouse button
; then disable yrange changes
;
  if ( event.press ne 2 and event.release ne 2 ) then begin
    do_sel = 0
    do_cont = 0
  endif

  change_x = 0
  change_y = 0
  first_x = 0
  first_y = 0

  coord = convert_coord(event.x,event.y,0.,/device,/to_data)
  case event.press of
   0 : 
   1 : begin
         x0 = coord(0)
         x1 = 0
         first_x = 1
       end
   2 : begin
         y0 = coord(1)
         y1 = 0
         first_y = 1
       end
   else :
  endcase

  case event.release of
   0 :
   1 : begin
         x1 = coord(0)
         change_x = 1
       end
   2 : begin
         y1 = coord(1)
         change_y = 1
       end
   4 : begin
         widget_control,/hourglass
         apply_cont
       end
   else: return
  endcase

  if ( first_x ) then begin
    device, get_graphics = old_mode, set_graphics = 6
    plots,[coord(0),coord(0)],!y.crange,col=4
    plots,[coord(0),coord(0)],!y.crange,col=4
    wait,0.05
    coord_x = coord
  endif
  if ( first_y ) then begin
    device, get_graphics = old_mode, set_graphics = 6
    plots,!x.crange,[coord(1),coord(1)],col=4
    plots,!x.crange,[coord(1),coord(1)],col=4
    wait,0.05
    coord_y = coord
  endif

  if ( event.type eq 2 and n_elements(coord_x) ge 2 ) then begin
    plots,[coord_x(0),coord_x(0)],!y.crange,col=4
    empty
    plots,[coord(0),coord(0)],!y.crange,col=4
    coord_x = coord
    wait,0.05
  endif
  if ( event.type eq 2 and n_elements(coord_y) ge 2 ) then begin
    plots,!x.crange,[coord_y(1),coord_y(1)],col=4
    empty
    plots,!x.crange,[coord(1),coord(1)],col=4
    coord_y = coord
    wait,0.05
  endif
;
; plot the line and reset graphics mode
;
  if ( change_x ) then begin
    plots,[coord_x(0),coord_x(0)],!y.crange,col=4
    empty
    plots,[coord(0),coord(0)],!y.crange,col=4
    coord_x = 0
    device, set_graphics = old_mode
  endif
  if ( change_y ) then begin
    plots,!x.crange,[coord_y(1),coord_y(1)],col=4
    empty
    plots,!x.crange,[coord(1),coord(1)],col=4
    coord_y = 0
    device, set_graphics = old_mode
  endif
;
; set the xrange
;
  if ( change_x ) then begin
    if ( x0 eq x1 ) then xrange = [BAD_RANGE,BAD_RANGE] $
    else begin
      x = [min([x0,x1]),max([x0,x1])]
      xrange = [max([x(0),!x.crange(0)]),min([x(1),!x.crange(1)])]
    endelse
  endif  
;
; yrange
;
  if ( change_y ) then begin
    if ( y0 eq y1 ) then begin
      if ( do_cont ) then yrange_cont = [BAD_RANGE,BAD_RANGE]
      if ( do_sel  ) then yrange_sel  = [BAD_RANGE,BAD_RANGE]
    endif else begin
      y = [min([y0,y1]),max([y0,y1])]
      y = [max([y(0),!y.crange(0)]),min([y(1),!y.crange(1)])]
      if ( do_cont ) then yrange_cont = y
      if ( do_sel  ) then yrange_sel  = y
    endelse
  endif 

  widget_control,/hourglass
  if ( change_x or change_y ) then begin
    if ( do_sel or do_cont ) then begin
      if ( do_sel  ) then plot_data, plot=[1,2]
      if ( do_cont ) then plot_data, plot=[3,4]
    endif else plot_data
  endif
END

PRO PLOT_SEL2_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

;  error,'D','PRO PLOT_SEL2_Event'

  if ( (event.press eq 1 or event.press eq 2 or event.press eq 4) and $
       !d.window ne plot_index(1) ) then begin
    widget_control,/hourglass
    plot_data,plot=2
  endif
  determine_range,event,/sel
END

PRO PLOT_CONT1_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

;  error,'D','PRO PLOT_CONT1_Event'

  if ( (event.press eq 1 or event.press eq 2 or event.press eq 4) and $
       !d.window ne plot_index(2) ) then begin
    widget_control,/hourglass
    plot_data,plot=3
  endif
  determine_range,event,/cont
END

PRO PLOT_CONT2_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

;  error,'D','PRO PLOT_CONT2_Event'

  if ( (event.press eq 1 or event.press eq 2 or event.press eq 4) and $
       !d.window ne plot_index(3) ) then begin
    widget_control,/hourglass
    plot_data,plot=4
  endif
  determine_range,event,/cont
END

PRO OKEE_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO OKEE_Event'

  widget_control,widget_id.okee_base,/destroy
END

PRO OKEE_BASE_Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO OKEE_BASE_Event'

  widget_control,widget_id.okee_base,/destroy
END

PRO WIDGET_OKEE, text

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def,mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO WIDGET_OKEE'

  if ( n_elements(text) eq 0 ) then text=''

  widget_id.okee_base = widget_base(/column, $
                                    xoffset=300, $
                                    yoffset=300, $
                                    /map, $
                                    uvalue='OKEE_BASE')
  txt = widget_text(widget_id.okee_base,value=text)
  widget_id.okee = widget_button( widget_id.okee_base, $
                                  frame=2, $
                                  uvalue='OKEE', $
                                  value='  OKEE  ', $
                                  xsize=80)
  widget_control,widget_id.okee_base,/realize
  widget_control,widget_id.okee,/input_focus
  xmanager, 'OKEE', WIDGET_ID.OKEE_BASE
END

PRO APPLY_CONT

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset
  common sh_cf_extra , dofix, dopower

  error,'D','PRO APPLY_CONT'

  aot_band = aot_band(0)
  method = strupcase(method(0))
;
; first check if all ranges are defined
;
  check_range

  error,'D','NSELECT '+string(nselect)

  if ( nselect gt 0 ) then index = where(wave_sel gt 0., nselect)
  if ( nselect gt 0 ) then begin
    wave_sel = wave_sel(index)
    flux_sel = flux_sel(index)
    if ( nselect gt 1 ) then begin
      stat = chk_multix(wave_sel,flux_sel,make_array(nselect,value=1.),0.0,mean=0)
      nselect = n_elements(wave_sel)
    endif
  endif
  case nselect of
   0 : begin
         error,'W','NO CONTINUUM DATA'
         cont_flux = replicate(1.,n_elements(aar_band.data))
         goto,no_continuum
       end
   1 : cont_flux = replicate(flux_sel(0),n_elements(aar_band.data))
   else : begin
            case method of
             'SPLINE' : begin
                          tmp = nr_spline(wave_sel,flux_sel)
                          cont_flux = nr_splint(wave_sel,flux_sel, $
                                                tmp,aar_band.data.wave)
                        end
             'POLY'   : begin
                          case degree of
                           0 : cont_flux = replicate(flux_sel(index(0)), $
                                                     n_elements(aar_band.data))
                           1 : begin
                                    p = poly_fit(wave_sel,flux_sel,degree)
                                    cont_flux = poly(aar_band.data.wave,p)
                               end
                           else : begin
                                    flux_tmp = interpol(flux_sel,wave_sel, $
                                                        aar_band.data.wave)
                                    cont_flux = opfit_1d(flux_tmp,degree, $
                                                         aar_band.data.wave)
                                  end
                          endcase
                        end
             'POLYINT' : begin
                           nband     = n_elements(aar_band.data)
                           cont_flux = fltarr(nband)
                           for i=0L,nband-1 do begin
                             polint,wave_sel,flux_sel,aar_band.data(i).wave,tmp
                             cont_flux(i) = tmp
                           endfor
                         end
             'BLACKBODY' : begin
                             if ( n_elements(flux_sel) le 2) then begin
                               error,'W', $
                                 'Not enough data points to fit a black-body'+$
                                 ' need at least 5'
                               widget_okee, $
                                 'Not enough data points to fit a black-body'+$
                                 ' need at least 5'
                               cont_flux = replicate(1., $
                                                     n_elements(aar_band.data))
                               goto,no_continuum
                             endif else begin
                               cont_flux=bbfit(wave_sel,flux_sel,temp, $
                                           aar_band.data.wave,temp=bbtemp(0))
                               bbtemp(1) = temp
                             endelse
                           end
             'MODIFIED BLACKBODY' : begin
                             if ( n_elements(flux_sel) le 2) then begin
                               error,'W', $
                                 'Not enough data points to fit a black-body'+$
                                 ' need at least 5'
                               widget_okee, $
                                 'Not enough data points to fit a black-body'+$
                                 ' need at least 5'
                               cont_flux = replicate(1., $
                                                     n_elements(aar_band.data))
                               goto,no_continuum
                             endif else begin
                               cont_flux=sh_bbfit(wave_sel,flux_sel,temp, $
                                                  power, aar_band.data.wave, $
                                                  temp=mbbtemp(0),relerr=.1, $
                                                  power=dopower,fix=dofix)
                               mbbtemp(1) = temp
                             endelse
                           end
            endcase
          end
  endcase

  cont_band.data.flux = cont_flux
  cont_band.data.tint = 1
  cont.data(index_band).flux = cont_flux
  cont.data(index_band).tint = 1

  widget_control,/hourglass
  plot_data

  if ( aot_band ne '' ) then begin
    ador = aot_to_apband(aot_band)
    dets = band2det(aot_band)
  endif

  if ( nselect gt 0 ) then begin
    tmp_sel = sh_define_aar(length=nselect)
    i = indgen(nselect)
    if ( nselect eq 1 ) then i = i(0)
    tmp_sel.data(i).wave = wave_sel(i)
    tmp_sel.data(i).flux = flux_sel(i)
    tmp_sel.data(i).line = line(i)
    if ( aot_band ne '' ) then begin
      tmp_sel.data(i).det  = dets(0)
      tmp_sel.data(i).status = make_status(aper =ador(0))
      tmp_sel.data(i).flag   = make_flag  (order=ador(2))
    endif
    tmp_sel.data(i).tint = 1
  endif else tmp_sel = 0

  if ( is_aar(aar_sel) ) then begin
    bool = aar_sel.data.tint eq 0L
    index = where( bool, count)
    if ( count gt 0 ) then sel_tmp = select(aar_sel,bool) $
    else sel_tmp = 0
  endif
  if ( is_aar(sel_tmp) ) then begin
    if ( is_aar(tmp_sel) ) then aar_sel = combine(sel_tmp,tmp_sel) $
    else aar_sel = sel_tmp
  endif else aar_sel = tmp_sel

  no_continuum:

RETURN
END

PRO CHECK_RANGE

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO CHECK_RANGE'

  if ( xrange(0) eq BAD_RANGE ) then xrange(0) = min(aar_band.data.wave)
  if ( xrange(1) eq BAD_RANGE ) then xrange(1) = max(aar_band.data.wave)
;
; check the yranges
;
  if ( yrange_sel(0) eq BAD_RANGE ) then begin
    index = where( aar_band.data.wave ge xrange(0) and $
                   aar_band.data.wave le xrange(1), count )
    if ( count gt 0 ) then yrange_sel(0) = min(aar_band.data(index).flux) $
    else yrange_sel(0) = min(aar_band.data.flux)
  endif
  if ( yrange_sel(1) eq BAD_RANGE ) then begin
    index = where( aar_band.data.wave ge xrange(0) and $
                   aar_band.data.wave le xrange(1), count )
    if ( count gt 0 ) then yrange_sel(1) = max(aar_band.data(index).flux) $
    else yrange_sel(1) = max(aar_band.data.flux)
  endif
; 
; check the ranges for the continuum plots
;
  index = where(cont_band.data.tint eq 1, count)
  if ( count gt 0 ) then begin
    if ( n_elements(yrange_cont) ne 2 ) then yrange_cont = [BAD_RANGE,BAD_RANGE]
    if ( yrange_cont(0) eq BAD_RANGE ) then begin
      if ( offset ) then y = aar_band.data.flux - cont_band.data.flux $
      else y = aar_band.data.flux / cont_band.data.flux
      index = where( aar_band.data.wave ge xrange(0) and $
                     aar_band.data.wave le xrange(1), count )
      if ( count gt 0 ) then yrange_cont(0) = min(y(index)) $
      else yrange_cont(0) = min(y)
    endif
    if ( yrange_cont(1) eq BAD_RANGE ) then begin
      if ( offset ) then y = aar_band.data.flux - cont_band.data.flux $
      else y = aar_band.data.flux / cont_band.data.flux
      index = where( aar_band.data.wave ge xrange(0) and $
                     aar_band.data.wave le xrange(1), count )
      if ( count gt 0 ) then yrange_cont(1) = max(y(index)) $
      else yrange_cont(1) = max(y)
    endif
  endif else yrange_cont = [BAD_RANGE,BAD_RANGE]

  widget_control,widget_id.xrange0,set_value=strtrim(xrange(0),2)
  widget_control,widget_id.xrange1,set_value=strtrim(xrange(1),2)
  widget_control,widget_id.yrange0_sel,set_value=strtrim(yrange_sel(0),2)
  widget_control,widget_id.yrange1_sel,set_value=strtrim(yrange_sel(1),2)
  if ( yrange_cont(0) ne BAD_RANGE ) then $
    widget_control,widget_id.yrange0_cont,set_value=strtrim(yrange_cont(0),2)
  if ( yrange_cont(1) ne BAD_RANGE ) then $
    widget_control,widget_id.yrange1_cont,set_value=strtrim(yrange_cont(1),2)

END

PRO APPLY_Event, Event
  error,'D','PRO APPLY_Event'
  widget_control,/hourglass
  apply_cont
END

PRO CLEAR_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

  error,'D','PRO CLEAR_Event'

  wave_sel = 0.
  flux_sel = 0.
  nselect = 0
  widget_control,/hourglass
  plot_data
END

PRO MAIN_Event, Event

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset

;  error,'D','PRO MAIN_Event'

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

;  error,'D',Ev

  CASE Ev OF 
  'AOT_BAND'     : begin
                     line = 0
                     widget_control,/hourglass
                     AOT_BAND_Event, Event
                   end
  'LINE'         : begin
                     widget_control,/hourglass
                     LINE_Event, Event
                   end
  'METHOD'       : METHOD_Event, Event
  'CORRECTION'   : CORRECTION_Event, Event
  'XRANGE0'      : XRANGE0_Event, Event
  'XRANGE1'      : XRANGE1_Event, Event
  'YRANGE0_SEL'  : YRANGE0_SEL_Event, Event
  'YRANGE1_SEL'  : YRANGE1_SEL_Event, Event
  'YRANGE0_CONT' : YRANGE0_CONT_Event, Event
  'YRANGE1_CONT' : YRANGE1_CONT_Event, Event
  'CLEAR'        : CLEAR_Event, Event
  'APPLY'        : begin
                     widget_control,/hourglass
                     APPLY_Event, Event
                   end
  'EXIT'         : BEGIN
                     WIDGET_CONTROL,WIDGET_ID.MAIN,/DESTROY
                     return
                   END
  'PLOT_SEL1'    : PLOT_SEL1_Event, Event
  'PLOT_SEL2'    : PLOT_SEL2_Event, Event
  'PLOT_CONT1'   : PLOT_CONT1_Event, Event
  'PLOT_CONT2'   : PLOT_CONT2_Event, Event
  else : error,'W','UNKNOWN EVENT '+Ev
  ENDCASE


END

FUNCTION SH_CONT_FIT, aar_in, $
                      aar_sel_in, $
                      cont=cont_in, $
                      direct=direct, $
                      method=method_in, $
                      filename=filename, $
; (SH Mar 30 1999)
; neem parameters voor sh_cont over
                      fixed = fixed, $
                      power=power
    

  common sh_cf_aot   , aot_type, aot_band, line, object, band_def
  common sh_cf_data  , aar, aar_band, cont_band, index_band
  common sh_cf_cont  , cont, aar_sel, wave_sel, flux_sel, nselect
  ;mbbtemp added (SH Mar 24 1999)
  common sh_cf_fit   , method, degree, bbtemp, method_def, mbbtemp
  common sh_cf_plot  , plot_index, xrange, yrange_sel, yrange_cont
  common sh_cf_widget, widget_id, junk
  common sh_cf_par   , BAD_RANGE, offset
;(SH Mar 30 1999)
  common sh_cf_extra    , dofix, dopower
  if (n_elements(power) ne 0) then dopower = power else power =0d0
  if (n_elements(fixed) ne 0) then begin
    dofix = fixed(0)
    print,'USING FIXED EMMISIVITY PARAMETER: ',dopower
  endif else begin
    dofix=0
  endelse
  
  clean_comm

  programme = 'SH_CONT_FIT'
  version = 'V1.2'
  print,''
  error,'M',programme + '  ' + version
  print,''

  junk   = { CW_PDMENU_S, flags:0, name:'' }
  WIDGET_ID = { MAIN         : 0L, $
                BASE1        : 0L, $
                BASE2        : 0L, $
                BASE3        : 0L, $
                PLOT1        : 0L, $
                PLOT2        : 0L, $
                AOT_BAND     : 0L, $
                LINE         : 0L, $
                METHOD       : 0L, $
                CORRECTION   : 0L, $
                XRANGE0      : 0L, $
                XRANGE1      : 0L, $
                YRANGE0_SEL  : 0L, $
                YRANGE1_SEL  : 0L, $
                YRANGE0_CONT : 0L, $
                YRANGE1_CONT : 0L, $
                OFFSET       : 0L, $
                SCALING      : 0L, $
                CLEAR        : 0L, $
                APPLY        : 0L, $
                EXIT         : 0L, $
                PLOT_SEL1    : 0L, $
                PLOT_SEL2    : 0L, $
                PLOT_CONT1   : 0L, $
                PLOT_CONT2   : 0L, $
                OKEE_BASE    : 0L, $
                OKEE         : 0L }
;
; give info to the user and 
; get the input data
;
  object = ''
  if ( is_aar(aar_in) ) then begin
    aar = aar_in
    status = read_fits_key( aar.header, 'OBJECT', object )
    error,'M','OBJECT ' + object
    print,''
  endif else begin
    if ( n_elements(aar_in) ne 0 ) then begin
      error,'M','INPUT IS NOT AN AAR, ASSUME IT IS ASCII X,Y TABLE'
      aar = read_file(aar_in)
      if ( not is_aar(aar) ) then error,'F','ERROR READING INPUT FILE'
    endif else error,'F','NO INPUT'
  endelse
  aar_band = aar
;
; use selected datapoints for continuum if already present
;
  if ( is_aar(aar_sel_in) ) then begin
    aar_sel = aar_sel_in
  endif else begin
    if ( n_elements(aar_sel_in) ne 0 ) then begin
      error,'M','SELECT INPUT IS NOT AN AAR, ASSUME IT IS ASCII X,Y TABLE'
      aar_sel = read_file(aar_sel_in)
    endif
  endelse
;
; set the colors
;
  red  =[0,1,1,0,0,1]
  green=[0,1,0,1,0,1]
  blue =[0,1,0,0,1,0]
  tvlct,255*red, 255*green, 255*blue
;
; color=0 BLACK
; color=1 WHITE
; color=2 RED
; color=3 GREEN
; color=4 BLUE
; color=5 YELLOW
;
; initialise the method to use as default
;
  if ( n_elements(method_in) eq 0 ) then method_in = 'SPLINE'
  method_def = method_in
  METHOD_Event,Event,method=method_def
;
; band definitions
;  
  aot_type = get_aot_number(aar)
  grating = 0 & fp = 0
  if ( aot_type le 6 ) then grating = 1 else fp = 1
  band_def = define_aotband(grating=grating,fp=fp)
;
; initialize the continuum aar
;
  if ( is_aar(cont_in) ) then begin
    if (n_elements(cont.data) ne n_elements(aar.data) ) then begin
      cont = aar
      cont.data.flux = 1.
      cont.data.stdev = 0.
      cont.data.tint = 0
    endif else cont = cont_in
  endif else begin
    cont = aar
    cont.data.flux = 1.
    cont.data.stdev = 0.
    cont.data.tint = 0
  endelse
  cont_band = cont
;
; set up the widget environment
;
  WIDGET_ID.MAIN = WIDGET_BASE(  COLUMN=1, MAP=1, $
                                 XOFFSET=100, YOFFSET=50, $
                                 TITLE='SH_CONT_FIT', $
                                 UVALUE='MAIN')
  WIDGET_ID.BASE1 = WIDGET_BASE( WIDGET_ID.MAIN, $
                                 ROW=1, MAP=1, SPACE=15, $
                                 UVALUE='BASE1')
  WIDGET_ID.BASE2 = WIDGET_BASE( WIDGET_ID.MAIN, $
                                 ROW=1, MAP=1, SPACE=15, $
                                 UVALUE='BASE2')
  WIDGET_ID.PLOT1 = WIDGET_BASE( WIDGET_ID.MAIN, $
                                 ROW=1, MAP=1, $
                                 UVALUE='BASE1')
  WIDGET_ID.BASE3 = WIDGET_BASE( WIDGET_ID.MAIN, $
                                 ROW=1, MAP=1, $
                                 UVALUE='BASE3')
  WIDGET_ID.PLOT2 = WIDGET_BASE( WIDGET_ID.MAIN, $
                                 ROW=1, MAP=1, $
                                 UVALUE='BASE1')
;
; Build up the AOT_BAND menu only of the bands 
; which are present in the AAR
;
  aotbands = ['1a','1b','1d','1e','2a','2b','2c', $
              '3a','3c','3d','3e','4', $
              '5a','5b','5c','5d','6']
  MenuAOTband = [ { CW_PDMENU_S, 3, 'AOT_BAND' } ]
  for i=0,n_elements(aotbands)-1 do begin
    band = aotbands(i)
    ador = aot_to_apband(band)
    dets = band2det(ador(1))
    index = where( aar.data.det ge min(dets) AND $
                   aar.data.det le max(dets) AND $
                   test_status(aar,aper=ador(0)) AND $
                   test_flag(aar.data.flag,order=ador(2)), count )
    if ( count gt 0 ) then MenuAOTband = [MenuAOTband,{CW_PDMENU_S,0,band}]
  endfor
  MenuAOTband = [ MenuAOTband, { CW_PDMENU_S, 2, 'ALL'} ]
;
; Menu for the different methods the user can select
;
  MenuMethod = [ $
      { CW_PDMENU_S,       3, 'METHOD' }, $
        { CW_PDMENU_S,       0, 'DEFAULT' }, $
        { CW_PDMENU_S,       0, 'SPLINE' }, $
        { CW_PDMENU_S,       1, 'POLY' }, $
          { CW_PDMENU_S,       0, '1' }, $
          { CW_PDMENU_S,       0, '2' }, $
          { CW_PDMENU_S,       0, '3' }, $
          { CW_PDMENU_S,       0, '4' }, $
          { CW_PDMENU_S,       0, '5' }, $
          { CW_PDMENU_S,       0, '6' }, $
          { CW_PDMENU_S,       0, '7' }, $
          { CW_PDMENU_S,       0, '8' }, $
          { CW_PDMENU_S,       0, '9' }, $
          { CW_PDMENU_S,       2, '10' }, $
        { CW_PDMENU_S,       0, 'POLYINT' }, $
        { CW_PDMENU_S,       1, 'BLACKBODY' }, $
          { CW_PDMENU_S,       0, '10'   }, $
          { CW_PDMENU_S,       0, '50'   }, $
          { CW_PDMENU_S,       0, '100'  }, $
          { CW_PDMENU_S,       0, '250'  }, $
          { CW_PDMENU_S,       0, '500'  }, $
          { CW_PDMENU_S,       0, '1000' }, $ 
          { CW_PDMENU_S,       0, '2000' }, $ 
          { CW_PDMENU_S,       2, '10000' }, $ 
        { CW_PDMENU_S,       1, 'MODIFIED BLACKBODY' }, $
          { CW_PDMENU_S,       0, '10'   }, $
          { CW_PDMENU_S,       0, '50'   }, $
          { CW_PDMENU_S,       0, '100'  }, $
          { CW_PDMENU_S,       0, '250'  }, $
          { CW_PDMENU_S,       0, '500'  }, $
          { CW_PDMENU_S,       0, '1000'  }, $
          { CW_PDMENU_S,       0, '2000'  }, $
          { CW_PDMENU_S,       2, '10000' } ]

  MenuCorrection = [ $
      { CW_PDMENU_S,       3, 'CORRECTION' }, $
        { CW_PDMENU_S,       0, 'OFFSET' }, $
        { CW_PDMENU_S,       2, 'SCALING' } ]
;
; fill in BASE1, the main
;
  WIDGET_ID.CLEAR = WIDGET_BUTTON( WIDGET_ID.BASE1, $
                                   FRAME=2, $
                                   UVALUE='CLEAR', $
                                   VALUE='  CLEAR  ', $
                                   XSIZE=80)
  WIDGET_ID.APPLY = WIDGET_BUTTON( WIDGET_ID.BASE1, $
                                   FRAME=2, $
                                   UVALUE='APPLY', $
                                   VALUE='  APPLY  ', $
                                   XSIZE=80)
  WIDGET_ID.EXIT  = WIDGET_BUTTON( WIDGET_ID.BASE1, $
                                   FRAME=2, $
                                   UVALUE='EXIT', $
                                   VALUE='  EXIT  ', $
                                   XSIZE=80)
;
; BASE2 with the menu selection buttons
;
  WIDGET_ID.METHOD     = CW_PDMENU( WIDGET_ID.BASE2, $
                                    MenuMethod, $
                                    /RETURN_FULL_NAME, $
                                    UVALUE='METHOD')
  WIDGET_ID.CORRECTION = CW_PDMENU( WIDGET_ID.BASE2, $
                                    MenuCorrection, $
                                    /RETURN_FULL_NAME, $
                                    UVALUE='CORRECTION')
  WIDGET_ID.AOT_BAND   = CW_PDMENU( WIDGET_ID.BASE2, $
                                    MenuAOTband, $
                                    /RETURN_FULL_NAME, $
                                    UVALUE='AOT_BAND')
;
; we add the LINE menu later with the first AOT_BAND_Event call
;
;  select_lines, menu=MenuLINE
;  line = 0
;  WIDGET_ID.LINE = CW_PDMENU( WIDGET_ID.BASE2, $
;                              MenuLINE, $
;                              /RETURN_FULL_NAME, $
;                              UVALUE='LINE')
;
;
; The selection plots
;
  WIDGET_ID.PLOT_SEL1 = WIDGET_DRAW( WIDGET_ID.PLOT1, $
                                     BUTTON_EVENTS=1, $
                                     MOTION_EVENTS=0, $
                                     RETAIN=1, $
                                     UVALUE='PLOT_SEL1', $
                                     XSIZE=650, $
                                     YSIZE=300)
  WIDGET_ID.PLOT_SEL2 = WIDGET_DRAW( WIDGET_ID.PLOT1, $
                                     BUTTON_EVENTS=1, $
                                     MOTION_EVENTS=1, $
                                     RETAIN=1, $
                                     UVALUE='PLOT_SEL2', $
                                     XSIZE=300, $
                                     YSIZE=300)
;
; the range widgets
;
  WIDGET_ID.XRANGE0      = CW_FIELD( WIDGET_ID.BASE3, $
                                     VALUE=strtrim(xrange(0),2), $
                                     ROW=1, $
                                     /STRING, $
                                     ALL_EVENTS=1, $
                                     TITLE='XRANGE', $
                                     UVALUE='XRANGE0', $
                                     XSIZE=8)
  WIDGET_ID.XRANGE1      = CW_FIELD( WIDGET_ID.BASE3, $
                                     VALUE=strtrim(xrange(1),2), $
                                     ROW=1, $
                                     /STRING, $
                                     ALL_EVENTS=1, $
                                     TITLE=' ', $
                                     UVALUE='XRANGE1', $
                                     XSIZE=8)
  WIDGET_ID.YRANGE0_SEL  = CW_FIELD( WIDGET_ID.BASE3, $
                                     VALUE=strtrim(yrange_sel(0),2), $
                                     ROW=1, $
                                     /STRING, $
                                     ALL_EVENTS=1, $
                                     TITLE='YRANGE SELECTION', $
                                     UVALUE='YRANGE0_SEL', $
                                     XSIZE=8)
  WIDGET_ID.YRANGE1_SEL  = CW_FIELD( WIDGET_ID.BASE3, $
                                     VALUE=strtrim(yrange_sel(1),2), $
                                     ROW=1, $
                                     /STRING, $
                                     ALL_EVENTS=1, $
                                     TITLE=' ', $
                                     UVALUE='YRANGE1_SEL', $
                                     XSIZE=8)
  WIDGET_ID.YRANGE0_CONT = CW_FIELD( WIDGET_ID.BASE3, $
                                     VALUE=strtrim(yrange_cont(0),2), $
                                     ROW=1, $
                                     /STRING, $
                                     ALL_EVENTS=1, $
                                     TITLE='YRANGE CONTINUUM', $
                                     UVALUE='YRANGE0_CONT', $
                                     XSIZE=8)
  WIDGET_ID.YRANGE1_CONT = CW_FIELD( WIDGET_ID.BASE3, $
                                     VALUE=strtrim(yrange_cont(1),2), $
                                     ROW=1, $
                                     /STRING, $
                                     ALL_EVENTS=1, $
                                     TITLE=' ', $
                                     UVALUE='YRANGE1_CONT', $
                                     XSIZE=8)
;
; the continuum (corrected) plots
;
  WIDGET_ID.PLOT_CONT1 = WIDGET_DRAW( WIDGET_ID.PLOT2, $
                                      BUTTON_EVENTS=1, $
                                      MOTION_EVENTS=1, $
                                      RETAIN=1, $
                                      UVALUE='PLOT_CONT1', $
                                      XSIZE=650, $
                                      YSIZE=300)
  WIDGET_ID.PLOT_CONT2 = WIDGET_DRAW( WIDGET_ID.PLOT2, $
                                      BUTTON_EVENTS=1, $
                                      MOTION_EVENTS=1, $
                                      RETAIN=1, $
                                      UVALUE='PLOT_CONT2', $
                                      XSIZE=300, $
                                      YSIZE=300)
;
; Okee now build up the widget
;
  WIDGET_CONTROL, WIDGET_ID.MAIN, /REALIZE
;
; Get drawable window index
;
  WIDGET_CONTROL, WIDGET_ID.PLOT_SEL1 , GET_VALUE=INDEX
  plot_index(0) = INDEX
  WIDGET_CONTROL, WIDGET_ID.PLOT_SEL2 , GET_VALUE=INDEX
  plot_index(1) = INDEX
  WIDGET_CONTROL, WIDGET_ID.PLOT_CONT1, GET_VALUE=INDEX
  plot_index(2) = INDEX
  WIDGET_CONTROL, WIDGET_ID.PLOT_CONT2, GET_VALUE=INDEX
  plot_index(3) = INDEX
;
; get the data, lines and plots
;
  widget_control,/hourglass
  AOT_BAND_Event
;
; manage it
;
  XMANAGER, 'MAIN', WIDGET_ID.MAIN
;
; write continuum data to file
;
  if ( n_elements(filename) ne 0 ) then begin
    error,'M','WRITING CONTINUUM DATA TO ASCII TEXT FILE'
    filename = string(filename)
    openw,unit,filename,/get_lun
    index = where(cont.data.tint eq 1, count)
    x = ' '
    for i=0,count-1 do begin
      printf,unit,aar.data(index(i)).wave, x, $
                  aar.data(index(i)).flux, x, $
                  aar.data(index(i)).stdev, x, $
                  cont.data(index(i)).flux, x, $
                  cont.data(index(i)).line, $
                  format='(F12.5,A1,F12.5,A1,F12.5,A1,F12.5,A1,I6)'
    endfor
    close,unit
  endif
;
; check if aar_sel is aar or file
; 
  if ( n_elements(aar_sel_in) ne 0 and $
       not is_aar(aar_sel_in) and $
       not is_aar(aar_in) and $
       is_aar(aar_sel) ) then begin
    error,'M','WRITING SELECT DATA TO ASCII TEST FILE'
    aar_sel_in = string(aar_sel_in)
    openw,unit,aar_sel_in,/get_lun
    x = ' '
    for i=0, n_elements(aar_sel.data)-1 do begin
      printf,unit,aar_sel.data(i).wave, x, $
                  aar_sel.data(i).flux, x, $
                  aar_sel.data(i).line, $
                  format='(F12.5,A1,F12.5,A1,I6)'
    endfor
    close,unit
  endif else begin
    if ( is_aar(aar_sel) ) then begin
      aar_sel_in = aar_sel
      aar_sel_in.data.tint = 0
    endif
  endelse
;
; clean up some common variables
;
  clean_comm

  return, cont
END
