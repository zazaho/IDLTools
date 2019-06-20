; S Hony
; function to select range from an aar
; optionally also deselects range
; usage:
; outaar = sh_select_range,inaar,xrange=xrange,/deselect,/help
function sh_select_range,in,xrange=xrange,deselect=deselect,help=help,$
                         screen=screen,quiet=quiet,_extra=_extra
  
; no parameters or help requested
  if ((n_params() eq 0) or keyword_set(help)) then begin
    print,'function sh_select_range:'
    print,'usage:'
    print,'outaar = sh_select_range,inaar,xrange=xrange,/deselect,', $
      '/screen,/quiet,/help'
    print,'inaar: aar to select from'
    print,'xrange: range to select'
    print,'/deselect: exclude selected range from aar'
    print,'/screen: indicate range in the current plotscreen'
    print,'/quiet: do not ask for confirmation'
    print,'/help provides this help'
    return,in
  endif
  
; initialise
  answer = ''

; Check for valid input
  if not is_aar(in) then error,'F','No valid AAR structure specified!'
  
; always work with a new structure 'cos old structure is by reference
  tmp = in
  
  loud = not keyword_set(quiet)
  
  if keyword_set(screen) then begin
    print,'select the limits for selection'
    wait,0.5
    veri,x1,/nostatus
    wait,0.5
    veri,x2,/nostatus
    xrange=[x1,x2]
  endif

  if not keyword_set(xrange) then begin
    
    jump1:
    pl, tmp, tit='sh select range',_EXTRA = _extra
    print,'select the limits for selection'
    print,'select for upper then lower to zoom'
    wait,0.5
    veri,x1,/nostatus
    wait,0.5
    veri,x2,/nostatus
    
; Zoom selected
    if (x1 gt x2) then begin
      print,'zooming between: ',x2,' and ',x1
      tmp = sh_select(tmp,(tmp.data.wave gt x2) AND (tmp.data.wave lt x1))
      goto,jump1
    endif
; Xranges determined    
    xrange = [x1,x2]
  endif
  
; sort the xranges
  xrange = xrange(sort(xrange))
  
; now select the requested part
; do we want the selected part or the all but the selected part
  if keyword_set(deselect) then begin
    if (loud) then print,'will exclude: ',xrange(0),' to ',xrange(1)
    if keyword_set(quiet) then begin
      return,sh_select(in,(in.data.wave lt xrange(0)) or $ 
                    (in.data.wave gt xrange(1)))
    endif else begin
      print,'are you sure (y[es])'
      read,answer
      if (strmid(strlowcase(answer),0,1) eq 'y') then begin
        return,sh_select(in,(in.data.wave lt xrange(0)) or $ 
                      (in.data.wave gt xrange(1)))
      endif
    endelse
  endif else begin    
    if (loud) then print,'will keep: ',xrange(0),' to ',xrange(1)
    if keyword_set(quiet) then begin
      return,sh_select(in,(in.data.wave gt xrange(0)) and $ 
                    (in.data.wave lt xrange(1)))
    endif else begin
      print,'are you sure (y[es])'
      read,answer
      if (strmid(strlowcase(answer),0,1) eq 'y') then begin
        return,sh_select(in,(in.data.wave gt xrange(0)) and $ 
                      (in.data.wave lt xrange(1)))
      endif
    endelse
  endelse
  
; ask to try again
  print,'Try again? (y[es])'
  read,answer
  if (strmid(strlowcase(answer),0,1) eq 'y') then begin
    tmp = in
    goto,jump1
  endif
  
; user has aborted,return in structure
  return,in
end
