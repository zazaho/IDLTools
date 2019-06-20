function sh_gauss,ref,x,y,w,center=center,height=height,width=width, $
                  janksy=jansky
  case n_params() of
    1: begin
      if not (keyword_set(center) and keyword_set(height) and $
              keyword_set(width)) then goto,help
      x=center
      y=height
      w=width
    end
    2: begin
      if not (keyword_set(height) and keyword_set(width)) $
        then goto,help
      y=height
      w=width
      
    end
    3: begin
      if not (keyword_set(width)) $
        then goto,help
      w=width
    end
    4: begin
    end
  endcase
  
    out = ref
    wv = out.data.wave
;(SH Feb 16 1999)
;dont calculate too far away from the center because this is nonsense
;and will issues all these warnings
;    stop
    out.data.flux = 0d0
    ix = where((wv gt (x-1d1*w)) and (wv lt (x+1d1*w)))
;    print,n_elements(wv),min(ix),max(ix)
    wv = wv(ix)
;    stop
    out.data(ix).flux = y*exp(-1d0*(((x-wv)/w)^2d0)/2d0)
    if not (keyword_set(jansky)) then out = sh_calcaar(out,fl=-1)
    return,out

    help:
    print,'Usage:'
    print,'out = sh_gauss(ref,[x|center=value],[y|height=value],'+ $
      '[w,width=value],/jansky'
    print,'where'
    print,'x= center position'
    print,'y= gauss height'
    print,'w= gauss width'
    print,'/jansky height is in janskies otherwise it is f_lambda'
    return,0
end
