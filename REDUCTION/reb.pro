;(SH Dec  3 1998)
function reb,inaar,inres,median=median,mean=mean,md=md,mn=mn,wmean=wmean $
             ,ref=ref,_extra=_extra
  mymeth = 'fluxcon'
  if (keyword_set(median) or keyword_set(md)) then mymeth='median' 
  if (keyword_set(mean) or keyword_set(mn)) then mymeth='mean' 
  if (keyword_set(wmean)) then mymeth='wmean' 
  IF is_aar(ref) THEN BEGIN
      if (n_params() eq 2) then begin
          return,sws_rebin(inaar,ref,res=inres,over=2,met=mymeth,_extra=_extra)
      end else begin
          return,sws_rebin(inaar,ref,met=mymeth,over=2,_extra=_extra)
      ENDELSE
  ENDIF ELSE BEGIN
      if (n_params() eq 2) then begin
          return,sws_rebin(inaar,res=inres,over=2,met=mymeth,_extra=_extra)
      end else begin
          return,sws_rebin(inaar,met=mymeth,over=2,_extra=_extra)
      ENDELSE
  ENDELSE
      
END

