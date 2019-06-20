;; simple routine to plot a fit from a given pixel in a fitting
;; structure

pro plf,s,x,y, $
        component=component, $
        noobserved=noobserved, $
        nomodel=nomodel, $
        error=error, $
        inspect=inspect, $
        _extra=_extra
  
  default,x,0
  default,y,0
  default,component,'tot'
  default,noobserved,0
  default,nomodel,0
  default,error,0
  default,inspect,0
  
  if inspect ne 0 then begin
     summed = total(s.fnuobs,3)
     nx = n_elements(summed[*,0])
     ny = n_elements(summed[0,*])
     
     for xx=0,nx-1 do begin
        for yy=0,ny-1 do begin
           if summed[xx,yy] ne 0d0 then begin
              plf,s,xx,yy, $
                  component=component, $
                  noobserved=noobserved, $
                  error=error, $
                  nomodel=nomodel, $
                  _extra=_extra,title='x:'+string(xx)+',y:'+string(yy)

              ;; want to pause before showing the next in interactive mode
              if !d.name eq 'X' then begin
                 print,'press enter to continue'
                 a=''
                 read,a
              endif

           endif
        endfor
     endfor
     return
  endif
  
  obs=transpose([[s.wavobs],[reform(s.fnuobs[x,y,*])],[reform(s.dfnuobs[x,y,*])]])

  shouldoplotmodel=0
  if noobserved eq 0 then begin
     pl,obs,error=error,_extra=_extra
     shouldoplotmodel=1
  endif

  if nomodel eq 0 then begin

     idx_plateaux = where(stregex( s.bands.label,'plateau', /boolean,/fold_case ),complement=idx_sharp)
  
     xmod = reform(s.fit.w)
     
     cmps=strlowcase(component) 

     for i=0,n_elements(cmps)-1 do begin
        cmp = cmps[i]
        
        ;; subsequent components should always be overplotted
        if i eq 1 then shouldoplotmodel=1

        case 1 of
           (cmp eq 'bb') or (cmp eq 'blackbodies') or (cmp eq 'blackbody')or (cmp eq 'cont')or (cmp eq 'continuum'):begin
              ymod=total(reform(s.fit.fnu_bb[x,y,*,*]),1)
           end
           (cmp eq 'star') or (cmp eq 'stellar'):begin
              ymod=reform(s.fit.fnu_star[x,y,*])
           end
           (cmp eq 'line') or (cmp eq 'lines'):begin
              ymod=reform(s.fit.fnu_line[x,y,*])
           end
           (cmp eq 'pahs') or (cmp eq 'bands') or (cmp eq 'features'):begin
              ymod=total(reform(s.fit.fnu_band[x,y,*,*]),1)
           end
           (cmp eq 'sharp'):begin
              ymod=total(reform(s.fit.fnu_band[x,y,idx_sharp,*]),1)
           end
           (cmp eq 'plateaux') or (cmp eq 'plateau'):begin
              ymod=total(reform(s.fit.fnu_band[x,y,idx_plateaux,*]),1)
           end
           else:begin
              ymod=reform(s.fit.fnu_tot[x,y,*])
           end
        endcase
     
        mdl = transpose([[xmod],[ymod]])
        
        pl,mdl,oplot=shouldoplotmodel,_extra=_extra
        
     endfor

  endif

end
