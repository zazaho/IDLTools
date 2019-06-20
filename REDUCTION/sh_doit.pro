; S Hony (SH Jul 21 1998)
; Do a standard reduction of aar
; Useage out = sh_doit(aar,band)
; Loop through:
;a2 = sigclip(a,sig=5)
;f1 = sws_flatfield(a2,sig=sigma,scaling=scaling,clip=2)
;s1 = sigclip(f1,sig=sigma,nit=2)
;r1 = sws_rebin(s1,res=resol/4)
;f2 = sws_flatfield(a2,sig=sigma,ref=r1,scaling=scaling,order,clip=2)
;s2 = sigclip(f2,sig=sigma,nit=3)
;r2 = sws_rebin(s2,met=method,weight=weight)
;return, r2

function my_flatfield,a,o,scaling=scaling,_extra=_extra
  if (keyword_set(scaling) and (scaling ne 0)) then begin
    return,sws_flatfield(a,o,/scaling,_extra=_extra)
  endif else begin
    return,sws_flatfield(a,o,_extra=_extra)
  endelse
end

function sh_doit,aar,aband,resol=resol, $
                 sigma=sigma,method=method, $
                 ffordr=ffordr,weight=weight,scaling=scaling

  a=aar
;a = get_aotband(aar,aband)
;a = cut_aot_bands(a)
  if not keyword_set(resol) then begin
    midwave = (a.data(n_elements(a.data)-1).wave + a.data(0).wave)/2.  
    resol = resolution(midwave,aband)
  endif

  if not keyword_set(sigma) then begin
    sigma = 2.
  endif

  if not keyword_set(method) then begin
    method = 'fluxcon'
  endif

  if not keyword_set(weight) then begin
    weight = 's'
  endif

  if not keyword_set(ffordr) then begin
    ffordr = 0
  endif

  if not keyword_set(scaling) then begin
    scaling=0
  endif
  
  a2 = sigclip(a,sig=5)

  f1 = my_flatfield(a2,scaling=scaling,clip=2,/noupdown)
  s1 = sigclip(f1,sig=sigma,nit=2)
  r1 = sws_rebin(s1,res=resol/4)
  
  f2 = my_flatfield(a2,ffordr,ref=r1,scaling=scaling,clip=2)
  s2 = sigclip(f2,sig=sigma,nit=3)
  r2 = sws_rebin(s2,met=method,weight=weight)

  pl,r2
  pl,a2,/o,ps=3
  print,'<-> FLATFIELD  order: ',ffordr
  print,'<-> SIGCLIP:   sigma: ',sigma
  print,'<-> REBIN:resolution: ',resol(0)
  print,'<-> REBIN:   method: ',method
  return,r2
end 

