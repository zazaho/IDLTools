;+
; NAME: sh_convgg
;
; PURPOSE: calculate the observed width from line of given width after
; convolution with gaussian instrumental-profile 
;
; CALLING SEQUENCE:
; ratios = sh_convgg()
;      
; KEYWORD PARAMETERS:
; lambda=lambda, central wavelength around which to calculate lineprofile 
; xrange=xrange, wavelength range for which to calcualte the lineproflie
; rp=rp, instrumental resolving power
; nwidth=nwidth, number of different widths to calculate 
; wrange=wrange, range of width-ratios to do the calculations for
; plot=plot, plot the results if 1
; nlambda=nlambda, number of wavelength points
;
; OUTPUTS:
; result = replicate({inratio:0d0,outratio:0d0},nwidth)
;
; EXAMPLE:
; ratios = sh_convgg(nw=100,wr=[1d-1,3d0],lambda=1d1,xr=[9.5,10.5],/pl)
;
; MODIFICATION HISTORY:
;(SH Dec 31 1998) version 1.0
;-

function sh_convgg,lambda=lambda,xrange=xrange,rp=rp,nwidth=nwidth, $
                   wrange=wrange,plot=plot,nlambda=nlambda,help=help
  
; show the usage help
  if keyword_set(help) then begin
    print,'ratios = sh_convgg()'
    print,'     '
    print,'KEYWORD PARAMETERS:'
    print,'lambda=lambda, central wavelength of line'
    print,'xrange=xrange, wavelength range for which' + $
      'to calcualte the lineproflie'
    print,'rp=rp, instrumental resolving power'
    print,'nwidth=nwidth, number of different widths to calculate '
    print,'wrange=wrange, range of width-ratios to do the calculations for'
    print,'plot=plot, plot the results if 1'
    print,'nlambda=nlambda, number of wavelength points'
    return,0
  endif
  
    
; The central wavelength of the line
  if not keyword_set(lambda) then begin
    lambda = 3d0
  endif
  
; The wavelengthlimits 
  if not keyword_set(xrange) then begin
    xrange = [lambda-5d-3*lambda,lambda+5d-3*lambda]
  endif
  
; The number wavelength points
  if not keyword_set(nlambda) then begin
    nlambda = 501
  endif
  dnlambda = double(nlambda)
  
; The instrumental resolving power
  if not keyword_set(rp) then begin
    rp = 1500d0
  endif
  
; The number of different widths to calculate
  if not keyword_set(nwidth) then begin
    nwidth = 21
  endif
  dnwidth = double(nwidth)
  
; The range of widths to calculate
  if not keyword_set(wrange) then begin
    wrange = [5d-1,3d0]
  endif
  
;plot the profiles 1=yes
  if not keyword_set(plot) then begin
    plot = 0
  endif
  
;Initialize
  A = STRING(0)
  
;Make a wavelength grid
  wv = xrange(0)+(xrange(1)-xrange(0))/(dnlambda-1d0)*dindgen(nlambda)
;Make a structure to hold the results
  result = replicate({inratio:0d0,outratio:0d0,FWHM:0d0},nwidth)
  
;Calculate the width parameter of the instrumental profile: 
; R = RP*2*sqrt(ln(2)/lambda)
  R = RP*2d0*double(sqrt(alog(2d0)))/lambda
  
; Iterate through the requested width-ratios
  For i = 0,nwidth-1 do begin
    w = R/(wrange(0)+double(i)*(wrange(1)-wrange(0))/(dnwidth-1d0))
    
; The resultant profile is given by the following function of R and w 
; fx=R/sqrt(R^2+w^2)*
;    exp( -R^2*lambda^2-w^2*wv^2+(R^2*lambda+w^2*wv)^2/(R^2+w^2) )
    fx = R/sqrt(R^2d0+w^2d0)* $
      exp( -1d0*(R*lambda)^2d0 $
           -1d0*(w*wv)^2d0 $
           + (R^2d0*lambda + w^2d0*wv)^2d0/(R^2d0+w^2d0))
; The original line flux is given by :
; linefx = exp(-1d0*(w(wv-lambda))^2d0)
    linefx = exp(-1d0*(w*(wv-lambda))^2d0)
    
    fit = gaussfit(wv,fx,fitparam,nterms=3)
    
    if (plot eq 1) then begin    
      plot,wv,linefx,/xs,/ys
      oplot,wv,fx,col=150
      oplot,wv,fit,col=100,ps=1
      print,'Any key to continue; "n/N/0" to stop plotting'
      READ,A
      if ((A eq 'n') or (A eq 'N') or (A eq '0')) then plot = 0
    endif
    
    infwhm = R/w
    outfwhm = R*fitparam(2)*sqrt(2d0)
    result(i).inratio = infwhm
    result(i).outratio = outfwhm
    
  endfor
  return,result
end
