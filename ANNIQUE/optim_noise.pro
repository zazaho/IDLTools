;; Simply loop trough the smoothing to find the optimal value
FUNCTION optim_noise,aar,cont_range=xr,srange=srange,step=step, $
                     moments=moments,order=ord,_extra=_extra
  
  default,ord,1
  default,xr,[0,200]
  default,srange,[1,100]
  default,step,3
  
  ;; the actual values for the smoothing
  smo_pts = srange[0]+step*indgen((srange[1]-srange[0]+1)/step)
  nsmo = n_elements(smo_pts)
  ;; create a structure to hold the moment data for the continuum
  moments    = dindgen(nsmo,4)
  moments_up = dindgen(nsmo,4)
  moments_dw = dindgen(nsmo,4)
  
  ;; indice for the continuum
  cont_points = (aar.data.wave GE xr[0]) AND (aar.data.wave LE xr[1])
  idx_cont = where(cont_points)
;  idx_cont_up = where(cont_points AND aar.data.sdir EQ -1)
;  idx_cont_dw = where(cont_points AND aar.data.sdir EQ  1)
  
  ;; romve a 1 order poly of this continuum
  w = aar.data[idx_cont].wave
  f = aar.data[idx_cont].flux
  f = f[sort(w)]
  w = w[sort(w)]
  fit = poly_fit(w,f,ord,yfit)
  yfit[sort(w)] = yfit
  
  FOR i = 0,nsmo-1 DO BEGIN
    tmp = sh_noisefilter(aar,smo=smo_pts[i],_extra=_extra)
    moments[i,*]    = moment(tmp.data[idx_cont   ].flux-yfit)
;    moments_up[i,*] = moment(tmp.data[idx_cont_up].flux)
;    moments_dw[i,*] = moment(tmp.data[idx_cont_dw].flux)
  ENDFOR
  plot,smo_pts,moments[*,1],/ystyle
  
;  oplot,smo_pts,moments_up[*,1],col=50
;  oplot,smo_pts,moments_dw[*,1],col=100
  ;; This needs to be the optimal
  tmp = min(moments[*,1],idx_opt)
  
  return,sh_noisefilter(aar,smo=smo_pts[idx_opt])
END
