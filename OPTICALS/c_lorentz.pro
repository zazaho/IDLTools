;; FeS2:
;; osc=[[293,348,402,415],[0.0258,0.065,0.27,0.022],[0.005,0.0056,0.013,0.02]]
;; eps_inf = 21.32
;; eps_0 = 26.13
;; fes2 = c_lorentz(osc,eps_inf,/aar)
FUNCTION c_lorentz,osc,eps_inf,R=R,xrange=xr,step=step,aar=aar,n=n,k=k,l=l
  
  default,xr,[1,100]
  default,step,.1
  default,R,1
  
  wavelen = xr[0]+dindgen((xr[1]-xr[0])/step+1)*step
  wavenum = 1d4/wavelen

  nosc = n_elements(osc[*,0])
  ome = reform(osc[*,0],nosc)
  rho = reform(osc[*,1],nosc)
  gam = reform(osc[*,2],nosc)

  eps_1 = eps_inf
  eps_2 = 0d0

  FOR j=0,nosc-1 DO BEGIN
      eps_1 = eps_1+(4d0*!pi*rho[j]*ome[j]^2d0*(ome[j]^2d0-wavenum^2d0))/ $
        ((ome[j]^2d0-wavenum^2d0)^2d0+(gam[j]*ome[j]*wavenum)^2d0)
      eps_2 = eps_2+4d0*!pi*rho[j]*ome[j]^3d0*gam[j]*wavenum/ $
        ((ome[j]^2d0-wavenum^2d0)^2d0+(gam[j]*ome[j]*wavenum)^2d0)
  ENDFOR 

  n = sqrt((sqrt(eps_1^2d0+eps_2^2d0)+eps_1)/2)
  k = sqrt((sqrt(eps_1^2d0+eps_2^2d0)-eps_1)/2)
  l = wavelen

  eps = complex(eps_1,eps_2)
  a = 4d0*!pi*R*(eps-1)/(eps+2)
  c_abs = 2d0*!pi/wavelen*imaginary(a)
  
  IF keyword_set(aar) THEN BEGIN
      out = sh_define_aar(len=n_elements(wavelen))
      out.data.wave = wavelen
      out.data.flux = c_abs
      c_abs = out
  ENDIF 

  return,c_abs
END
