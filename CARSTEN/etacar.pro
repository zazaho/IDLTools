pro etacar
;; Program to look at the output of the collisional model code for
;; Vega--like stars;;
;;
;; (c) 1998 Carsten Dominik 
;; $Id: vegaview.pro,v 1.1 1999/02/17 15:41:04 dominik Exp dominik $

;;
;; Default values
;;

  n = 81             ; Resolution, should be an odd number
  draw_xsize = 600   ; Pixel size of drawing window.
  draw_ysize = 600
  inclmin  = 0 & inclmax  =  90 &  incldef  = 30   ; inclination
  thetamin = 0 & thetamax = 180 &  thetadef = 27  ; roll angle
  l = 2.                                          ; size of cube

  ; No we define a few arrays, so that we can make the state structure
  ; in the correct size
  rho=fltarr(n,n,n)
  x=fltarr(n,n,n) & y=fltarr(n,n,n) & z=fltarr(n,n,n)
  x0 = 0. & y0 = 0. & z0 = 0.
  xwidth = x[1]-x[0]  &  xmin = 0  &  xmax = l  &  nmid = floor(n/2)

;
; Read the image
;
image = readfits('~/d1/TIMMI2_RENS/DECONVOLVED/deconvshiftadd_10350732.fits')
image = bytscl(image)
;; We have to cut out the proper part
;image = image[156:185,110:139]
image = image[155:184,109:138]
image = congrid(image,600,600)
;; Do some pixel scaling to enhance contrast
cutlow = 30B
cuthigh = 130B
image = (((image>cutlow)<cuthigh)-cutlow)/ $
  (1.d0*cuthigh-1.d0*cutlow)*(!D.N_COLORS-1)
image=byte(image)

;image = readfits('deconvshiftadd_10350756.fits')
;image = bytscl(image)
;;; We have to cut out the proper part
;image = image[148:177,117:146]
;image = congrid(image,600,600)
;;; Do some pixel scaling to enhance contrast
;cutlow = 0B
;cuthigh = 70B
;image = (((image>cutlow)<cuthigh)-cutlow)/ $
;  (1.d0*cuthigh-1.d0*cutlow)*!D.N_COLORS
;image=byte(image)

;
; Backdrop bitmap
;
window,/free,/pixmap,xsize=draw_xsize,ysize=draw_ysize
mybitmap=!d.window
;
; Set up the widgets
;
wbase      = WIDGET_BASE(column=2,title='ETA CAR') 

dbase1     = WIDGET_BASE(wbase,column=1)          ; Base for Draw widget
wdr_main   = WIDGET_DRAW(dbase1,xsize=draw_xsize,$; Main Draw Window
             ysize=draw_ysize,frame=10)           ;
dsbase1    = WIDGET_BASE(dbase1,column=5)         ; Base for buttons

but_redo   = WIDGET_BUTTON(dsbase1,value='Recompute Geometry',$ ; Quit button
                           uvalue='wbu_redo') ; 
wsl_incl   = WIDGET_SLIDER(dsbase1,$
             minimum=inclmin,maximum=inclmax,$
             uvalue='wsl_incl',$
             title = 'Inclination', $
             value=incldef,/drag)
wsl_theta  = WIDGET_SLIDER(dsbase1,$
             minimum=thetamin,maximum=thetamax,$
             uvalue='wsl_theta',$
             title = 'Theta', $
             value=thetadef,/drag)

but_quit   = WIDGET_BUTTON(dsbase1,value='QUIT',$ ; Quit button
                           uvalue='wbu_quit') ; 
;
; Put things on the screen
;
WIDGET_CONTROL,wbase,/realize
WIDGET_CONTROL,wdr_main,get_value=window_draw
;
; The big data structure
;
state={wbase:wbase,$
       window_draw:window_draw,draw_xsize:draw_xsize,draw_ysize:draw_ysize,$
       incl:incldef,theta:thetadef, $
       inclmin:inclmin,inclmax:inclmax,thetamin:thetamin,thetamax:thetamax,$
       xmin:xmin,xmax:xmax,xwidth:xwidth,nmid:nmid, $
       n:n,l:l,rho:rho,x:x,y:y,z:z,$
       mybitmap:mybitmap, $
       image:image $
       }

; Fill the cube with data points

do_geometry,state

; Give this structure to the main widget 
;
WIDGET_CONTROL,wbase,set_uvalue=state

;
; Make a initial plot
;

do_plot,state

; Now activate the widget and exit.
;
xmanager,'setup_hello',wbase,/NO_BLOCK
end

pro do_geometry,state
  ;; Fill the cube with new data

  n = state.n
  l = state.l
  rho=fltarr(n,n,n)
  x=fltarr(n,n,n)
  y=fltarr(n,n,n)
  z=fltarr(n,n,n)

  x0 = 0. & y0 = 0. & z0 = 0. & 
  r0 = 0.5
  rin = 0.23
  rout = 0.37
  zmin = 0.5
  zmax = 0.6

  for i = 0,n-1 do begin
      x[i,*,*] = replicate(l/(n-1.)*(i - floor(n/2.)),n,n)
      y[*,i,*] = replicate(l/(n-1.)*(i - floor(n/2.)),n,n)
      z[*,*,i] = replicate(l/(n-1.)*(i - floor(n/2.)),n,n)
  endfor
  distz = sqrt((x-x0)^2+(y-y0)^2)
  cond = where((distz ge rin) and (distz le rout) and abs(z) ge zmin and abs(z) le zmax)
;  cond = where((distz ge rin) and (distz le rout) and z ge zmin and z le zmax)
;  cond = where((distz ge rin) and (distz le rout) and z le -zmin and z ge -zmax)
  rho[cond] = 1.

  ;; Store back into the state structure
  state.rho = rho
  state.x=x
  state.y=y
  state.z=z
end


;----------------------------------------------------------
; WIDGET EVENT HANDLER
;----------------------------------------------------------

PRO setup_hello_event,sevent

WIDGET_CONTROL,sevent.id,get_uvalue=uvalue
WIDGET_CONTROL,sevent.top,get_uvalue=state,/no_copy

case uvalue of
    'wsl_incl':begin
        WIDGET_CONTROL,sevent.id,get_value=txt
        val=float(txt)
        state.incl=val(0)
        do_plot,state	
    end
    'wsl_theta': begin
        WIDGET_CONTROL,sevent.id,get_value=txt
        val=float(txt)
        state.theta=val(0)
        do_plot,state	
    end
    'wbu_quit' : begin
        WIDGET_CONTROL,sevent.top,/destroy
        !except=1
    end
    'wbu_redo' : begin
        do_geometry,state
        do_plot,state	
    end
    else: notrecog=1
endcase
;;WIDGET_CONTROL,sevent.top,set_uvalue=state,/no_copy
end

PRO do_plot,state
  wset,state.mybitmap
  v1 = [1.,0.,0.]
  v2 = [0.,1.,0.]
  rotate_vectors,v1,v2,state.incl,state.theta,v1new=v1,v2new=v2

  p1 = (state.x*v1[0]+state.y*v1[1]+state.z*v1[2] + 1.)/2.*state.n
  p11 = p1*state.rho
  p2 = (state.x*v2[0]+state.y*v2[1]+state.z*v2[2] + 1.)/2.*state.n
  p22 = p2*state.rho
  p3 = (1.*floor(p22+.49))*state.n+(1.*floor(p11+.49))
  p3[state.n-1,state.n-1,state.n-1]=state.n^2-1
  z=histogram(p3,min=0.,max=state.n^2-1)
  z[0]=0
  z = reform(z,state.n,state.n,/overwrite)

  ;; Fix the problem in the center
  z[state.nmid+1,*] = (z[state.nmid+2,*]+z[state.nmid,*])/2.
  z[*,state.nmid+1] = (z[*,state.nmid+2]+z[*,state.nmid])/2.

  ;; Plot it in the backdrop map, and swap the image in.
  ;; Carefull, we need to place the real image in here as well.
  tv,state.image
  contour,z,levels=indgen(100),xmargin=[0,0],ymargin=[0,0],/noer
;  shade_surf,z
  wset,state.window_draw
  device,copy=[0,0,state.draw_xsize,state.draw_ysize,0,0,state.mybitmap]
  
                                ;(SH Apr  4 2001) To get an ps file
  sh_ps,'fitted_11.9_theta16.ps',/color,xsize=10,ysize=10,bits=8
  tv,state.image
  contour,z,levels=indgen(100),xmargin=[0,0],ymargin=[0,0],/noer, $
    color=!D.N_COLORS
  stop
  sh_ps,/view
end


PRO warn,text
info,"WARNING: "+text
END

PRO info,text
print,text
END

function normvect,v
  return, v/(sqrt(v[0]^2+v[1]^2+v[2]^2))
end

pro rotate_vectors,v1,v2,incl,theta,v1new=v1new,v2new=v2new

  ;; apply inclination: rotate around y axis
  a  = incl*!PI/180.
  v1 = [cos(a)*v1[0]+sin(a)*v1[2], v1[1] , -sin(a)*v1[0]+cos(a)*v1[2]]
  v2 = [cos(a)*v2[0]+sin(a)*v2[2], v2[1] , -sin(a)*v2[0]+cos(a)*v2[2]]

  ;; Now rotate around the new z axis
  a = theta*!PI/180.
  newz = fltarr(3)
  newz[0] = v1[1]*v2[2]-v1[2]*v2[1];
  newz[1] = v1[2]*v2[0]-v1[0]*v2[2];
  newz[2] = v1[0]*v2[1]-v1[1]*v2[0];
  p = fltarr(4)
  p[0] = newz[0]*sin(a/2)
  p[1] = newz[1]*sin(a/2)
  p[2] = newz[2]*sin(a/2)
  p[3] = cos(a/2)
  mat = fltarr(9)
  mat[0] = p[0]*p[0] - p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
  mat[1] = 2.*(p[0]*p[1] - p[2]*p[3]);
  mat[2] = 2.*(p[0]*p[2] + p[1]*p[3]);
  mat[3] = 2.*(p[0]*p[1] + p[2]*p[3]);
  mat[4] = -p[0]*p[0] + p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
  mat[5] = 2.*(p[1]*p[2] - p[0]*p[3]);
  mat[6] = 2.*(p[0]*p[2] - p[1]*p[3]);
  mat[7] = 2.*(p[1]*p[2] + p[0]*p[3]);
  mat[8] = -p[0]*p[0] - p[1]*p[1] + p[2]*p[2] + p[3]*p[3];
  r = fltarr(3)
  r[0] = mat[0]*v1[0] + mat[1]*v1[1] + mat[2]*v1[2];
  r[1] = mat[3]*v1[0] + mat[4]*v1[1] + mat[5]*v1[2];
  r[2] = mat[6]*v1[0] + mat[7]*v1[1] + mat[8]*v1[2];
  v1 = r
  r[0] = mat[0]*v2[0] + mat[1]*v2[1] + mat[2]*v2[2];
  r[1] = mat[3]*v2[0] + mat[4]*v2[1] + mat[5]*v2[2];
  r[2] = mat[6]*v2[0] + mat[7]*v2[1] + mat[8]*v2[2];
  v2 = r

  ;; This should not be necessary
  v1 = normvect(v1)
  v2 = normvect(v2)
  v1new = v1
  v2new = v2
end
