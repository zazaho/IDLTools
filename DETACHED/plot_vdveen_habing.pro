;; Simple routine to overplot the iras 2 color diagram lines from
;; van der veen and habing 1988

PRO plot_vdveen_habing,regions=reg,names=names,_extra=_extra

  I_x=[-1.5,-1.2,-1.2,-1.5,-1.8,-1.8,-1.5]
  I_y=[-2.4,-2.2,-1.8,-1.6,-1.8,-2.2,-2.4]
  II_x=[-1.1,-0.8,-0.8,-1.2,-1.2,-1.1]
  II_y=[-2.4,-2.4,-1.8,-1.8,-2.2,-2.4]
  IIIa_x=[-0.8,0.2,0.2,-0.4,-0.4,-0.8,-0.8]
  IIIa_y=[-2.4,-2.4,-1.0,-1.0,-1.8,-1.8,-2.4]
  IIIb_x=[0.2,1.0,1.0,0.4,0.4,0.2,0.2]
  IIIb_y=[-2.4,-1.6,-0.4,-0.4,-1.0,-1.0,-2.4]
  IV_x=[1.0,1.2,1.4,1.4,0.4,0.4,1.0,1.0]
  IV_y=[-1.2,-1.2,-1.0,0.2,0.2,-0.4,-0.4,-1.2]
  V_x=[0.8,2.0,2.0,1.4,1.4,1.2,1.0,1.0,0.8]
  V_y=[-1.8,-1.8,1.0,0.2,-1.0,-1.2,-1.2,-1.6,-1.8]
  VIa_x=[-1.8,-1.5,-1.4,-1.4,-0.8,-0.8,-1.8,-1.8]
  VIa_y=[-1.8,-1.6,-1.6,-1.0,-1.0,1.0,1.0,-1.8]
  VIb_x=[-0.8,0.4,0.4,-0.8,-0.8]
  VIb_y=[-1.0,-1.0,1.0,1.0,-1.0]
  VII_x=[-1.2,-0.4,-0.4,-1.4,-1.4,-1.5,-1.2]
  VII_y=[-1.8,-1.8,-1.0,-1.0,-1.6,-1.6,-1.8]
  VIII_x=[0.4,1.4,2.0,0.4,0.4]
  VIII_y=[0.2,0.2,1.0,1.0,0.2]
  
  reg_names = ['I','II','IIIa','IIIb','IV','V','VIa','VIb','VII','VIII']
  
  default,reg,reg_names
  default,names,0
  
  size_reg = size(reg)
  IF NOT size_reg[size_reg[0]+1] EQ 7 THEN BEGIN
      reg = reg_names[reg]
  ENDIF 
  
  FOR i =0, n_elements(reg)-1 DO BEGIN
      foo = execute('oplot,_extra=_extra,'+reg[i]+'_x,'+reg[i]+'_y')
  ENDFOR 

  IF names NE 0 THEN BEGIN
      names_I_x=1.5
      names_I_y=-2
      names_II_x=-1
      names_II_y=-2
      names_IIIa_x=-0.2
      names_IIIa_y=-1.8
      names_IIIb_x=1.8
      names_IIIb_y=-1.6
      names_IV_x=1
      names_IV_y=-0.2
      names_V_x=1.7
      names_V_y=-0.2
      names_VIa_x=-1.4
      names_VIa_y=0
      names_VIb_x=-0.4
      names_VIb_y=0
      names_VII_x=-1.0
      names_VII_y=-1.4
      names_VIII_x=1
      names_VIII_y=0.6
      
      FOR i =0, n_elements(reg)-1 DO BEGIN
          foo = execute('xyouts,align=0.5,_extra=_extra,names_'+reg[i]+'_x,names_'+reg[i]+'_y,reg[i]')
      ENDFOR 
  ENDIF 

END 
