function makevariablestruct_isine,values,range
  return,(values ge range[0]) and (values le range[1])
end

function makevariablestruct,s

  if size(s,/tname) ne 'STRUCT' then return,-1

  parstr = s.parstr
  mask   = s.mask  
  fit    = s.fit   
  bands  = s.bands 
  lines  = s.lines 
  cont   = s.cont  

  nx = n_elements(mask[*,0])
  ny = n_elements(mask[0,*])

  pah62 = REFORM( parstr.Iband[WHERE(bands.label EQ "Main 6.2 (1)")] $
                  + parstr.Iband[WHERE(bands.label EQ "Main 6.2 (2)")] )
  pah77 = REFORM( parstr.Iband[WHERE(bands.label EQ "Main 7.7 (1)")] $
                  + parstr.Iband[WHERE(bands.label EQ "Main 7.7 (2)")] )
  plateau77 = REFORM(parstr.Iband[WHERE(bands.label EQ "Plateau 7.7")])
  pah86 = REFORM(parstr.Iband[WHERE(bands.label EQ "Main 8.6")])
  pah113 = REFORM(parstr.Iband[WHERE(bands.label EQ "Main 11.2")])
  plateau113 = REFORM(parstr.Iband[WHERE(bands.label EQ "Plateau 11.3")])
  pah127 = REFORM( parstr.Iband[WHERE(bands.label EQ "Main 12.7 (1)")] $
                   + parstr.Iband[WHERE(bands.label EQ "Main 12.7 (2)")] )
  pah17 = TOTAL(parstr.Iband[WHERE(makevariablestruct_ISINE(bands.wave,[16.3,18]))],1)
  pah   = TOTAL(parstr.Iband,1)
  
;; Lines
  neiii = REFORM(parstr.Iline[WHERE(lines.label EQ "NeIII1")])
  neii = REFORM(parstr.Iline[WHERE(lines.label EQ "NeII")])
  siv = REFORM(parstr.Iline[WHERE(lines.label EQ "SIV")])
  siii = REFORM(parstr.Iline[WHERE(lines.label EQ "SIII1")])
  
;; Continuum
  Fnu_cont = TOTAL(fit.Fnu_BB,3) + fit.Fnu_star
  
  cont10 = DBLARR(nx,ny)
  cont14 = DBLARR(nx,ny)
  cont1018 = DBLARR(nx,ny)
  alox = DBLARR(nx,ny)
  FOR i=0,nx-1 DO BEGIN
     FOR j=0,ny-1 DO BEGIN
        if mask[i,j] eq 0 then begin
           cont10[i,j] = TRAP_INT(fit.w,REFORM(Fnu_cont[i,j,*])*fit.nu/fit.w, $
                                  XRANGE=[9.5,10.5])
           cont14[i,j] = TRAP_INT(fit.w,REFORM(Fnu_cont[i,j,*])*fit.nu/fit.w, $
                                  XRANGE=[13.5,14.5])
           cont1018[i,j] = TRAP_INT(fit.w,REFORM(Fnu_cont[i,j,*])*fit.nu/fit.w, $
                                    XRANGE=[10.,18.])
           alox[i,j] = TRAP_INT(fit.w, $ 
                                REFORM(fit.Fnu_BB[i,j,WHERE(cont.type EQ "al2o3_koike"),*])*fit.nu/fit.w,$
                                XRANGE=[10.7,12.7])
        endif
     ENDFOR
  ENDFOR
  
  
;; definitions as used by fred for the error_reference.xdr
  
  MKSJy = 10d-26
  variables = { $
              I6_2: pah62*MKSJy*1.D15, $
              I7_7: pah77*MKSJy*1.D15, $
              I8_6: pah86*MKSJy*1.D15, $
              I11_3: pah113*MKSJy*1.D15, $
              I12_7: pah127*MKSJy*1.D15, $
              Comp17: pah17*MKSJy*1.D15, $
              Icont: cont1018*MKSJy*1.D15, $
              NeII: neii*MKSJy*1.D15, $
              NeIII: neiii*MKSJy*1.D15, $
              SIII: siii*MKSJy*1.D15, $
              SIV: siv*MKSJy*1.D15, $
              Plat7_7: plateau77*MKSJy*1.D15, $
              Plat11_3: plateau113*MKSJy*1.D15, $
              PlatCor11_3: (plateau113+alox)*MKSJy*1.D15, $
              Comp11_3: (plateau113+pah113)*MKSJy*1.D15, $
              CompCor11_3: (plateau113+pah113+alox)*MKSJy*1.D15, $
              Comp7_7: (pah77+plateau77)*MKSJy*1.D15, $
              IPAH: pah*MKSJy*1.D15, $
              goodidx:where(mask eq 0) $
              }        
  return,variables
end
