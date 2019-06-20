PRO plseds,s_in,index=index,fit=fit,_extra=_extra,coord=coord

  IF keyword_set(index) THEN begin
      s = select_mr(s_in,where=index)
  ENDIF ELSE BEGIN
      s = s_in
  ENDELSE

  IF keyword_set(coord) THEN BEGIN
      IF is_string(coord) EQ 1 THEN BEGIN
          c=sh_conv_coords(coord,/to_degree)
      ENDIF ELSE BEGIN
          c = coord
      ENDELSE 
      ra = c[0]
      de = c[1]
      r=col_mr(s,'ra')
      d=col_mr(s,'dec')
      
      ; calculate the distance to each source in tab2 in arcsecs
      dist = sqrt(((ra-r)*cos(!dpi/180d0*(de+d)/2d0))^2d0 + $
                   (de-d)^2d0 )*3600d0
      
      ;; find the closest match
      closest_dist = min(dist,closest_idx)
      print, 'found a source at ',closest_dist,' arcsec distance'
      s = select_mr(s,where=closest_idx)
  ENDIF


  
;; grab the columns
  ra     =col_mr(s,'ra')
  dec    =col_mr(s,'dec')
  Bmag   =col_mr(s,'mBmag')
  Vmag   =col_mr(s,'mVmag')
  Rmag   =col_mr(s,'mRmag')
  Imag   =col_mr(s,'mImag')
  Jmag   =col_mr(s,'Jmag')
  Hmag   =col_mr(s,'Hmag')
  Kmag   =col_mr(s,'Kmag')
  F1     =col_mr(s,'F1')
  e_F1   =col_mr(s,'e_F1')
  F2     =col_mr(s,'F2')
  e_F2   =col_mr(s,'e_F2')
  F3     =col_mr(s,'F3')
  e_F3   =col_mr(s,'e_F3') 
  F4     =col_mr(s,'F4')
  e_F4   =col_mr(s,'e_F4') 
  SpType =col_mr(s,'SpType')

  nelements = n_elements(ra)
  sra  = make_array(nelements,val='')
  sdec = sra
  
  FOR i=0,nelements-1 DO sra[i] = ra2s(ra[i],/full)
  FOR i=0,nelements-1 DO sdec[i] = dec2s(dec[i],/full)


;; convert mags to jy
  WV_B     = 0.44
  WV_V     = 0.55
  WV_R     = 0.70
  WV_I     = 0.90
  WV_J     = 1.235
  WV_H     = 1.662
  WV_K     = 2.159
  WV_1     = 3.6
  WV_2     = 4.5
  WV_3     = 5.8
  WV_4     = 8.0

  ZP_lam_B = 7.19d-8
  ZP_lam_V = 3.92d-8
  ZP_lam_R = 1.76d-8
  ZP_lam_I = 8.30d-9
  ZP_lam_J = 3.129d-9
  ZP_lam_H = 1.133d-9
  ZP_lam_K = 4.283d-10

  ZP_jy_B = ZP_lam_B * WV_B^2d0 / 3d14 * 1d26
  ZP_jy_V = ZP_lam_V * WV_V^2d0 / 3d14 * 1d26
  ZP_jy_R = ZP_lam_R * WV_R^2d0 / 3d14 * 1d26
  ZP_jy_I = ZP_lam_I * WV_I^2d0 / 3d14 * 1d26
  ZP_jy_J = ZP_lam_J * WV_J^2d0 / 3d14 * 1d26
  ZP_jy_H = ZP_lam_H * WV_H^2d0 / 3d14 * 1d26
  ZP_jy_K = ZP_lam_K * WV_K^2d0 / 3d14 * 1d26

  FLX_mJy_B = ZP_jy_B*1d1^(-1d0*Bmag/2.5d0)*1d6
  FLX_mJy_V = ZP_jy_V*1d1^(-1d0*Vmag/2.5d0)*1d6
  FLX_mJy_R = ZP_jy_R*1d1^(-1d0*Rmag/2.5d0)*1d6
  FLX_mJy_I = ZP_jy_I*1d1^(-1d0*Imag/2.5d0)*1d6
  FLX_mJy_J = ZP_jy_J*1d1^(-1d0*Jmag/2.5d0)*1d6
  FLX_mJy_H = ZP_jy_H*1d1^(-1d0*Hmag/2.5d0)*1d6
  FLX_mJy_K = ZP_jy_K*1d1^(-1d0*Kmag/2.5d0)*1d6


  FOR i = 0,nelements-1 DO BEGIN

      wave = [WV_B,WV_V,WV_R,WV_I,wv_j,WV_h,WV_k,wv_1,wv_2,wv_3,wv_4]
      flux = [FLX_mJy_B[i],FLX_mJy_V[i],FLX_mJy_R[i],FLX_mJy_I[i],FLX_mJy_J[i],FLX_mJy_H[i],FLX_mJy_K[i],F1[i],F2[i],F3[i],F4[i]]
      stdev= [1d-1*[FLX_mJy_B[i],FLX_mJy_V[i],FLX_mJy_R[i],FLX_mJy_I[i],FLX_mJy_J[i],FLX_mJy_H[i],FLX_mJy_K[i]],e_F1[i],e_F2[i],e_F3[i],e_F4[i]]
      
      idx_valid = where(finite(flux) EQ 1,cnt)
      
      IF cnt NE 0 THEN BEGIN
         wave = wave [idx_valid]
         flux = flux [idx_valid]
         stdev= stdev[idx_valid]
         
         aar=  {type   : 'SAAR'  ,$ ; structure type
                header : ''      ,$ ; the header string
                history: ''      ,$ ; history string
                data   : replicate({wave:0.0,flux:0.0,stdev:0.0,tint:0L,det:0L,itk:0L, $
                                    line:0L,sdir:0L,flag:0L},n_elements(wave)) } 
         
         aar.data.wave = wave
         aar.data.flux = flux
         aar.data.stdev= stdev
         
         pl,aar,ytitle='Flux [!Mm!XJy]',title=sra[i]+" "+sdec[i]+" ("+SpType[i]+")",_extra=_extra
         
         IF keyword_set(fit) THEN BEGIN
            fit = sh_bbfit(sh_select(aar,aar.data.wave LT 3.5),T=18000,pow=0,/fixpo,out=aar)
            pl,fit,/opl,linestyle=2,thick=3,ps=0,/noclip
         ENDIF
      ENDIF ELSE BEGIN
         message,'the source have no valid wavelength values',/info
      ENDELSE
  ENDFOR

END
