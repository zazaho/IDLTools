PRO feps_remove_bad_pix,orders_present,pl_version
;
; Purpose : to remove NaN values and pad pixels from images by replacing them by 
;           the mean of a 1x6 pixel box surrounding the dead pixel
;           The working dir. is assumed to be the parent directory containing
;           the background corrected images for an observed target 
;
;    In:        orders_present
;                  list of all observed low res orders and number of cycles
;
;
;    Author: Jeroen Bouwman, MPIA, Germany
; 
;    (c) 2004
;
;

      ;define pixel masks around bad pixel for replacing that pixel
      mean_sub = fltarr(1)
      var_sub = fltarr(1)
      sub_index = {x1:[0,0,0,0,0,0],$
                   y1:[3,-3,2,-2,-1,1],$
                   x2:[-1,1,0,0,0,0],$
                   y2:[2,-2,2,-2,-1,1],$
                   x3:[1,-1,0,0,0,0],$
                   y3:[2,-2,2,-2,-1,1]}
     
      ;loop over all observed modules
      for ii=0,n_elements(orders_present.cycles)-1 do begin

         ;if not observed, skip
         if orders_present.cycles[ii] ne 0 then begin

           im=replicate({pos1:dblarr(128,128,3),pos2:dblarr(128,128,3)},$
                           orders_present.cycles[ii])

           ;loop over all cycles for module ii
           for jj=0,orders_present.cycles[ii]-1 do begin

             ;read all images, unc. plains and pixel masks
             im[jj].pos1[*,*,0]=readfits(orders_present.modules[ii]+$
                                        '/'+orders_present.modules[ii]+$
                    '_pos1_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+$
                                           '.fits',header0_pos1,/silent)
             im[jj].pos2[*,*,0]=readfits(orders_present.modules[ii]+'/'+$
                                     orders_present.modules[ii]+$
                    '_pos2_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+$
                                     '.fits',header0_pos2,/silent)
             im[jj].pos1[*,*,1]=readfits(orders_present.modules[ii]+$
                                      '/'+orders_present.modules[ii]+$
                    '_pos1_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+$
                                     '.fits',header0_pos1_unc, exten=1,/silent)
             im[jj].pos2[*,*,1]=readfits(orders_present.modules[ii]+$
                                     '/'+orders_present.modules[ii]+$
                    '_pos2_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+$
                                      '.fits',header0_pos2_unc, exten=1,/silent)
             im[jj].pos1[*,*,2]=readfits(orders_present.modules[ii]+$
                                      '/'+orders_present.modules[ii]+$
                    '_pos1_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+$
                                   '.fits',header0_pos1_mask, exten=2,/silent)
             im[jj].pos2[*,*,2]=readfits(orders_present.modules[ii]+$
                              '/'+orders_present.modules[ii]+$
                    '_pos2_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+$
                             '.fits',header0_pos2_mask, exten=2,/silent)

             ;read all header for each cycle jj and put them into a struckture
             if jj eq 0 then begin
                  header = create_struct(string(jj,format='(i4.1)'),$
                         {pos1:header0_pos1,pos2:header0_pos2})
                  header_unc = create_struct(string(jj,format='(i4.1)'),$
                         {pos1:header0_pos1_unc,pos2:header0_pos2_unc})
                  header_mask = create_struct(string(jj,format='(i4.1)'),$
                         {pos1:header0_pos1_mask,pos2:header0_pos2_mask})
             endif else begin
                  header= create_struct(header,string(jj,format='(i4.1)'),$
                          {pos1:header0_pos1,pos2:header0_pos2})
                  header_unc= create_struct(header_unc,string(jj,format='(i4.1)'),$
                          {pos1:header0_pos1_unc,pos2:header0_pos2_unc})
                  header_mask= create_struct(header_mask,string(jj,format='(i4.1)'),$
                          {pos1:header0_pos1_mask,pos2:header0_pos2_mask})
             endelse

           ;end loop jj over all cycles for module ii
           endfor
           
           ;check if there is a user defined bad pixel file. If so, flag listed pixels as bad
           if ((file_search(orders_present.modules[ii]+$
                "/"+orders_present.modules[ii]+"_bad_pix.dat") ne '' )[0]) then begin
                openr,lun,orders_present.modules[ii]+"/"+orders_present.modules[ii]+"_bad_pix.dat",/get_lun
                dummy=''
                xbad=dblarr(200)
                ybad=dblarr(200)
                ibad=0L
                while not eof(lun) do begin
                   readf,lun,dummy
                   tempo = strsplit(dummy,/extract)
                   xbad[ibad]=fix(tempo[0])
                   ybad[ibad]=fix(tempo[1])
                   ibad = ibad+1L
                endwhile
               nbad=ibad-1
               xbad=xbad[0:nbad]
               ybad=ybad[0:nbad]
               close,lun
               free_lun,lun

               for ibad=0,nbad do begin
                  im[*].pos1[xbad[ibad],ybad[ibad],2]=2L^16
                  im[*].pos2[xbad[ibad],ybad[ibad],2]=2L^16
               endfor
           endif


           ;pick right order mask
           if strmid(orders_present.modules[ii],0,2) eq 'SL' then begin
             omask = readfits(!sm_sl_omask_f,/silent)
             wmask = readfits(!sm_sl_wavesamp_wave_f,/silent)
             order = fix(strmid(orders_present.modules[ii],2,1))
           endif else begin
             omask = readfits(!sm_ll_omask_f,/silent)
             wmask = readfits(!sm_ll_wavesamp_wave_f,/silent)
             order = fix(strmid(orders_present.modules[ii],2,1))
           endelse

           defsysv, '!extract_background', EXISTS=i
           if i EQ 1 then begin
             if !extract_background EQ 2 then begin
                if (order eq '1') then begin
                     order='2'
                endif else if ((order eq '2') or (order eq '3')) then order='1'
             endif
          endif

           ;make the order mask 2 pixels smaller at each side to avoid edges
           for irows=2,125 do begin
               index = where(omask[*,irows] eq order,count)
               if count ne 0 then omask[index[[0,1,n_elements(index)-2,$
                                            n_elements(index)-1]],irows]=0
           endfor
           omask[*,[0,1,126,127]]=0

           rsrf_pos1=dblarr(128,128)
           rsrf_pos1[*,*]=1.d0
           rsrf_pos2=dblarr(128,128)
           rsrf_pos2[*,*]=1.d0

           ;check if additional bad pixels need to be identified
           if (!feps_identify_bad_pixel gt 0) and (orders_present.cycles[ii] gt 1) then begin

               index = where(omask eq order, count)
               index_x = index mod 128
               index_y = index / 128
               index_z = intarr(n_elements(index_x))
               index_z_mask = intarr(n_elements(index_x))
               index_z[*] = 0
               index_z_mask[*] = 2

               uniq_index_y = index_y[uniq(index_y, sort(index_y))]
               uniq_index_x = index_x[uniq(index_x, sort(index_x))]

               mean_row=dblarr(n_elements(uniq_index_y),2)

               sigma_level=!feps_identify_bad_pixel
               sigma_level_row=!feps_identify_bad_row

               if !feps_rsrf[0] ne '0' then begin
                  module_use = orders_present.modules[ii]
                  defsysv, '!extract_background', EXISTS=i
                  if i EQ 1 then begin
                     if !extract_background GT 1 then begin
                        if (module_use eq 'SL1') then begin
                             module_use='SL2'
                        endif else if ((module_use eq 'SL2') or (module_use eq 'SL3')) then module_use='SL1'
                        if (module_use eq 'LL1') then begin
                           module_use='LL2'
                        endif else if ((module_use eq 'LL2') or (module_use eq 'LL3')) then module_use='LL1'
                     endif
                  endif
                  feps_cal_standard=feps_read_standard(pl_version,!feps_rsrf)
                  index_structure = where(tag_names(feps_cal_standard.pos1) eq 'WAVE_'+module_use,count)
                  for k=0,n_elements(index_x)-1 do begin
                      rsrf_pos1[index_x[k],index_y[k]]=$
                      feps_interpol( feps_cal_standard.pos1.(index_structure),$
                      feps_cal_standard.pos1.(index_structure+1),wmask[index_x[k],index_y[k]],/linear )
                      rsrf_pos2[index_x[k],index_y[k]]=$
                      feps_interpol( feps_cal_standard.pos2.(index_structure),$
                      feps_cal_standard.pos2.(index_structure+1),wmask[index_x[k],index_y[k]],/linear )
                  endfor
               endif

               for jj=0,orders_present.cycles[ii]-2 do begin
                  im_dif_pos1=im[jj].pos1[index_x,index_y,index_z]-$
                                im[jj+1].pos1[index_x,index_y,index_z]
                  im_dif_pos2=im[jj].pos2[index_x,index_y,index_z]-$
                                im[jj+1].pos2[index_x,index_y,index_z]
                 
                 ;find bad pixels per row
                 for kk=0,n_elements(uniq_index_y)-1 do begin
                    index=where(finite(im_dif_pos1) and $
                                index_y eq uniq_index_y[kk], count)
; FL check if there is any finite data
                    if count eq 0 then goto,next_kk
                    feps_resistant_mean,im_dif_pos1[index],sigma_level,Mean,Sigma,Num_Rej,$
                          index_bad=index_bad
                    mean_row[kk,0]=Mean
                    ;mask the bad pixels
                    if index_bad[0] ne -1 then begin
                        im[jj].pos1[index_x[index[index_bad]],index_y[index[index_bad]],$
                         index_z_mask[index[index_bad]]]=2L^16
                        im[jj+1].pos1[index_x[index[index_bad]],index_y[index[index_bad]],$
                         index_z_mask[index[index_bad]]]=2L^16
                    endif
                    index=where(finite(im_dif_pos2) and $
                                index_y eq uniq_index_y[kk], count)
                    if count eq 0 then goto,next_kk
                    feps_resistant_mean,im_dif_pos2[index],sigma_level,Mean,Sigma,Num_Rej,$
                        index_bad=index_bad
                    mean_row[kk,1]=Mean
                    if index_bad[0] ne -1 then begin
                        im[jj].pos2[index_x[index[index_bad]],index_y[index[index_bad]],$
                         index_z_mask[index[index_bad]]]=2L^16
                        im[jj+1].pos2[index_x[index[index_bad]],index_y[index[index_bad]],$
                         index_z_mask[index[index_bad]]]=2L^16
                    endif
                    next_kk:
                 endfor 

                 ;identify bad rows
                 feps_resistant_mean,mean_row[*,0],sigma_level_row,Mean,Sigma,Num_Rej,index_bad=index_bad
                 if index_bad[0] ne -1 then begin
                   for kk=0,n_elements(index_bad)-1 do begin
                      index=where(index_y eq uniq_index_y[index_bad[kk]], count)
                      im[jj].pos1[index_x[index],index_y[index],index_z_mask[index]]=2L^16
                      im[jj+1].pos1[index_x[index],index_y[index],index_z_mask[index]]=2L^16
                   endfor
                 endif
                 feps_resistant_mean,mean_row[*,1],sigma_level_row,Mean,Sigma,Num_Rej,index_bad=index_bad
                 if index_bad[0] ne -1 then begin
                   for kk=0,n_elements(index_bad)-1 do begin
                      index=where(index_y eq uniq_index_y[index_bad[kk]], count)
                      im[jj].pos2[index_x[index],index_y[index],index_z_mask[index]]=2L^16
                      im[jj+1].pos2[index_x[index],index_y[index],index_z_mask[index]]=2L^16
                   endfor
                 endif

               endfor
           endif

           for jj=0,orders_present.cycles[ii]-1 do begin
               index_pos1 = where( ((finite(im[jj].pos1[*,*,0]) ne 1) and (omask eq order)) or $
               ((im[jj].pos1[*,*,2] ge !feps_bad_pix_treshold) and (omask eq order)),count)
               if count ne 0 then begin
                index_x_nan = index_pos1 mod 128
                index_y_nan = index_pos1 / 128
                index_z_nan = intarr(n_elements(index_x_nan))
                index_z_nan[*] = 0
                if n_tags(index_bad_pix_pos1) eq 0 then begin
                   index_bad_pix_pos1=create_struct('cycle'+feps_i2s(jj,3),$
                            {x:index_x_nan,y:index_y_nan,z:index_z_nan,count:count,alt:intarr(count,orders_present.cycles[ii])})
                endif else begin
                   index_bad_pix_pos1=create_struct(index_bad_pix_pos1,$
                       'cycle'+feps_i2s(jj,3),{x:index_x_nan,y:index_y_nan,z:index_z_nan,$
                              count:count,alt:intarr(count,orders_present.cycles[ii])})
                endelse
               endif else begin
                if n_tags(index_bad_pix_pos1) eq 0 then begin
                    index_bad_pix_pos1=create_struct('cycle'+feps_i2s(jj,3),{x:[0],y:[0],z:[0],$
                              count:0,alt:intarr(1,orders_present.cycles[ii])})
                endif else begin
                    index_bad_pix_pos1=create_struct(index_bad_pix_pos1,$
                         'cycle'+feps_i2s(jj,3),{x:[0],y:[0],z:[0],count:0,alt:intarr(1,orders_present.cycles[ii])})
                endelse
               endelse

               index_pos2 = where( ((finite(im[jj].pos2[*,*,0]) ne 1) and (omask eq order)) or $
               ((im[jj].pos2[*,*,2] ge !feps_bad_pix_treshold) and (omask eq order)),count)
               if count ne 0 then begin
                index_x_nan = index_pos2 mod 128
                index_y_nan = index_pos2 / 128
                index_z_nan = intarr(n_elements(index_x_nan))
                index_z_nan[*] = 0
                if n_tags(index_bad_pix_pos2) eq 0 then begin
                   index_bad_pix_pos2=create_struct('cycle'+feps_i2s(jj,3),$
                       {x:index_x_nan,y:index_y_nan,z:index_z_nan,count:count,alt:intarr(count,orders_present.cycles[ii])})
                endif else begin
                   index_bad_pix_pos2=create_struct(index_bad_pix_pos2,$
                         'cycle'+feps_i2s(jj,3),{x:index_x_nan,y:index_y_nan,z:index_z_nan,$
                              count:count,alt:intarr(count,orders_present.cycles[ii])})
                endelse
               endif else begin
                if n_tags(index_bad_pix_pos2) eq 0 then begin
                    index_bad_pix_pos2=create_struct('cycle'+feps_i2s(jj,3),{x:[0],y:[0],z:[0],$
                              count:0,alt:intarr(1,orders_present.cycles[ii])})
                endif else begin
                    index_bad_pix_pos2=create_struct(index_bad_pix_pos2,$
                         'cycle'+feps_i2s(jj,3),{x:[0],y:[0],z:[0],count:0,alt:intarr(1,orders_present.cycles[ii])})
                endelse
               endelse
           endfor

           if orders_present.cycles[ii] gt 1 then begin
            for jj=0,orders_present.cycles[ii]-1 do begin
                 for k=0,index_bad_pix_pos1.(jj).count-1 do begin
                    flag=intarr(orders_present.cycles[ii])
                    flag[jj]=1
                    for kk=0,orders_present.cycles[ii]-1 do begin
                       if (kk eq jj) then continue
                       index=where((index_bad_pix_pos1.(kk).x eq index_bad_pix_pos1.(jj).x[k]) and $
                       (index_bad_pix_pos1.(kk).y eq index_bad_pix_pos1.(jj).y[k]),count2)
                       if count2 ne 0 then begin
                             flag[kk] = 1
                       endif
                    endfor
                    index_bad_pix_pos1.(jj).alt[k,*]=(flag ne 1)
                 endfor
                 for k=0,index_bad_pix_pos2.(jj).count-1 do begin
                    flag=intarr(orders_present.cycles[ii])
                    flag[jj]=1
                    for kk=0,orders_present.cycles[ii]-1 do begin
                       if (kk eq jj) then continue
                       index=where((index_bad_pix_pos2.(kk).x eq index_bad_pix_pos2.(jj).x[k]) and $
                       (index_bad_pix_pos2.(kk).y eq index_bad_pix_pos2.(jj).y[k]),count2)
                       if count2 ne 0 then begin
                             flag[kk] = 1
                       endif
                    endfor
                    index_bad_pix_pos2.(jj).alt[k,*]=(flag ne 1)
                 endfor
            endfor
           endif
        

           for jj=0,orders_present.cycles[ii]-1 do begin

              header0_pos1=header.(jj).pos1
              header0_pos2=header.(jj).pos2

              if index_bad_pix_pos1.(jj).count ne 0 then begin

                im[jj].pos1[index_bad_pix_pos1.(jj).x,index_bad_pix_pos1.(jj).y,index_bad_pix_pos1.(jj).z] = !values.f_nan

                ;repeat the pixel replacement a few times in case of multiple
                ;bad pixels inside box 
                for m=0,5 do begin

                   im_temp = im[jj].pos1[*,*,0]/rsrf_pos1

                   ;loop over all bad pixels
                   for k=0,index_bad_pix_pos1.(jj).count-1 do begin
                     ix = index_bad_pix_pos1.(jj).x[k]
                     iy = index_bad_pix_pos1.(jj).y[k]
                     if total(index_bad_pix_pos1.(jj).alt[k,*]) ne 0 then begin
                       index=where(index_bad_pix_pos1.(jj).alt[k,*] eq 1)
                       im[jj].pos1[ix,iy,0] = avg(im[index].pos1[ix,iy,0])
                       im[jj].pos1[ix,iy,1] = sqrt(avg(im[index].pos1[ix,iy,1]^2))
                       im[jj].pos1[ix,iy,2] = 2L^16 
                       continue
                     endif
               
                     criteria = fltarr(n_elements(mean_sub))
                     ;select sub erea in 5x5 box around bad-pixel with low sigma
                     for isub=0,n_elements(mean_sub)-1 do begin
                        box_pos = im_temp[sub_index.(isub*2)+ix,sub_index.(isub*2+1)+iy]
                        ;check if more than one defined value is inside box
                        ;if single value, put mean to zero
                        temp0=where(finite(box_pos) eq 1, count_box)
                        if count_box ne 1 then begin
                           mean_sub[isub]=(moment(box_pos,/nan))[0]
                           var_sub[isub]=(moment(box_pos,/nan))[1]
                        endif else begin
                           mean_sub[isub]= 0.0
                           var_sub[isub] = box_pos[temp0]^2
                        endelse

                        criteria[isub] = total((box_pos - mean_sub)^2/var_sub)
                     endfor
                     temp0 = min(criteria,imin)

                     im[jj].pos1[ix,iy,0] = mean_sub[imin]*rsrf_pos1[ix,iy]
                     im[jj].pos1[ix,iy,1] = sqrt(var_sub[imin])*rsrf_pos1[ix,iy]
                     im[jj].pos1[ix,iy,2] = 2L^16

                   ;end k loop
                   endfor
               
                ;end m loop
                endfor

              ;end bad pixel replacement pos1
              endif

              if index_bad_pix_pos2.(jj).count ne 0 then begin

                im[jj].pos2[index_bad_pix_pos2.(jj).x,index_bad_pix_pos2.(jj).y,index_bad_pix_pos2.(jj).z] = !values.f_nan

                ;repeat the pixel replacement a few times in case of multiple
                ;bad pixels inside box 
                for m=0,5 do begin

                   im_temp = im[jj].pos2[*,*,0]/rsrf_pos2

                   ;loop over all bad pixels
                   for k=0,index_bad_pix_pos2.(jj).count-1 do begin
                     ix = index_bad_pix_pos2.(jj).x[k]
                     iy = index_bad_pix_pos2.(jj).y[k]
                     if total(index_bad_pix_pos2.(jj).alt[k,*]) ne 0 then begin
                       index=where(index_bad_pix_pos2.(jj).alt[k,*] eq 1)
                       im[jj].pos2[ix,iy,0] = avg(im[index].pos2[ix,iy,0])
                       im[jj].pos2[ix,iy,1] = sqrt(avg(im[index].pos2[ix,iy,1]^2))
                       im[jj].pos2[ix,iy,2] = 2L^16
                       continue
                     endif
               
                     criteria = fltarr(n_elements(mean_sub))
                     ;select sub erea in 5x5 box around bad-pixel with low sigma
                     for isub=0,n_elements(mean_sub)-1 do begin
                        box_pos = im_temp[sub_index.(isub*2)+ix,sub_index.(isub*2+1)+iy]
                        ;check if more than one defined value is inside box
                        ;if single value, put mean to zero
                        temp0=where(finite(box_pos) eq 1, count_box)
                        if count_box ne 1 then begin
                           mean_sub[isub]=(moment(box_pos,/nan))[0]
                           var_sub[isub]=(moment(box_pos,/nan))[1]
                        endif else begin
                           mean_sub[isub]= 0.0
                           var_sub[isub] = box_pos[temp0]^2
                        endelse

                        criteria[isub] = total((box_pos - mean_sub)^2/var_sub)
                     endfor
                     temp0 = min(criteria,imin)

                     im[jj].pos2[ix,iy,0] = mean_sub[imin]*rsrf_pos2[ix,iy]
                     im[jj].pos2[ix,iy,1] = sqrt(var_sub[imin])*rsrf_pos2[ix,iy]
                     im[jj].pos2[ix,iy,2] = 2L^16

                   ;end k loop
                   endfor
               
                ;end m loop
                endfor

              ;end bad pixel replacement pos2
              endif

              ;store new header
              if jj eq 0 then begin
                      header_fix = create_struct(string(jj,format='(i4.1)'),$
                         {pos1:header0_pos1,pos2:header0_pos2})
              endif else begin
                      header_fix = create_struct(header_fix,string(jj,format='(i4.1)'),$
                          {pos1:header0_pos1,pos2:header0_pos2})
              endelse


           ;end jj loop
           endfor

           ;set bad pixel index to zero for next ii loop
           index_bad_pix_pos1=0
           index_bad_pix_pos2=0
 
           ;new header with replaced pixel info for module ii
           header=header_fix


           ;update all headers to include info over pixel replacement
           for jj=0,orders_present.cycles[ii]-1 do begin
              fits_header = header.(jj).pos1
              fits_header_unc = header_unc.(jj).pos1
              fits_header_mask = header_mask.(jj).pos1
              outfile = orders_present.modules[ii]+'/'+orders_present.modules[ii]+$
                    '_pos1_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+'.fits'
              fits_open,outfile,fcb, /Write
              fits_write, fcb, im[jj].pos1[*,*,0], fits_header
              fits_write, fcb, im[jj].pos1[*,*,1], fits_header_unc,xtension='IMAGE',extver=1
              fits_write, fcb, im[jj].pos1[*,*,2], fits_header_mask,xtension='IMAGE',extver=2
              fits_close,fcb
              fits_header = header.(jj).pos2
              outfile = orders_present.modules[ii]+'/'+orders_present.modules[ii]+$
                    '_pos2_cycle'+strtrim(string(jj, format = '(i3.3)'),2)+'.fits'
              fits_open,outfile,fcb, /Write
              fits_write, fcb, im[jj].pos2[*,*,0], fits_header
              fits_write, fcb, im[jj].pos2[*,*,1], fits_header_unc,xtension='IMAGE',extver=1
              fits_write, fcb, im[jj].pos2[*,*,2], fits_header_mask,xtension='IMAGE',extver=2
              fits_close,fcb
           endfor

         ;end loop over all modules
         endif
      endfor


END
