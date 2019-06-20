function sh_do_fit,x,y,num, fit_method=fit_method
  
  if not keyword_set(fit_method) then fit_method=0
  
  n = n_elements(x)
;now we can do the fit
  CASE fit_method OF
    0: begin
;mean fit
      fit_y = fltarr(num) + total(y)/n
    end
    1: begin
;linear interpol
      sx=total(float(x))
      sy=total(y)
      sxx=total(float(x)*float(x))
      sxy=total(float(x)*y)
      denom=n*sxx-sx*sx
      a=(sxx*sy-sx*sxy)/denom
      b=(n*sxy-sx*sy)/denom
      fit_y = a + b*indgen(num)
    end
    else:
  ENDCASE
  return,fit_y
  
end

PRO sh_apply_all,adet,kappa,firstx
; apply to all darks of current band
  
  common sh_mea_data, scantable, mea, dark_data, cut, section, glitch_flag
  common sh_dark_spddata, spd_flux,outspd,det,reset_m,toggle,spd_flag
  common sh_fit, fit_method, previous, next, execute
  
  num_sel = total(mea(section).dark.selected ne 0) ; number of
                                ; selectable darks
  
  det = adet
  
  if det le 48 then begin
    ndet_band = 12
    det_band  = (det-1) mod ndet_band
    part      = intarr(ndet_band) + 1
    part(det_band) = 4
    band      = (det-1)/12+1
  endif else begin
    ndet_band = 2
    det_band  = (det-49) mod ndet_band
    part      = intarr(ndet_band) +1
    part(det_band) = 2
    band      = 5+(det-1)/50
  endelse
  first_det = det-1 - det_band 
  last_det  = first_det + ndet_band-1
  
  cutsum = intarr(ndet_band)
  
  x_val = firstx
  x2_val = 0
  x_val_sum = x_val + x2_val
  
  clip_limit_set = [2]
  
  clip_method = 1
  kappa_val = kappa
  
  glitch_val = 1                ; glitch button is on
  
  for i_sel = 0,num_sel - 1 do begin ; loop over all selectable darks
    
    seldark = abs(mea(section).dark.selected(i_sel)) - 1
    
    mea(section).dark.selected = -abs(mea(section).dark.selected)
    mea(section).dark.selected(i_sel) = $
      abs(mea(section).dark.selected(i_sel))
    ind = mea(section).dark.ind(seldark)
    
    darkstart = dark_data(section).off(seldark)
    darkend   = dark_data(section).end_off(seldark)
    
    cut_start = dark_data(section).c_off(seldark)
    cut_end   = dark_data(section).c_off(seldark)+$
      dark_data(section).num(seldark)-1
    temp_cut = cut(first_det:first_det-1+ndet_band,cut_start:cut_end)
    len      = n_elements(temp_cut(det_band,*))
    
    for idet = 0, ndet_band-1 do cutsum(idet) = total(temp_cut(idet,*))
    not_all_cut = where(cutsum lt len,n_idet)
    
    if n_idet eq 0 then goto, dark_loop_end ; already all points
                                ; of this dark cut
    
    if (x_val_sum le len) and (x_val_sum gt 0) then begin
      for iidet = 0,n_idet-1 do begin ; loop over dets in band
        idet = not_all_cut(iidet)
        for j=0,x_val-1 do temp_cut(idet,j) = 1
        for j=len-x2_val,len-1 do temp_cut(idet,j) = 1
      endfor                    ; loop over dets in band
    endif
    
    for idet = 0, ndet_band-1 do cutsum(idet) = total(temp_cut(idet,*))
    not_all_cut = where(cutsum lt (len-1),n_idet)
    if n_idet eq 0 then goto, dark_loop_end ; 1 or 0 point left
    
    if glitch_val then begin
      for iidet = 0,n_idet-1 do begin ; loop over dets in band
        idet = not_all_cut(iidet)
        t_cut = temp_cut(idet,*)
        glitch_ind = where( glitch_flag(first_det+idet,*) eq 1)
        dummy=where((glitch_ind ge darkstart) and $
                    (glitch_ind le darkend),nglitch)
        if nglitch gt 0 then begin
          glitch_ind=glitch_ind(dummy)
          for j=0,nglitch-1 do begin
            dummy2 = where(reset_m(darkstart:glitch_ind(j),section) $
                           eq 1,count)
            t_cut(count-1) = glitch_val
          endfor
        endif
        if total(t_cut) lt len then temp_cut(idet,*) = t_cut
      endfor                    ; loop over dets in band
    endif
    
    if clip_method gt 0 then begin ; do clipping
      
      x_d = where( reset_m(scantable(ind).scan_start(section): $
                           scantable(ind).scan_end(section),section) ge 1) + $
        scantable(ind).scan_start(section)
      
      clip_limit = [clip_limit_set,len]
      
      for idet = 0, ndet_band-1 do begin ; loop over dets of band
        
        t_cut   = temp_cut(idet,*)
        spare   = len - total(t_cut)
        iclip   = where(clip_limit ge spare)
        nclip   = iclip(0)
        irepeat = 0
        
        s = size(spd_flux(first_det+idet,x_d)) ; following copied from
                                ;number of data
                                ; pro clip_...
        kn = s(s(0))
                                ;number of detectors
        dets = s(s(0)-1)
        
        xp=float(x_d)
        yp=reform(spd_flux(first_det+idet,x_d),dets,kn)
        
        while (irepeat lt nclip) do begin
          
          cp=reform(t_cut,dets,kn)
          if (n_elements(kappa_val) eq 1) then $
            kappa=fltarr(dets)+kappa_val
          
          for j=0,dets-1 do begin
            yr=reform(yp(j,*),kn)
            icut=reform(cp(j,*),kn)
            l=kappa(j)/1d0
            
            ind_clip=where(icut eq 0,c)
            if c gt 1 then begin
              ind2=ind_clip(where(yr(ind_clip) ne max(yr(ind_clip))))
              if c gt 2 then $
                ind2=ind2(where(yr(ind2) ne min(yr(ind2))))
              average=total(yr(ind2))/float(n_elements(ind2))
              s = sqrt(total((yr(ind2)-average)^2)/ $
                       float(n_elements(ind2)))
              cutted=where(abs(yr(ind_clip)-average) gt s*l,cc)
              if cc ne 0 then  icut(ind_clip(cutted)) = 1
            endif
            cp(j,*) = icut
          endfor                ;detector loop 
          t_cut=cp
          
          new_spare = len-total(t_cut)
          
          if (new_spare ge clip_limit(0)) and $
            (new_spare ne spare)         then begin
            temp_cut(idet,*) = t_cut
            spare            = new_spare
            irepeat          = irepeat + 1
          endif else irepeat = nclip
          
        endwhile                ; irepeat
        
      endfor                    ; loop over dets of band
    endif                       ; do clipping
    
    dark_loop_end :
    
    cut(first_det:first_det+ndet_band-1,cut_start:cut_end) = temp_cut
    spd_dat = where( reset_m(scantable(ind).scan_start(section): $
                             scantable(ind).scan_end(section),section) ge 1,count) + $
      scantable(ind).scan_start(section)
    cut_flag=outspd.data(spd_dat).det(first_det:first_det+ndet_band-1).flag
    
    cut_flag = cut_flag and not '40000000'xl
    cut_flag = cut_flag or (temp_cut * '40000000'xl)
    
    outspd.data(spd_dat).det(first_det:first_det+ndet_band-1).flag=cut_flag
    
  endfor                        ; loop over all selectable darks
  
end


;---------------------------------------------------------------------------

PRO sh_init_mea, sctbl
;   
  common sh_mea_data, scantable, mea, dark_data, cut, section, glitch_flag
  common sh_dark_spddata, spd_flux,outspd,det,reset_m,toggle,spd_flag
;
  
  
  scantable = sctbl.data
  n = n_elements(scantable)
  
  
;create structure for measurments (dark,scan,phot) with:
; number of measurments (.num), index to scantable (.ind) and
; index of selected measurments( .selected)
; index of non relevant scans(.zerowave) KS added 26.11.97
; zerowave(i,j) : i=0 band1,band3, i=1 band2,band4, i=2 band5, i=3 band6
; zerowave(*,0) : number of non relevant scans
; zerowave(*,1:number of non relevant scans) : indices of non rel.scans
  tmp = {num:0, ind:intarr(n), selected:intarr(n)} 
  tmp_scan = {num:0,ind:intarr(n),selected:intarr(n),zerowave:intarr(4,n+1)}
  mea = replicate({dark: tmp, scan: tmp_scan, phot: tmp} ,2)
;
; KS, dark_data.center added
  dark_data= $
    replicate({off:intarr(n),end_off:intarr(n),center:intarr(n), $
               t_reset:intarr(n)+1,num:intarr(n),c_off:intarr(n)},2)
;find darks in scantable
  for l=0,1 do begin
; i = where((strpos(strlowcase(scantable.scan_type(l)),'dark') ne -1),c)
    i = where(strlowcase(strmid(scantable.scan_type(l),0,4)) eq 'dark',c)
    mea(l).dark.num = c
    if c eq 0 then begin
      print,format='($,a)',string(7b)
      widget_control,report,set_value='no dark in section'+ strcompress(l) +' !'
    endif else begin  
      mea(l).dark.ind(0:c-1) = i
      dark_data(l).off     = scantable(i).scan_start(l)
      dark_data(l).end_off = scantable(i).scan_end(l)
      dark_data(l).center  = round((scantable(i).scan_start(l) + $ ;KS
                                    scantable(i).scan_end(l))/2)
      dark_data(l).t_reset = scantable(i).t_reset(l)
      
      dark_data(l).num = (scantable(i).scan_end(l) - dark_data(l).off) $
        /dark_data(l).t_reset + 1
      
      for j=0,n_elements(i)-1 do begin
        dum = where( reset_m(scantable(i(j)).scan_start(l): $
                             scantable(i(j)).scan_end(l),l) ge 1,num) ;+ $
        
        dark_data(l).num(j) = num
      endfor
      
;print,'dark_data(l).num',dark_data(l).num
      
      dark_data(l).c_off(0) = 0
      for k=1,c-1 do begin
        dark_data(l).c_off(k) = dark_data(l).c_off(k-1) + $
          dark_data(l).num(k-1)
      endfor
    endelse 
    
;find scans in scantable
    i = where(((strpos(strlowcase(scantable.scan_type(l)),'s0') ne -1) or $
               (strpos(strlowcase(scantable.scan_type(l)),'norm') ne -1) or $
               (strpos(strlowcase(scantable.scan_type(l)),'aot') ne -1) or $
               (strpos(strlowcase(scantable.scan_type(l)),'ref up') ne -1) or $
               (strpos(strlowcase(scantable.scan_type(l)),'updown') ne -1)) and $
              (strpos(strlowcase(scantable.scan_type(l)),'dark') eq -1), c)
    mea(l).scan.num = c
    if c eq 0 then begin
      print,format='($,a)',string(7b)
      widget_control,report,set_value='no scan in section'+ strcompress(l) +' !'
    endif else begin  
      mea(l).scan.ind(0:c-1) = i
;select all scans
      mea(l).scan.selected(0:c-1) = indgen(c) + 1
      
;select zerowave scans KS 26.11.97
      for k = 0,1 do begin      ; band1...band4
        j = l*24 + k*12         ; l=0,1 gives section, j gives det.-band
        count = 0                       
        for ii = 0,c-1 do begin ; loop over all scans
          scan_nr = i(ii)       ; i = indices of scans
          if total(scantable(scan_nr).det(j:j+11).wave_min + $
                   scantable(scan_nr).det(j:j+11).wave_max) eq 0 then begin
            count = count + 1
            mea(l).scan.zerowave(k,count) = scan_nr ; band1,2,3,4
          endif
        endfor
        mea(l).scan.zerowave(k,0) = count
        if l gt 0 then begin
          j = 48 + 2*k
          count = 0
          for ii = 0,c-1 do begin ; loop over all scans
            scan_nr = i(ii)
            if total(scantable(scan_nr).det(j:j+1).wave_min + $
                     scantable(scan_nr).det(j:j+1).wave_max) eq 0 then begin
              count = count + 1
              mea(l).scan.zerowave(k+2,count) = scan_nr ; band5,6
            endif
          endfor
          mea(l).scan.zerowave(k+2,0) = count
        endif
      endfor
    endelse
    
;find phot in scantable
    i = where((strpos(strlowcase(scantable.scan_type(l)),'grcal') ne -1) or $
              (strpos(strlowcase(scantable.scan_type(l)),'diff') ne -1), c)
    mea(l).phot.num = c
    if c eq 0 then begin
      print,format='($,a)',string(7b)
      widget_control,report,set_value='no phot in section'+ strcompress(l) +' !'
    endif else begin  
      mea(l).phot.ind(0:c-1) = i
    endelse
    
  endfor 
  
;initialize cut array:
  cut = intarr(52,max([total(dark_data(0).num),total(dark_data(1).num)]))
  
  sh_reset_mea
  
  
  return
END

;---------------------------------------------------------------------------

PRO sh_reset_mea
; 
  common sh_mea_data, scantable, mea, dark_data, cut, section, glitch_flag
  common sh_dark_spddata, spd_flux,outspd,det,reset_m,toggle,spd_flag
  
  section=0    
;cut(*) =0 ; KS eliminated
;KS, initialize CUT
  for l = 0,1 do begin
    for seldark = 0,mea(l).dark.num-1 do begin ; loop over darks
      j = mea(l).dark.ind(seldark)
      cut_start = dark_data(l).c_off(seldark)
      cut_end   = dark_data(l).c_off(seldark) + $
        dark_data(l).num(seldark) - 1
;kunze 25.3.98, laufvariable angepasst :
      d0 = l*24
      if l eq 0 then d1 = 23 else d1 = 51
      for i=d0,d1 do begin      ; loop over detectors
        spd_dat   = where(reset_m(scantable(j).scan_start(l):             $
                                  scantable(j).scan_end(l),l) ge 1,num) + $
          scantable(j).scan_start(l)
        cut_flag   = (outspd.data(spd_dat).det(i).flag and '40000000'xl) $
          eq 2l^30
        cut(i,cut_start:cut_end) = cut_flag
      endfor
    endfor
  endfor
;initialize CUT
  
  for l=0,1 do begin
;select all darks
    c =mea(l).dark.num
    if c gt 0 then $
      mea(l).dark.selected(0:c-1) = -(indgen(c) + 1)
;select all scans
    c =mea(l).scan.num
    if c gt 0 then $
      mea(l).scan.selected(0:c-1) = indgen(c) + 1
;select all photometric checks
    c =mea(l).phot.num
    if c gt 0 then $
      mea(l).phot.selected(0:c-1) = indgen(c) + 1
  endfor
  
  return
END    

;-----------------------------

pro sh_dark_do_for_all, do_scans=do_scans
  
; common bloecke:
  common sh_mea_data, scantable, mea, dark_data, cut, section, glitch_flag
  common sh_dark_spddata, spd_flux,outspd,det,reset_m,toggle,spd_flag
  common sh_fit, fit_method, previous, next, execute
  
;;first select all scans, refresh plot
  scan_num = mea(section).scan.num
  phot_num = mea(section).phot.num
  dark_num = mea(section).dark.num
  mea(section).scan.selected(0:scan_num-1) = indgen(scan_num) + 1
  if phot_num gt 0 then $
    mea(section).phot.selected(0:phot_num-1) = indgen(phot_num) + 1
  mea(section).dark.selected(0:dark_num-1) = -(indgen(dark_num) + 1)
  xr=0
  yr=0
  i_reset=where(reset_m(*,section) eq 1)
  
; detector band :
  if det lt 48 then begin
    dets = indgen(12) + 12*((det-1)/12) + 1
    band = dets(0)/12 + 1
  endif else begin
    dets = indgen( 2) + 49 + 2*(det/51)
    band = dets(0)/51 + 5
  endelse
  
  if keyword_set(do_scans) then do_scans=1 else do_scans=0 ; scans or phots
  
  if not previous and not next then do_default = 1 $
  else do_default = 0
  
  actual_method = fit_method
  act_prev      = previous
  act_next      = next
  
  if do_scans then begin        ; scans
    scan_indices = mea(section).scan.ind ; indices der scans, vector
    if dets(0) le 48 then k = (dets(0)-24*section)/12 $
    else k = (dets(0)-49)/2 + 2
    zero_count = mea(section).scan.zerowave(k,0) ; number of zero_scans
    test_zero  = zero_count < 1
    if do_default then begin
      actual_method = 1         ; linear
      act_prev      = 1         ; previous
      act_next      = 1         ; next
      print,format='($,a)',string(7b)
      widget_control,report,set_value=['default fit :', $
                                       'previous and next dark, method=linear']
    endif
  endif else begin              ; phots
    scan_num = phot_num
    scan_indices = mea(section).phot.ind ; indices der phots, vector
    test_zero = 0
    if do_default then begin
      actual_method = 0         ; mean
      act_prev      = 1         ; previous
      act_next      = 0         ; no next
      print,format='($,a)',string(7b)
      widget_control,report,set_value=['default fit :', $
                                       'previous dark, method=mean']
    endif
  endelse
  
  for m = 0,scan_num - 1 do begin
    
    scan_ind = scan_indices(m)
    if do_scans and test_zero then begin
      dum = where(mea(section).scan.zerowave(k,1:zero_count) eq scan_ind)
      if dum(0) ne -1 then goto, loop_end ; zerowave
      line1  = ['SCAN-NR : ' + strtrim(m+1,2)]
    endif else begin
      line1  = ['PHOT-NR : ' + strtrim(m+1,2)]
    endelse
    
    method = actual_method
    info   = ''
    
    i_dark = total(mea(section).dark.ind(0:dark_num-1) lt scan_ind)
    i_prev_dark=(where(abs(mea(section).dark.selected) eq i_dark))(0)
    
    i_dark = dark_num - total(mea(section).dark.ind(0:dark_num-1) gt $
                              scan_ind) + 1
    i_next_dark=(where(abs(mea(section).dark.selected) eq i_dark))(0)
    
    gain_scan = scantable(scan_ind).band(band-1).gain
    t_reset_scan = scantable(scan_ind).t_reset(section)
    
    if i_prev_dark ne -1 then begin
      i_dark_p = mea(section).dark.ind(i_prev_dark)
      prev_gain_dark = scantable(i_dark_p).band(band-1).gain
      prev_t_reset_dark = scantable(i_dark_p).t_reset(section)
      if (prev_gain_dark ne gain_scan) or $
        (prev_t_reset_dark ne t_reset_scan) then i_prev_dark = -1
    endif
    if i_next_dark ne -1 then begin
      i_dark_n = mea(section).dark.ind(i_next_dark)
      next_gain_dark = scantable(i_dark_n).band(band-1).gain
      next_t_reset_dark = scantable(i_dark_n).t_reset(section)
      if (next_gain_dark ne gain_scan) or $
        (next_t_reset_dark ne t_reset_scan) then i_next_dark = -1
    endif
    
    if (i_prev_dark eq -1) and (i_next_dark eq -1) then begin
      info = [line1,'neither previous nor next dark','no fit done']
      iacknowledge,text = info , group=top_id
    endif else begin
      case 1 of
        act_prev and act_next : begin
          case 1 of
            i_prev_dark eq -1 : begin
              i_sel_dark = [abs(mea(section).dark.selected(i_next_dark))]
              info = [line1,'no previous dark, next dark taken', 'method=mean']
              method = 0
              n_sel  = 1
            end
            i_next_dark eq -1 : begin
              i_sel_dark = [abs(mea(section).dark.selected(i_prev_dark))]
              info = [line1,'no next dark, previous dark taken', 'method=mean']
              method = 0
              n_sel  = 1
            end
            else : begin
              i_sel_dark = [abs(mea(section).dark.selected(i_prev_dark)), $
                            abs(mea(section).dark.selected(i_next_dark))]
              n_sel = 2
            end
          endcase
        end                     ; previous and next
        act_prev : begin
          if (i_prev_dark ne -1) then $
            i_sel_dark = [abs(mea(section).dark.selected(i_prev_dark))] $
          else begin
            i_sel_dark = [abs(mea(section).dark.selected(i_next_dark))]
            info = [line1,'no previous dark, next dark taken', 'method=mean']
            method = 0
          endelse
          n_sel = 1
        end                     ; previous
        act_next : begin
          if (i_next_dark ne -1) then $
            i_sel_dark = [abs(mea(section).dark.selected(i_next_dark))] $
          else begin
            i_sel_dark = [abs(mea(section).dark.selected(i_prev_dark))]
            info = [line1,'no next dark, previous dark taken', 'method=mean']
            method = 0
          endelse
          n_sel = 1
        end
      endcase
      i_sel_dark = i_sel_dark - 1
      
; following part taken from "event for apply2band"
; indices of dark in flux array
      i_dark = 0
      i_cut = 0
      
      for l=0,n_sel-1 do begin
        i_sel_d = i_sel_dark(l)
        ind = indgen(dark_data(section).num(i_sel_d))
        ind2= mea(section).dark.ind(i_sel_d)
        i_d = where( reset_m(scantable(ind2).scan_start(section): $
                             scantable(ind2).scan_end(section),section) ge 1,count) + $
          scantable(ind2).scan_start(section)
        i_dark = [i_dark,i_d]
        i_cut  = [i_cut, ind  + dark_data(section).c_off(i_sel_d)]
      endfor
;cut the first invalid dark
      i_dark= i_dark(1:*)
      i_cut = i_cut(1:*)
      
      n_flux=n_elements(spd_flux(det-1,*))          
      i_reset=where(reset_m(*,section) eq 1)
      xmin = min(i_dark, max=xmax)
      xr = [xmin,xmax]
      
      ndet = n_elements(dets)
      for i = 0,ndet-1 do begin
        j = dets(i) - 1
        i_not_cut=where(cut(j,i_cut) eq 0,n_dark)
        if n_dark eq 0 then begin
          line2 = 'DET NR : ' + strtrim(j+1,2)
          iacknowledge,group=top_id,text = [line1, line2, $
                       'You have cut all points in the selected dark(s)']
        endif else begin                                  
;prepare selected darks for fit
          ind_dark = i_dark(i_not_cut)
          flux = spd_flux(j,*)
;now we can do the fit
          fit_dark = sh_do_fit(ind_dark,flux(ind_dark), $
                            n_flux, fit_method=method)
          cor_dark = flux(i_dark) - fit_dark(i_dark)
;copy corrected scan into outspd and plot:
          i_scan = where(reset_m(scantable(scan_ind).scan_start(section): $
                                 scantable(scan_ind).scan_end(section),section) ge 1,count) + $
            scantable(scan_ind).scan_start(section)
          outspd.data(i_scan).det(j).flux = flux(i_scan)-fit_dark(i_scan)
          
        endelse
      endfor                    ; loop over dets
    endelse                     ; next and/or previous dark existent
;    if info(0) ne '' then iacknowledge,text = info, group=top_id
    
    loop_end :
  endfor                        ; loop over all scans
  
end                             ; sh_dark_do_for_all

; --------------------------------------------------------------------- DIK ---
; FUNCTION SH_DARK, spd, sctbl=sctbl, PARAMS=params
; --------------------------------------------------------------------- DIK ---
FUNCTION SH_DARK, spd, sctbl=sctbl,params=params 
; --------------------------------------------------------------------- DIK ---
  
  
  common sh_dark_spddata, spd_flux,outspd,det,reset_m,toggle,spd_flag
  common sh_fit, fit_method, previous, next, execute
  common sh_mea_data, scantable, mea, dark_data, cut, section, glitch_flag
  
;  junk   = { CW_PDMENU_S, flags:0, name:'' }
  
;--- do some initializations: --------------------------------------------
;
  spd_flux = spd.data.det.flux
  spd_flag = spd.data.det.flag
  outspd=spd
  
  det=1l
  section=0
  
; decode status word
  status = strcompress( strupcase( decode_status(spd.data.status) ), $     
                        /remove_all )
; sw/lw reset time 
  reset_m = [ [strpos( status, 'SWRESET' ) ge 0], $
              [strpos( status, 'LWRESET' ) ge 0] ]
  
;--------------------------------------------------------------------------
;generate glitch flag array:
  
  glitch_flag = test_flag(spd.data.det.flag,glitch=0)
  for i=0,23 do begin
    glitch_flag(i,*)=reform( (1-temporary(glitch_flag(i,*))) AND reset_m(*,0) )
  endfor
  for i=24,51 do begin
    glitch_flag(i,*)=reform( (1-temporary(glitch_flag(i,*))) AND reset_m(*,1) )
  endfor
  test=where(glitch_flag(40,*) eq 1,count)
  
;---------------------------------------------------------------------------
; get sctbl info:
  IF (n_elements(sctbl) EQ 0) THEN BEGIN
    error,'M','Re-deriving scantable - please be patient!'
    scantable=make_sctbl(spd)
    sctbl = scantable
  ENDIF ELSE BEGIN
    scantable = sctbl
  ENDELSE
  
;get index of dark, scan and phot
  sh_init_mea, scantable
  if total(mea.dark.num) eq 0 then begin
    print,format='($,a)',string(7b)
    return, outspd
  endif
  
  if (n_elements(params) NE 8) then begin
    print,'Using default values for the clipping and cutting:'
    params = [[2,0],[3,2],[2,0],[3,2]]
  ENDIF
  
  FOR i = 0,3 DO BEGIN
    print,string(format= $
                 '("Band: ",I2,", Cut first: ",I2,", Clipping: ",f4.1)', $
                 i,params(0,i),params(1,i))
  ENDFOR
  
  fit_method = 1
  next = 1
  previous = 1
  
  sh_apply_all,1L ,params(0,0),params(1,0)
  sh_apply_all,13L,params(0,1),params(1,1)
  sh_apply_all,25L,params(0,2),params(1,2)
  sh_apply_all,37L,params(0,3),params(1,3)
  
  det=1
  sh_dark_do_for_all,/do_scans
  sh_dark_do_for_all,do_scans=0
  
  det=13
  sh_dark_do_for_all,/do_scans
  sh_dark_do_for_all,do_scans=0
  
  det=25
  sh_dark_do_for_all,/do_scans
  sh_dark_do_for_all,do_scans=0
  
  det=37
  sh_dark_do_for_all,/do_scans
  sh_dark_do_for_all,do_scans=0
  
  return,outspd
  
END
