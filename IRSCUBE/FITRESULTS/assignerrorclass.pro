pro ae_safe_attribute,array,value,index
  if index[0] ne -1 then array[index] = value
end

function assignerrorclass,s
  
  if (n_elements(s) eq 0) or (size(s,/tname) ne 'STRUCT') then return,-1
;  if (n_elements(s_in) eq 0) or (size(s_in,/tname) ne 'STRUCT') then return,-1
;  s = s_in

  parstr = s.parstr
  mask   = s.mask  
  fit    = s.fit   
  bands  = s.bands 
  lines  = s.lines 
  
  flux_bands = parstr.iband[where(bands.label eq 'Main 7.7 (1)')]+parstr.iband[where(bands.label eq 'Main 7.7 (2)')]
  
  cutoff_bands0 =  0d0
  cutoff_bands1 =  7d10
  cutoff_bands2 = 14d10
  cutoff_bands3 =  max(flux_bands)
  
  fb  = (mask eq 0) and (flux_bands ge cutoff_bands0) and (flux_bands lt cutoff_bands1)
  mb  = (mask eq 0) and (flux_bands ge cutoff_bands1) and (flux_bands lt cutoff_bands2)
  bb  = (mask eq 0) and (flux_bands gt cutoff_bands2) and (flux_bands le cutoff_bands3)
  
  flux_lines = parstr.iline[where(lines.label eq 'NeIII1')]
  
  cutoff_lines0 =  0d0
  cutoff_lines1 = 1d10
  cutoff_lines2 = 5d10
  cutoff_lines3 =  max(flux_lines)
  
  fl  = (mask eq 0) and (flux_lines ge cutoff_lines0) and (flux_lines lt cutoff_lines1)
  ml  = (mask eq 0) and (flux_lines ge cutoff_lines1) and (flux_lines lt cutoff_lines2)
  bl  = (mask eq 0) and (flux_lines gt cutoff_lines2) and (flux_lines le cutoff_lines3)
  
  foo = min(fit.w-15d0,idxfifteen)
  flux_fifteen = reform(fit.fnu_tot[*,*,idxfifteen])
  
  cutoff_fifteen0 =  0d0
  cutoff_fifteen1 = 1d-3
  cutoff_fifteen2 = 4d-3
  cutoff_fifteen3 =  max(flux_fifteen)

  ff  = (mask eq 0) and (flux_fifteen ge cutoff_fifteen0) and (flux_fifteen lt cutoff_fifteen1)
  mf  = (mask eq 0) and (flux_fifteen ge cutoff_fifteen1) and (flux_fifteen lt cutoff_fifteen2)
  bf  = (mask eq 0) and (flux_fifteen gt cutoff_fifteen2) and (flux_fifteen le cutoff_fifteen3)
  
  errorclass = make_array(n_elements(mask[*,0]),n_elements(mask[0,*]),value='unassigned')

  ae_safe_attribute,errorclass,'fb_fl_ff',where(fb*fl*ff eq 1)
  ae_safe_attribute,errorclass,'fb_fl_mf',where(fb*fl*mf eq 1)
  ae_safe_attribute,errorclass,'fb_fl_bf',where(fb*fl*bf eq 1)
  ae_safe_attribute,errorclass,'fb_ml_ff',where(fb*ml*ff eq 1)
  ae_safe_attribute,errorclass,'fb_ml_mf',where(fb*ml*mf eq 1)
  ae_safe_attribute,errorclass,'fb_ml_bf',where(fb*ml*bf eq 1)
  ae_safe_attribute,errorclass,'fb_bl_ff',where(fb*bl*ff eq 1)
  ae_safe_attribute,errorclass,'fb_bl_mf',where(fb*bl*mf eq 1)
  ae_safe_attribute,errorclass,'fb_bl_bf',where(fb*bl*bf eq 1)
  ae_safe_attribute,errorclass,'mb_fl_ff',where(mb*fl*ff eq 1)
  ae_safe_attribute,errorclass,'mb_fl_mf',where(mb*fl*mf eq 1)
  ae_safe_attribute,errorclass,'mb_fl_bf',where(mb*fl*bf eq 1)
  ae_safe_attribute,errorclass,'mb_ml_ff',where(mb*ml*ff eq 1)
  ae_safe_attribute,errorclass,'mb_ml_mf',where(mb*ml*mf eq 1)
  ae_safe_attribute,errorclass,'mb_ml_bf',where(mb*ml*bf eq 1)
  ae_safe_attribute,errorclass,'mb_bl_ff',where(mb*bl*ff eq 1)
  ae_safe_attribute,errorclass,'mb_bl_mf',where(mb*bl*mf eq 1)
  ae_safe_attribute,errorclass,'mb_bl_bf',where(mb*bl*bf eq 1)
  ae_safe_attribute,errorclass,'bb_fl_ff',where(bb*fl*ff eq 1)
  ae_safe_attribute,errorclass,'bb_fl_mf',where(bb*fl*mf eq 1)
  ae_safe_attribute,errorclass,'bb_fl_bf',where(bb*fl*bf eq 1)
  ae_safe_attribute,errorclass,'bb_ml_ff',where(bb*ml*ff eq 1)
  ae_safe_attribute,errorclass,'bb_ml_mf',where(bb*ml*mf eq 1)
  ae_safe_attribute,errorclass,'bb_ml_bf',where(bb*ml*bf eq 1)
  ae_safe_attribute,errorclass,'bb_bl_ff',where(bb*bl*ff eq 1)
  ae_safe_attribute,errorclass,'bb_bl_mf',where(bb*bl*mf eq 1)
  ae_safe_attribute,errorclass,'bb_bl_bf',where(bb*bl*bf eq 1)

  ;; add in the real faint classes
  cutoff_realfaint_bands =  2.5d9
  rfb   = (mask eq 0) and (flux_bands le cutoff_realfaint_bands)

  cutoff_realfaint_lines =  21775987.
  rfl   = (mask eq 0) and (flux_lines le cutoff_realfaint_lines)

  cutoff_realfaint_fifteen = 3d-4
  rff   = (mask eq 0) and (flux_fifteen le cutoff_realfaint_fifteen)
  
  ;;ae_safe_attribute,errorclass,'faint',where(rfb*rfl*rff eq 1)

  ;; since we are interested mostly in the PAH maybe flag every thing
  ;; with low 77 as faint even if the lines or the continuum is strong.
  ae_safe_attribute,errorclass,'faint',where(rfb eq 1)

  pro_add_tag,s,'errorclass',errorclass
  return,s
end
