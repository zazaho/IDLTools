function index_of_items_in_list,items,list
  nitems = n_elements(items)
  nlist  = n_elements(list)

  ritems = reform(items,nitems)
  rlist  = reform(list,nlist)

  ;; expand
  l = lindgen(nlist,nitems)
  mlist  = rlist[l mod nlist]
  mitems = ritems[l / nlist]
  mlistindex = (lindgen(nlist))[l mod nlist]
  
  mindex = mlistindex*(mlist eq mitems)
  index = round(total(mindex,1))
  
  return,index
end

  
function assignerrors,xdrfile,s

  if (n_elements(s) eq 0) or (size(s,/tname) ne 'STRUCT') then return,-1
;  if (n_elements(s_in) eq 0) or (size(s_in,/tname) ne 'STRUCT') then return,-1
;  s=s_in

  restore,xdrfile
  
  nvar      = reform(erref.nvar     )
  namevar   = reform(erref.namevar  )
  namespec  = reform(erref.namespec )
  sigmedup  = reform(erref.sigmedup )
  sigmedown = reform(erref.sigmedown)

  nx = n_elements(s.errorclass[*,0])
  ny = n_elements(s.errorclass[0,*])
  
  ;; enlarge to have 'unassigned' in the namespec
  namespec  = ['unassigned',namespec]
  sigmedup  = [[make_array(nvar,value=!values.d_nan)],[sigmedup]]
  sigmedown = [[make_array(nvar,value=!values.d_nan)],[sigmedown]]

  ;; map error class to position in namespec
  idx = index_of_items_in_list(s.errorclass,namespec)
  
  errors ={ $
          sigmedup_i6_2 : reform(sigmedup[(WHERE(namevar EQ "I(6.2)"))[0],idx],nx,ny), $
          sigmedup_i7_7 : reform(sigmedup[(WHERE(namevar EQ "I(7.7)"))[0],idx],nx,ny), $
          sigmedup_i8_6 : reform(sigmedup[(WHERE(namevar EQ "I(8.6)"))[0],idx],nx,ny), $
          sigmedup_i11_3 : reform(sigmedup[(WHERE(namevar EQ "I(11.3)"))[0],idx],nx,ny), $
          sigmedup_i12_7 : reform(sigmedup[(WHERE(namevar EQ "I(12.7)"))[0],idx],nx,ny), $
          sigmedup_comp17 : reform(sigmedup[(WHERE(namevar EQ "Comp(17)"))[0],idx],nx,ny), $
          sigmedup_icont : reform(sigmedup[(WHERE(namevar EQ "I(cont)"))[0],idx],nx,ny), $
          sigmedup_neii : reform(sigmedup[(WHERE(namevar EQ "[NeII]"))[0],idx],nx,ny), $
          sigmedup_neiii : reform(sigmedup[(WHERE(namevar EQ "[NeIII]"))[0],idx],nx,ny), $
          sigmedup_siii : reform(sigmedup[(WHERE(namevar EQ "[SIII]"))[0],idx],nx,ny), $
          sigmedup_siv : reform(sigmedup[(WHERE(namevar EQ "[SIV]"))[0],idx],nx,ny), $
          sigmedup_plat7_7 : reform(sigmedup[(WHERE(namevar EQ "Plat(7.7)"))[0],idx],nx,ny), $
          sigmedup_plat11_3 : reform(sigmedup[(WHERE(namevar EQ "Plat(11.3)"))[0],idx],nx,ny), $
          sigmedup_platcor11_3 : reform(sigmedup[(WHERE(namevar EQ "PlatCor(11.3)"))[0],idx],nx,ny), $
          sigmedup_comp11_3 : reform(sigmedup[(WHERE(namevar EQ "Comp(11.3)"))[0],idx],nx,ny), $
          sigmedup_compcor11_3 : reform(sigmedup[(WHERE(namevar EQ "CompCor(11.3)"))[0],idx],nx,ny), $
          sigmedup_comp7_7 : reform(sigmedup[(WHERE(namevar EQ "Comp(7.7)"))[0],idx],nx,ny), $
          sigmedup_ipah : reform(sigmedup[(WHERE(namevar EQ "I(PAH)"))[0],idx],nx,ny), $
          sigmedown_i6_2 : reform(sigmedown[(WHERE(namevar EQ "I(6.2)"))[0],idx],nx,ny), $
          sigmedown_i7_7 : reform(sigmedown[(WHERE(namevar EQ "I(7.7)"))[0],idx],nx,ny), $
          sigmedown_i8_6 : reform(sigmedown[(WHERE(namevar EQ "I(8.6)"))[0],idx],nx,ny), $
          sigmedown_i11_3 : reform(sigmedown[(WHERE(namevar EQ "I(11.3)"))[0],idx],nx,ny), $
          sigmedown_i12_7 : reform(sigmedown[(WHERE(namevar EQ "I(12.7)"))[0],idx],nx,ny), $
          sigmedown_comp17 : reform(sigmedown[(WHERE(namevar EQ "Comp(17)"))[0],idx],nx,ny), $
          sigmedown_icont : reform(sigmedown[(WHERE(namevar EQ "I(cont)"))[0],idx],nx,ny), $
          sigmedown_neii : reform(sigmedown[(WHERE(namevar EQ "[NeII]"))[0],idx],nx,ny), $
          sigmedown_neiii : reform(sigmedown[(WHERE(namevar EQ "[NeIII]"))[0],idx],nx,ny), $
          sigmedown_siii : reform(sigmedown[(WHERE(namevar EQ "[SIII]"))[0],idx],nx,ny), $
          sigmedown_siv : reform(sigmedown[(WHERE(namevar EQ "[SIV]"))[0],idx],nx,ny), $
          sigmedown_plat7_7 : reform(sigmedown[(WHERE(namevar EQ "Plat(7.7)"))[0],idx],nx,ny), $
          sigmedown_plat11_3 : reform(sigmedown[(WHERE(namevar EQ "Plat(11.3)"))[0],idx],nx,ny), $
          sigmedown_platcor11_3 : reform(sigmedown[(WHERE(namevar EQ "PlatCor(11.3)"))[0],idx],nx,ny), $
          sigmedown_comp11_3 : reform(sigmedown[(WHERE(namevar EQ "Comp(11.3)"))[0],idx],nx,ny), $
          sigmedown_compcor11_3 : reform(sigmedown[(WHERE(namevar EQ "CompCor(11.3)"))[0],idx],nx,ny), $
          sigmedown_comp7_7 : reform(sigmedown[(WHERE(namevar EQ "Comp(7.7)"))[0],idx],nx,ny), $
          sigmedown_ipah : reform(sigmedown[(WHERE(namevar EQ "I(PAH)"))[0],idx],nx,ny) $
          }
  
  pro_add_tag,s,'assigned_errors',errors
  
  return,s
end
