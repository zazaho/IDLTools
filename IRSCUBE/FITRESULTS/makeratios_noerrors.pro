function makeratios_noerrors,s,tagpairs=tagpairs

  if (n_elements(s) eq 0 ) or (size(s,/tname) ne 'STRUCT') then return,-1
  
  vars = s.derived_variables
  tags = tag_names(vars)

  ;; list of ratios to calc: ab, means: b/a

  default,tagpairs, $
          [ $
          ['NEII','NEIII'], $
          ['SIII','SIV'], $
          ['ICONT','IPAH'], $
          ['ICONT','COMP7_7'], $
          ['ICONT','COMP11_3'], $
          ['ICONT','COMPCOR11_3'], $
          ['ICONT','PLATCOR11_3'], $
          ['ICONT','COMP17'], $
          ['IPAH','COMP7_7'], $
          ['COMP7_7','I6_2'], $
          ['COMP7_7','I8_6'], $
          ['COMP7_7','I11_3'], $
          ['COMP7_7','I12_7'], $
          ['COMP7_7','COMP11_3'], $
          ['COMP7_7','COMPCOR11_3'], $
          ['COMP7_7','COMP17'], $
          ['I6_2','I7_7'], $
          ['I6_2','I8_6'], $
          ['I6_2','I11_3'], $
          ['I6_2','I12_7'], $
          ['I6_2','COMP11_3'], $
          ['I6_2','COMPCOR11_3'], $
          ['I6_2','COMP17'], $
          ['I7_7','I8_6'], $
          ['I8_6','I11_3'], $
          ['PLAT7_7','COMP11_3'], $
          ['PLAT7_7','COMPCOR11_3'], $
          ['PLAT7_7','COMP17'], $
          ['I11_3','I6_2'], $
          ['I11_3','COMP7_7'], $
          ['I11_3','I7_7'], $
          ['I11_3','I8_6'], $
          ['I11_3','I12_7'], $
          ['I11_3','COMP17'], $
          ['COMPCOR11_3','I6_2'], $
          ['COMPCOR11_3','COMP7_7'], $
          ['COMPCOR11_3','I8_6'], $
          ['COMPCOR11_3','COMP17'], $
          ['I12_7','COMP7_7'], $
          ['PLATCOR11_3','COMP17'] $
          ]
  
  nx = n_elements((vars.(0))[*,0])
  ny = n_elements((vars.(0))[0,*])
  
  execcmd = 'r={b:0'
  
  for i=0,n_elements(tagpairs[0,*])-1 do begin
     tag_nom = tagpairs[1,i]
     tag_den = tagpairs[0,i]
     
     foo = execute('nom=reform(vars.'+tag_nom+',nx*ny)')
     foo = execute('den=reform(vars.'+tag_den+',nx*ny)')
     
     r=reform(nom/den,nx,ny)
     
     foo = execute('r_'+tag_nom+'_'+tag_den+'=r')
     
     execcmd = execcmd+','+'r_'+tag_nom+'_'+tag_den+':'+'r_'+tag_nom+'_'+tag_den
  endfor
  
  execcmd=execcmd+'}'
  foo = execute(execcmd)
  pro_add_tag,s,'band_ratios',r,/replace

  return,s
end
