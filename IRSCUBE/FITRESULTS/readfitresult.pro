function readfitresult,sourcename, $
                       topdir=topdir, $
                       subdir=subdir, $
                       xdrfilename=xdrfilename, $
                       tagpairs=tagpairs
  
  default,sourcename,'lmc-hii01'
  default,topdir,'~/d1/PROJECTS/PAHCUBES/LMC/SHORT_ONLY_SACHA/FIT_RESULTS'
  default,subdir,'Fit_noalox_jiggband'
  default,xdrfilename,'*.xdr*'

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
          ['IPAH','COMP7_7'], $
          ['COMP7_7','I6_2'], $
          ['COMP7_7','I8_6'], $
          ['COMP7_7','I11_3'], $
          ['COMP7_7','I12_7'], $
          ['COMP7_7','COMP11_3'], $
          ['I6_2','I7_7'], $
          ['I6_2','I8_6'], $
          ['I6_2','I11_3'], $
          ['I6_2','I12_7'], $
          ['I6_2','COMP11_3'], $
          ['I7_7','I8_6'], $
          ['I7_7','I6_2'], $
          ['I8_6','I11_3'], $
          ['PLAT7_7','COMP11_3'], $
          ['I11_3','I6_2'], $
          ['I11_3','COMP7_7'], $
          ['I11_3','I7_7'], $
          ['I11_3','I8_6'], $
          ['I11_3','I12_7'], $
          ['I12_7','COMP7_7'] $
          ]
  
  
  fullfilename = topdir+'/'+sourcename+'/'+subdir+'/'+xdrfilename
  
  output1 = restorefitxdr(fullfilename)
  output2 = makevariables(output1)
  output2 = makeratios_noerrors(output2,tagpairs=tagpairs)

  ;; cast to a bit more usable structure
  
  return,create_struct(output1,output2.derived_variables,output2.band_ratios)

end
