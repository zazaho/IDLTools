function pah_makeratios,in_struct,denominators=denominators,nominators=nominators

  struct=in_struct
  ;; for ratio pair make a sub structure with the following info
  ;; data (=ratio0
  ;; stdev
  ;; label in tex make-up
  ;; the substructure is called R_denominator_nominator

  for vv=0,n_elements(nominators)-1 do begin
     foo=execute("a=struct.V_"+denominators[vv]+".data")
     foo=execute("a_e=struct.V_"+denominators[vv]+".stdev")
     foo=execute("b=struct.V_"+nominators[vv]+".data")
     foo=execute("b_e=struct.V_"+nominators[vv]+".stdev")
     foo=execute("a_label=struct.V_"+denominators[vv]+".label")
     foo=execute("b_label=struct.V_"+nominators[vv]+".label")
     
     ratio = a/b
     ;; this is formally correct but in reality it is only
     ;; representative when the uncertainty on b is much smaller than
     ;;the value of b
     stdev = ratio * sqrt((a_e/a)^2+(b_e/b)^2)

     ;; let's do some more measures for quantifying the
     ;; significance of the ratio. It looks to me that
     ;; max(a/a_e;b/b_e) is interesting. If bothm measurements are
     ;;uncertaint that the ratio is not very interesting if on of the
     ;;bands iis well detected the ratio is interesting even if it is
     ;;may be quite uncertain

     significance = a/a_e > b/b_e

     ;; calculate reasonable max and min values
     ;; min = a_min / b_max
     ;; max = a_max / b_min
     ;; in this we set
     ;; a_min = (a-a_e) > 0
     ;; b_min = (b-b_e) > 0
     ;; a_max = (a+a_e) > 3*a_e
     ;; b_max = (b+b_e) > 3*b_e

     minimum = ((a-a_e) > 0d0)/((b+b_e) > 3d0*b_e)
     maximum = ((a+a_e) > 3d0*a_e)/((b-b_e) > 0d0)

     label = a_label+"/"+b_label
     var = {data:ratio,stdev:stdev,label:label,significance:significance,min:minimum,max:maximum}
     struct=sh_add_tag(struct,"R_"+denominators[vv]+"_"+nominators[vv],var)
  endfor

  return,struct

end
