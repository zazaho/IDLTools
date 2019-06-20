; function to return indices where elements in a are present in b
FUNCTION sh_where_array,A,B
; stretch the values in both directions
  AA = A#(b EQ b)
  BB = (a EQ a)#B
;compare the two matrices we just created
  I = where(AA eq BB)
  return,i / n_elements(a)
END
