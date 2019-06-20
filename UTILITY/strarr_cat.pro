;; function that returns a string that is the concationation of all
;; elements in the str_array.
FUNCTION strarr_cat,strarr
  return,STRING( strarr, FORMAT='(16(16384(16384A)))' )
END
