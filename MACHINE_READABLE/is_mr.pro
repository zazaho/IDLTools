FUNCTION is_mr,mr
  
  IF NOT keyword_set(mr) THEN return,0

  IF n_tags(mr) EQ 0 THEN return,0

  IF (where(strupcase(tag_names(mr)) EQ 'TYPE'))[0] EQ -1 THEN return,0
  
  IF strupcase(mr.type) NE 'MR_STRUCTURE' THEN return,0
 
  return,1
END
