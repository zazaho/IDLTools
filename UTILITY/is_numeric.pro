FUNCTION is_numeric,in
  s = size(in)
  return,(total(s[s[0]+1] EQ [1,2,3,4,5,6,9,12,13,14,15]) NE 0)
END
