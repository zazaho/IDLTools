FUNCTION is_int,in
  s = size(in)
  return,(total(s[s[0]+1] EQ [1,2,3,12,13,14,15]) NE 0)
END
