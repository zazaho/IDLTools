FUNCTION is_float,in
  s = size(in)
  return,(total(s[s[0]+1] EQ [4,5]) NE 0)
END
