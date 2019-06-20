FUNCTION i2s,in,pad
  default,pad,5
  foo = in+1d6
  str = string(foo,format='(I)')
  str = strmid(str,pad-1,/reverse)
  return,str
END
