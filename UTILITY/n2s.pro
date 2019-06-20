FUNCTION n2s,in,format=format
  default,format,''
  return,strtrim(string(format=format,in),2)
END
