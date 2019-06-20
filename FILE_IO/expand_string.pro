function expand_string,str,_extra=_extra
  spawn,'echo "'+str+'"',expstr
  return,expstr
end
