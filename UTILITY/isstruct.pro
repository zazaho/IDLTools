;; stupid shortcut to test if variable is a structure
function isstruct,v
  return,(size(v,/tname) eq 'STRUCT')
end
