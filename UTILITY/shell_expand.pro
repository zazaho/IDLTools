;; simple routine to use the shell to expand a string with the
;; embedded variables replaces
;; example print,shell_expand('$SWS_DATABASE')

function shell_expand,string
  spawn,'echo '+string,result
  return,result
end
