;; stupid wrapper to call read_lnk like a procedure instead of a function
PRO display_lnk,filename,_extra=_extra
  foo = read_lnk(filename,/plot,_extra=_extra,/no_block)
END

  
