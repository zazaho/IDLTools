;; stupid wrapper to call read_model like a procedure instead of a function
PRO display_model,filename,_extra=_extra
  foo = read_model(filename,/plot,_extra=_extra,/no_block)
END

  
