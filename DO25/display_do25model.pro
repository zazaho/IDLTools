;; stupid wrapper to call read_do25model like a procedure instead of a function
PRO display_do25model,filename,_extra=_extra
  foo = read_do25model(filename,/plot,_extra=_extra,/no_block)
END
