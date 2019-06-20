;; stupid wrapper to call read_spec like a procedure instead of a function
PRO display_spec,_extra=_extra
  foo = read_spec(/plot,_extra=_extra,/no_block)
END
