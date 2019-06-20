FUNCTION sh_replace,base,repl_in,xrange=xrange

  repl = repl_in

  IF keyword_set(xrange) THEN BEGIN
      repl = sh_select(repl,(repl.data.wave GT xrange[0]) AND (repl.data.wave LT xrange[1]))
  ENDIF
     
  base1 = sh_select(base,base.data.wave LT min(repl.data.wave))
  base2 = sh_select(base,base.data.wave GT max(repl.data.wave))

  out = sh_combine(base1,repl,base2)
  return,out
END
