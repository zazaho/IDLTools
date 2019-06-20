;;Function to convert start of obs to rev#
FUNCTION jd2rev,tm
  restore,shell_expand('~/IA_FILES/revtime.sav')
  
;; Find the rev from the time
  idx = max(where(revtime.start LT LONG64(tm)))
  return,revtime.revol(idx)
END
