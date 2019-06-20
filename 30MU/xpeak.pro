FUNCTION xpeak,a
  foo = max(a.data.flux,idx_peak)
  return,a.data[idx_peak].wave
END
