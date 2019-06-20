FUNCTION verify_cal02b,c
  return,(min(c.data.shape_det48) LT max(c.data.shape_det48))
END
