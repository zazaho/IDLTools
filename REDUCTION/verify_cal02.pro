FUNCTION verify_cal02,c
  return,(min(c.data.tau) LT max(c.data.tau))
END
