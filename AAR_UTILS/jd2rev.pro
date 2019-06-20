;;Function to convert julday of obs to rev#
FUNCTION jd2rev,jd
  return,floor(-2455946.2+1.0024111*jd)
END
