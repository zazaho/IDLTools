FUNCTION sh_uniq,a
  return,a[uniq(a,sort(a))]
END
