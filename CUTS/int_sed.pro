FUNCTION int_sed,w,f
  a = sh_define_aar(length=n_elements(w))
  
  isort= sort(w)
  a.data.wave = w[isort]
  a.data.flux = f[isort]

  int = sh_integrate(a,/quiet,/noplot)

  return,int
END
