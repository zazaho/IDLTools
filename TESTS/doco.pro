PRO doco,name

  foo = mrdfits('l*.fits',0,h)
  f = mrdfits('l*.fits',1)

  CASE n_elements(h) OF
     0: BEGIN
        head=''
     END
     1: BEGIN 
        head = h
     END
     ELSE: BEGIN
        head = h[0]
        FOR i = 1, n_elements(h)-1 DO BEGIN
           head = head+h[i]
        ENDFOR
     END
  ENDCASE

  len = n_elements(f.(0))
  aar = makeaar(len=len,header=head)

  aar.data.wave = f.wave
  aar.data.flux = f.flux
  aar.data.stdev= f.stdev
  aar.data.det  = f.det 

  aar = sh_calcaar(aar,flambda=-1)
  aar = sh_calcaar(aar,fact=1d4)

  IF NOT keyword_set(name) THEN BEGIN
     name = object(aar)
     name = strlowcase(strcompress(name,/remove_all))
  ENDIF

  write_fdat,aar,'../'+name+'.dat',ncol=4,/head
END
