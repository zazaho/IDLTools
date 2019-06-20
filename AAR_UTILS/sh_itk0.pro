FUNCTION sh_itk0,a
  utcs = ''
  sts   = READ_FITS_KEY( a.header, 'EOHAUTCS', utcs, comment )
  utcs = strcompress(utcs,/remove_all)
  if (strlen(utcs) eq 11) then $
       itks = ut_to_itk(a.header,utcs) $
  else itks=a.data(0).itk
  return,itks
END
