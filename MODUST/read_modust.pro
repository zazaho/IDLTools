FUNCTION read_modust,file,distance=distance, $
                     star=star

  default,file,'dust.idl'
  default,distance,360.
  dpc=distance

;	open file for reading
  OPENR, UNIT, file, /GET_LUN
  skip = ''
  PRINT, 'Reading dust properties and emergent flux'
  
;       o- size parameters
;
  READF, UNIT, skip
  READF, UNIT, nf, ng, naa, nr
;
;       o- wavelength grid
;
  waves        = dblarr(nf)
;                       = wavelength in micron
  READF, UNIT, skip
  READF, UNIT, waves
;
;       o- emergent flux & emergent stellar flux
;
  emf          = dblarr(nf)
  READF, UNIT, skip
  READF, UNIT, emf
  emf          = 1.d23*emf
;                       = convert ergs/cm^2/sec/Hz to Jansky

  ;; (SH Jul  6 2015)
  ;; Return the stellar spectrum instead of the total
  if keyword_set(star) then begin
     emf          = dblarr(nf)
     READF, UNIT, skip
     READF, UNIT, emf
     emf          = 1.d23*emf
  endif
  
  close,unit
  free_lun,unit
  
;
;          scale flux to the right distance
  print,'distance =',dpc,' pc'
  if dpc ne 10 then begin
      print,' scaling flux to',dpc,' parsec'
      emf       = (10./dpc)^2 * emf
  endif

  out = makeaar(length=nf)
  out.data.wave=waves
  out.data.flux=emf

  aperfile = strmid(file,0,strlen(file)-4)+'.aperture'
  print,aperfile
  IF file_test(aperfile) THEN BEGIN
      out.data.stdev = collapse(((read_ascii(aperfile,data_start=8)).field1)[3,*])
  ENDIF 

  return,out
END
