  ;;; This is stolen (taken) from get_dustout.pro

pro read_modust3_get_dustout, file, wave=wave, star=star, spec=spec,nwave=nwave, status=status

; Open file for reading, get unique file unit UNIT with /GET_LUN
;    LUN = logical unit number
  
;	open file for reading
  CASE 1 OF
     FILE_TEST(file,/READ): BEGIN
        OPENR, UNIT, file, /GET_LUN
     END
     FILE_TEST(file+'.gz',/READ): BEGIN
        OPENR, UNIT, file+'.gz', /GET_LUN,/compress
     END
     ELSE: BEGIN
        message,'File not found:'+file,/info
        return
     END
  ENDCASE
  
;; Read the number of frequency points
  READF, UNIT, nf, ng, naa, nr
  
  waves        = dblarr(nf)
  emf          = dblarr(nf)
  emf_tstar    = dblarr(nf)
;                       = wavelength in micron
  READF, UNIT, waves
  READF, UNIT, emf
  READF, UNIT, emf_tstar

  close,UNIT
  FREE_LUN, UNIT

  wave    = waves
;                       = convert ergs/cm^2/sec/Hz to Jansky
  spec    = 1.d23*emf
  star    = 1.d23*emf_tstar
  nwave = n_elements(wave)

  RETURN
END


;; Simply procedure to read the output of a modust multishell run into
;; some structure. 
;; We specify the output directory and then we read the following data:
;; dust.out, dust1.out,dust2.out each into an aar

FUNCTION read_modust3,dir,distance=distance

  default,dir,'./'
  default,distance,1000.

  IF strmid(dir,0,1,/reverse_offset) NE '/' THEN dir = dir+'/'
  
  read_modust3_get_dustout, dir+'dust.out' , wave=wave , star=star , spec=spec ,nwave=nwave

  IF n_elements(wave) EQ 0 THEN BEGIN
     return,-1
  ENDIF

  ;; Now correct for the distance
  if distance ne 10 then begin
      star       = (10./distance)^2 * star
      spec       = (10./distance)^2 * spec
  endif
;
  aar_star = sh_define_aar(len=nwave)
  aar_spec = sh_define_aar(len=nwave)
  
  aar_star .data.wave = wave
  aar_spec .data.wave = wave
  aar_star .data.flux = star 
  aar_spec .data.flux = spec 

  
  IF FILE_TEST(dir+'dust.dat',/READ) THEN BEGIN
     dust   = string(read_binary(dir+'dust.dat'))
  ENDIF ELSE BEGIN
     message,/info,'Could not read input file: '+dir+'dust.dat'
     dust = 'Not found, thus empty'
  ENDELSE 

  IF FILE_TEST(dir+'modust.inp',/READ) THEN BEGIN
     modust   = string(read_binary(dir+'modust.inp'))
  ENDIF ELSE BEGIN
     message,/info,'Could not read input file: '+dir+'modust.inp'
     modust = 'Not found, thus empty'
  ENDELSE 

;; Now combine everything in a single structure:
  model = {directory:dir,distance:distance,dust:dust, $
           modust:modust, $
           star:aar_star,full:aar_spec}

  return,model
END
