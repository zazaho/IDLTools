  ;;; This is stolen (taken) from get_dustout.pro

pro read_multi_get_dustout, file, wave=wave, star=star, spec=spec,nwave=nwave
  
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
          message,'File not found:'+file
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

FUNCTION read_multi,dir,distance=distance

  default,dir,'./'
  default,distance,1000.

  IF strmid(dir,0,1,/reverse_offset) NE '/' THEN dir = dir+'/'
  
;; Now read the output files
  read_multi_get_dustout, dir+'dust1.out', wave=wave1, star=star1, spec=spec1,nwave=nwave1
  read_multi_get_dustout, dir+'dust2.out', wave=wave2, star=star2, spec=spec2,nwave=nwave2
  read_multi_get_dustout, dir+'dust.out' , wave=wave , star=star , spec=spec ,nwave=nwave

  ;; Now correct for the distance
  if distance ne 10 then begin
      star       = (10./distance)^2 * star
      spec       = (10./distance)^2 * spec
      spec1      = (10./distance)^2 * spec1
      spec2      = (10./distance)^2 * spec2
  endif
;

  aar_star = sh_define_aar(len=nwave)
  aar_spec = sh_define_aar(len=nwave)
  aar_spec1= sh_define_aar(len=nwave1)
  aar_spec2= sh_define_aar(len=nwave2)

  aar_star .data.wave = wave
  aar_spec .data.wave = wave
  aar_spec1.data.wave = wave1
  aar_spec2.data.wave = wave2
  aar_star .data.flux = star 
  aar_spec .data.flux = spec 
  aar_spec1.data.flux = spec1
  aar_spec2.data.flux = spec2

  dust1   = string(read_binary(dir+'dust1.dat'))
  dust2   = string(read_binary(dir+'dust2.dat'))
  modust1 = string(read_binary(dir+'modust1.inp'))
  modust2 = string(read_binary(dir+'modust2.inp'))


;; Now combine everything in a single structure:
  model = {directory:dir,distance:distance,dust1:dust1, $
           dust2:dust2,modust1:modust1,modust2:modust2, $
           star:aar_star,full:aar_spec,shell1:aar_spec1,shell2:aar_spec2}

  return,model
END




