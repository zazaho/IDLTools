;; Wrapper om dynadark met de rest om gewoon te kunnen doen:
;; aar = dynadaar(spd)
FUNCTION dynadaar,spd_in,noclean=noclean
  
  spd = spd_in
  spd=antimem(spd)
  spd=updowncal(spd)

  spd = dynadark( spd, /plot, /verb)

  spd=respcal(spd)

  spd=fluxcon(spd)
  spd=velcor(spd)
  aar=extract_aar(spd)
; now only take the band 2 data
  aar = select(aar, (aar.data.line EQ 5) OR $
               (aar.data.line EQ 6) OR $
               (aar.data.line EQ 7))
  
  IF NOT keyword_set(noclean) THEN BEGIN
    aar = cleanstruct(aar,/nod)
    aar = sh_weg(aar,/bands)
    
    speed = get_aot_speed(spd)
    
    IF (speed EQ 1) THEN $
      aar = sh_weg(aar,tint=17) $
    ELSE $
      aar = sh_weg(aar,tint=39)
    
  ENDIF
  return,aar
END
