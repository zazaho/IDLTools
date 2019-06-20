pro extract_Q_values
;
; --------------------------------------------------------------------------
;
file='dust.out'
newfile=0
dpc=10.0
old = 0
print,'get_dustout:',file

; Open file for reading, get unique file unit UNIT with /GET_LUN
;    LUN = logical unit number

;	open file for reading
	OPENR, UNIT, file, /GET_LUN
	PRINT, 'Reading dust properties and emergent flux'

        IF newfile eq 0 THEN BEGIN
	   READF, UNIT, nf, ng, naa, nr
	   waves        = dblarr(nf)
;                       = wavelength in micron
	   READF, UNIT, waves
	   emf          = dblarr(nf)
           READF, UNIT, emf
           emf          = 1.d23*emf
;                       = convert ergs/cm^2/sec/Hz to Jansky
           emf_tstar    = dblarr(nf)
           READF, UNIT, emf_tstar
           emf_tstar    = 1.d23*emf_tstar
;                       = convert ergs/cm^2/sec/Hz to Jansky
;
;          scale flux to the right distance
           print,'distance =',dpc,' pc'
           if dpc ne 10 then begin
              print,' scaling flux to',dpc,' parsec'
              emf       = (10./dpc)^2 * emf
              emf_tstar = (10./dpc)^2 * emf_tstar
           endif
;
           rd           = dblarr(nr)
           if old eq 0 then READF, UNIT, rd
;
        ENDIF ELSE IF newfile eq 1 THEN BEGIN
           READF, UNIT, nf_new, ng_new, naa_new, nr_new
           waves_new    = dblarr(nf_new)
           emf_new      = dblarr(nf_new)

           if sed.addmodels eq -1 then begin
              waves_comp = dblarr(nf_new,10)
              emf_comp   = dblarr(nf_new,10)
           endif 

           READF, UNIT, waves_new
           READF, UNIT, emf_new
           emf_new      = 1.d23*emf_new
;                       = convert ergs/cm^2/sec/Hz to Jansky
;
;          scale flux to the right distance
           if dpc ne 10. then begin
              emf_new   = (10./dpc)^2 * emf_new
           endif
;
           sed.addmodels = sed.addmodels+1

           waves_comp(0:nf_new-1,sed.addmodels) = waves_new(0:nf_new-1)
           emf_comp(0:nf_new-1,sed.addmodels)   = emf_new(0:nf_new-1)
;
;          close file, return file unit with FREE_LUN
           FREE_LUN, UNIT
	   RETURN
	ENDIF        

        if n_elements(xyrange) ne 0 then begin
           xybracket    = dblarr(6)
           xybracket(0) = min(waves)
           xybracket(1) = max(waves)
           xybracket(2) = min(emf)
           xybracket(3) = max(emf)
           xybracket(4) = 0.         ; given x-range is linear (1 = log)
           xybracket(5) = 0.         ; given y-range is linear (1 = log)
           print,xybracket
        endif

        na         = dblarr(ng)
        dust_name  = strarr(ng)
        dust_abun  = dblarr(ng)
        dust_rho   = dblarr(ng)
        dust_alpha = dblarr(ng)
        a_dust     = dblarr(naa,ng)
	qabs       = dblarr(nf,naa,ng)
        tdust      = dblarr(nr,naa,ng)
        x          = dblarr(nf)
        y          = dblarr(nr)
        xdust_name = ''
        FOR ig = 0,ng-1 DO BEGIN
	   READF, UNIT, xdust_name
           dust_name(ig) = xdust_name
           READF, UNIT, xna,xdust_abun,xdust_rho,xdust_alpha
           na(ig) = xna
	   FOR ia = 0,xna-1 DO BEGIN
	      READF, UNIT, xa_dust
              a_dust(ia,ig) = xa_dust
	      READF, UNIT, x
              READF, UNIT, y
              qabs(0:nf-1,ia,ig)  = x(0:nf-1)
              tdust(0:nr-1,ia,ig) = y(0:nr-1)
	   ENDFOR
        ENDFOR
	
;	close file, return file unit with FREE_LUN
        FREE_LUN, UNIT
        print,'ready reading'

   filename = 'Q.dat'
   openw, unit_out, filename, /get_lun
     FOR ig = 0,ng-1 DO BEGIN
       printf,unit_out,'DUST SPECIES:',dust_name(ig)
       FOR ia = 0,xna-1 DO BEGIN
         printf,unit_out,'GRAIN SIZE:',a_dust(ia,ig)
         FOR ii = 0,nf-1 DO BEGIN
           printf,unit_out,waves(ii),qabs(ii,ia,ig)
         ENDFOR
       ENDFOR
     ENDFOR

   free_lun, unit_out

   filename = 'T.dat'
   openw, unit_out, filename, /get_lun
     FOR ig = 0,ng-1 DO BEGIN
       printf,unit_out,'DUST SPECIES:',dust_name(ig)
       FOR ia = 0,xna-1 DO BEGIN
         printf,unit_out,'GRAIN SIZE:',a_dust(ia,ig)
         FOR ii = 0,nr-1 DO BEGIN
           printf,unit_out,rd(ii),tdust(ii,ia,ig)
         ENDFOR
       ENDFOR
     ENDFOR

   free_lun, unit_out
END






