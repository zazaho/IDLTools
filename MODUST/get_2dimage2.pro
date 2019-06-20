pro get_2dimage2, file=file,emi=emi2
;
; Reads actual 2D image calculated using modust
;
; Intensity as stored in the modust output file `dust.2Dimage' is in  
; ergs/cm^2/sec/Hz/sr  and is converted in this routine to Jansky/sr
;
; --------------------------------------------------------------------------
;
@modust.com
;
if n_elements (file) eq 0 then file='dust.2Dimage'
print,'get_image 2D: ',file
;
; Open file for reading, get unique file unit UNIT with /GET_LUN
;    LUN = logical unit number

;	open file for reading
	OPENR, UNIT, file, /GET_LUN
	PRINT, 'Reading 2D image'

        READF, UNIT, nw
        wave2Di = dblarr(nw)
        READF, UNIT, wave2Di
        READF, UNIT, nx,ny
        dx      = dblarr(nx)
                ; 2D impact x grid in rstar
        dy      = dblarr(ny)
                ; 2D impact y grid in rstar     
        READF, UNIT, rstar_check
        READF, UNIT, dx
        READF, UNIT, dy
;
;       read actual 2D emerging intensities
;
        emi2D   = dblarr(nx,ny,nw)
;               = emerging intensities
        emi0    = dblarr(nx,ny)
;
        for k=0,nw-1 do begin
           READF, UNIT, k0, wave0
           if abs(wave0-wave2Di(k)) gt 0.0001 then stop,$
             'wave conflict while reading 2D image'
           READF, UNIT, emi0
           emi2D(*,*,k) = 1.d23*emi0
                        ; convert ergs/cm^2/sec/Hz/sr to Jansky/sr
        endfor
;
;	close file, return file unit with FREE_LUN
        FREE_LUN, UNIT
        print,'Ready reading 2D image'
;
        emi2=emi2d
        return
        end
;
;
        
