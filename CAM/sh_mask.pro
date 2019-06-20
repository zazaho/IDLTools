;+
; sh_mask
; routine to mask data in a raster_pds
;
; usage: sh_mask,raster[,pixel,range][,/interactive]
; where
;   pixel = [x,y]
;   range = [f1,f2]
;
; if /interactive is set than the data are plotted and the user can
; interactively select which data to mask.
; after the interactive stage the commandliness syntax is echo to
; execute the masking no interactively
;-
PRO sh_mask_apply_range,mask,pixel,range
  mask[pixel[0],pixel[1],range[0]:range[1]] = mask[pixel[0],pixel[1],range[0]:range[1]] OR 1 
END 

PRO sh_mask,raster,pixel,range,interactive=interactive

  default,interactive,0

  IF interactive THEN BEGIN
      print,'SH_MASK: select region to  
      ;; do a lot of trick here that we dont know yet
      
  ENDIF ELSE BEGIN
      IF (n_elements(pixel) NE 2) OR (n_elements(range) NE 2)) THEN BEGIN
          doc_library,'sh_mask'
          return
      ENDIF
      raster.mask = sh_mask_apply_range,raster.mask,pixel,range
  ENDELSE
END

  
