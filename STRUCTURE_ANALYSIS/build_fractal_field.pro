                             ;PRO BUILD_FRACTAL_FIELD

;; this is the "astronomical" interface to GenerateFractal2D
;; ie it generates a field of sources in 2d using GenerateFractal2D
;; and then it does the conversion to an astronomical catalog/image that can
;; be treated with the regular auto correlation functions or the pdf
;; tools

pro build_fractal_field, $
   specfile=specfile, $
   tablefile=tablefile, $
   _extra=_extra

  ;; file where the parameters of the field to build are stored
  default,specfile,'fractal-field.par.info'
  default,tablefile,specfile+'.data'
  
;;;    PARAMETERS INITIATION
  dim = 0 ;;; DIMENSIONS OF THE DISTRIBUTION (2D or 3D) - FOR LATER EXPANSION INTO 2D
  fd = 0. ;;; FRACTAL DIMENSION (DEPENDENT ON "dim")
  Ndiv = 0  ;;; Number of length-divisions per mother-cube
  Nscb = 0  ;;; Number of total sub-cubes each mother-cube will be divided into
  Nran = 0 ;;; Number of randomly selected sub-cubes that will be themselves further divided (become mothers)
  length = 0. ;;; Original Length of the first Mother-Cube (generation 0)
  asec2pc = 0.   ;;; 
  pxsz = 0.      ;;;
  
;;;    PARAMETERS FILE READ OUT
  openr,lun, specfile,/get_lun
;;;    Build of a 2D or 3D Distribution
  readf,lun, dim
;;;    Input Fractal (3D or 2D) Dimension
  readf,lun, fd
;;;    Number of mother-cube length divisions into child-cubes
  readf,lun, Ndiv
;;;    Mother-Cube Original length in pixels (multiples of 16)
  readf,lun, length
;;;    pixel size in arcsec
  readf,lun, pxsz
;;;    arcsec to parsec conversion
  readf,lun, asec2pc
  close, lun
  free_lun,lun

  stararray = generatefractal2d( $
              _extra=_extra, $
              NDiv=ndiv, $
              NIterations=NIterations, $
              FractalDimension=fd, $
              InfoHeader=InfoHeader $
                               )
  
  ;; now we need to scale this to the sky in pixels.
  ;; we take a 10% margin
  minmin=min(stararray,max=maxmax)
  
  stararray_pixels = length*0.05 + length*0.9*(stararray - minmin)/(maxmax-minmin)
  
  openw,lun,tablefile,/get_lun
  printf,lun,InfoHeader
  printf,lun,stararray_pixels,format="(2F14.1)"
  close,lun
  free_lun,lun

END
