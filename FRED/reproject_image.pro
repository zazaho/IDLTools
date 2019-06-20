;;*****************************************************************************
;;*
;;*           REPROJECT AN IMAGE ON ANOTHER GRID WITH A DIFFERENT PFOV
;;*
;;*****************************************************************************


FUNCTION reproject_image, image, pfov, pfov_ref, NO_NORMALIZATION=nonorm

  ;; Inputs
  ;;-------
  IF (N_PARAMS() LT 3) THEN BEGIN
    PRINT, "newim[pfov_ref] = REPROJECT_ARRAY(image[pfov],PFOV,PFOV_ref," $
         + "/NO_NORMALIZATION)"
    RETURN, !VALUES.D_NaN
  ENDIF

  ;; Interpolate
  ;;------------
  fact = DOUBLE(pfov)/pfov_ref
  sizim = SIZE(image)     &   Nx = sizim[1]      &   Ny = sizim[2]
  Nx_ref = FIX(Nx*fact)   &   Ny_ref = FIX(Ny*fact)
  x = DINDGEN(Nx_ref)/fact
  y = DINDGEN(Ny_ref)/fact
  reproj = INTERPOLATE(image,x,y,/GRID,CUBIC=-0.5)

  ;; Normalization (surface brightness)
  ;;-----------------------------------
  IF (NOT KEYWORD_SET(nonorm)) THEN BEGIN
    photomIN = TOTAL(image)/(Nx*Ny)
    photomOUT = TOTAL(reproj)/(Nx*Ny)/fact^2
    reproj *= photomIN/photomOUT
  ENDIF

  RETURN, reproj

END
