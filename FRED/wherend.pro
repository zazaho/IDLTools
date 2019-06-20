;;*****************************************************************************
;;*
;;*                     WHERE FUNCTIONS FOR A nD ARRAY
;;*
;;*****************************************************************************


FUNCTION whereND, bool, Nindex, COMPLEMENT=comp_nD, NCOMPLEMENT=Ncomp


  ;; Check arguments
  IF (N_PARAMS() LT 1) THEN BEGIN
    PRINT, "index[Nindex,NDim] = WHEREND(boolean[dim],Nindex,"
    PRINT, "                    COMPLEMENT=comp[Ncomp,Ndim],NCOMPLEMENT=Ncomp)"
    PRINT, "  use: tab[index[*,0],index[*,1],index[*,2]]"
    RETURN, 0./''
  ENDIF
  
  ;; Check the size
  siztab = SIZE(bool)
  Ndim = siztab[0]
 
  index_1D = WHERE(bool,Nindex,COMPLEMENT=comp_1D,NCOMPLEMENT=Ncomp)
  ;; Returns -1 if no TRUE.
  IF (Nindex EQ 0) THEN RETURN, -1
  IF (Ncomp EQ 0) THEN comp_nD = -1
  
  ;; Make the index array
  CASE Ndim OF
    0: BEGIN
      index_ND = 0
      comp_ND = 0
    END
    1: BEGIN
      index_ND = index_1D
      comp_ND = comp_1D
    END
    2: BEGIN
      siztab_x = siztab[1]
      index_ND = INTARR(Nindex,2)
      index_ND[*,1] = index_1D[*] / siztab_x
      index_ND[*,0] = index_1D[*] - index_ND[*,1] * siztab_x
      IF (Ncomp GT 0) THEN BEGIN
        comp_ND = INTARR(Ncomp,2)
        comp_ND[*,1] = comp_1D[*] / siztab_x
        comp_ND[*,0] = comp_1D[*] - comp_ND[*,1] * siztab_x
      ENDIF
    END
    3: BEGIN
      siztab_x = siztab[1]
      siztab_y = siztab[2]
      index_ND = INTARR(Nindex,3)
      index_ND[*,2] = index_1D[*] / ( siztab_x * siztab_y )
      index_temp = index_1D[*] - index_ND[*,2] * siztab_x * siztab_y
      index_ND[*,1] = index_temp / siztab_x
      index_ND[*,0] = index_temp - index_ND[*,1] * siztab_x
      IF (Ncomp GT 0) THEN BEGIN
        comp_ND = INTARR(Ncomp,3)
        comp_ND[*,2] = comp_1D[*] / ( siztab_x * siztab_y )
        comp_temp = comp_1D[*] - comp_ND[*,2] * siztab_x * siztab_y
        comp_ND[*,1] = comp_temp / siztab_x
        comp_ND[*,0] = comp_temp - comp_ND[*,1] * siztab_x
      ENDIF
    END
    ELSE: BEGIN
      PRINT, "WhereND does not handle dimensions greater than 3, yet."
      RETURN, 0./''
    END
  ENDCASE

  RETURN, index_ND


END
