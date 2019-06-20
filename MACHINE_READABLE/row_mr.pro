FUNCTION row_mr,mr,row

  IF NOT is_mr(mr) THEN BEGIN
      message,'the input structure is not valid',/inform
      return,0
  ENDIF

  default,row,0
  row=fix(row[0])

  IF (row LT 0) OR (row GE n_elements(mr.data.(0))) THEN BEGIN
      message,n2s(row)+' is out of range, valid range: [0,'+ $
              n2s(n_elements(mr.data.(0))-1)+']', $
              /informational
      return,0
  ENDIF

  return,select_mr(mr,where=row)

END
