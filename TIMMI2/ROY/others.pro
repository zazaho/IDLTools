PRO outmes, message, infow = infow
IF NOT keyword_set(infow) THEN print, message $
  ELSE widget_control, infow, set_value = message, /append
END

;;(SH Jul 25 2001)
;;Dispersion relation
FUNCTION timmi2s_disp_offset,dy,coeff=coeff
  ;; as determined by SH, check please and also the sign!
  default,coeff,2d0/1d2

  return,coeff*dy
END

FUNCTION fff,in
  return,string(in,format='(F7.2)')
END

FUNCTION n2s,in,format=format
  default,format,''
  return,strtrim(string(format=format,in),2)
END

PRO default,var,value
  IF n_elements(var) EQ 0 THEN var=value
END
