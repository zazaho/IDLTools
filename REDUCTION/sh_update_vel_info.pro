PRO sh_update_vel_info,newspd,oldspd
  
  kwd = ['TREFCOR1','TREFHEL1','TREFDOP1','TREFCOR2','TREFHEL2','TREFDOP2','TREFCOR3','TREFHEL3','TREFDOP3','INSTRA','INSTDEC','INSTROLL']

  header = newspd.header
  
  FOR i = 0,n_elements(kwd)-1 DO BEGIN
    status = read_fits_key(oldspd.header,kwd[i],old_value,old_comm)
    IF (status NE 2) THEN $
      header = write_fits_key(header,kwd[i],old_value,'R',old_comm,status)
  ENDFOR
  
  newspd.header = header
END
