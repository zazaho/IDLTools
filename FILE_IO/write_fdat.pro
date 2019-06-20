PRO write_fdat,a,file,filename=fname,ncol=ncol,head=head,comment=comment, $
               sort=sort,_extra=_extra
  
  usage = "WRITE_FDAT: Usage: write_fdat,aar,'test.dat'[,file='test.dat',/head,comment=';;; ',ncol=2]"
  CASE n_params() OF 
    0: BEGIN
      print,usage
      return
    END
    1: BEGIN
      IF keyword_set(fname) THEN BEGIN
        file = fname
      ENDIF ELSE BEGIN
        print,usage
        return
      ENDELSE
    END
    ELSE: BEGIN 
      ;;
    END
  ENDCASE
  
  ;; Allow simple arrays to be written too
  IF NOT is_aar(a) THEN BEGIN
      IF keyword_set(sort) THEN BEGIN
          idx = sort(a[0,*])
          FOR i=0,n_elements(a[*,0])-1 DO BEGIN
              a[i,*] = a[i,idx]
          ENDFOR 
      ENDIF 
      openw,lun,file,/get_lun
      printf,lun,a
      close,lun
      free_lun,lun
      return
  ENDIF 

  IF keyword_set(sort) THEN BEGIN
      idx = sort(a.data.wave)
      FOR i=0,n_tags(a.data)-1 DO BEGIN
          a.data.(i) = a.data[idx].(i)
      ENDFOR 
  ENDIF 

  ;; Comment string
  default,comment,'#'
  
  ;; colums: wave flux stdev line sdir
  default,ncol,5
  CASE ncol OF
    2: BEGIN
      col_head = comment+' wave      flux'
      row_data = 'w[i],f[i]'
    END
    3: BEGIN
      col_head = comment+' wave      flux           stdev'
      row_data = 'w[i],f[i],s[i]'
    END
    4: BEGIN
      col_head = comment+' wave      flux          stdev           line'
      row_data = 'w[i],f[i],s[i],l[i]'
    END
    ELSE: BEGIN
      col_head = comment+' wave      flux           stdev          line           sdir'
      row_data = 'w[i],f[i],s[i],l[i],r[i]'
    END
  ENDCASE
    
  w = a.data.wave
  f = a.data.flux
  s = a.data.stdev
  l = a.data.line
  r = a.data.sdir
  
  nw = n_elements(w)
  
  ;;now write ascii table
  openw,lun,file,/get_lun
  
  if keyword_set(head) then begin
    printf,lun,comment+' object: ',object(a)
    printf,lun,comment+' time: ',systime()
    printf,lun,comment+' number of values: ',nw
    printf,lun,col_head      
  ENDIF
  
  for i = 0,nw-1 do begin
    foo = execute('printf,lun,_extra=_extra,'+row_data)
  endfor
  
  close,lun
  free_lun,lun

end
