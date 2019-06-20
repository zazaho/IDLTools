;; Simple wrapper to make psfiles with standard sizes
PRO sh_ps,f,filename=filename,view=view,font=font,aa=aa, $
          a4port=a4port,a4land=a4land,nott=nott,loadtable=loadtable, $
          fixbb=fixbb,thick=thick,charsize=charsize,pdf=pdf,_extra=_extra
  
  COMMON COMMON_sh_ps, old_r, old_g, old_b, table_loaded
  
;; This toggles if the device is open close  
  IF !d.name eq 'PS' THEN BEGIN
     f = (fstat(!d.unit)).name
     device,/close_file
     message,/info,'Closed file: '+f

     ;; post treatment
     ;; for convenience we deduce the dirname+filename+extension
     ;; assume we make a postscript file which could be called
     ;; /PPP/NNN.EEE where .EEE can be '.eps' '.ps' or ''

     f_dirname = file_dirname(f)
     f_basename = file_basename(f)

     f_parts=stregex(f_basename,'(.*)\.([^.]*$)',/fold_case,/extract,/subexpr)

     if n_elements(f_parts) eq 3 then begin
        f_name=f_parts[1]
        f_extension='.'+f_parts[2]
     endif else begin
        f_name=f_basename
        f_extension=''
     endelse
     	
     IF keyword_set(fixbb) THEN BEGIN
        ;; just try it. if the command is not available nothing too
        ;; bad will happen
        spawn,'psfix '+f_dirname+path_sep()+f_name+f_extension
        spawn,'psfixbb -ef '+f_dirname+path_sep()+f_name+f_extension, $
              exit_status=status

        ;; psfixxbb returned no error
        ;; the old file was called .ps
        ;; and a file .eps exist
        if (status eq 0) and $
           (strlowcase(f_extension) eq '.ps') and $
           (file_test(f_dirname+path_sep()+f_name+'.eps') eq 1) $
        then begin
           f_extension='.eps'
        endif
     ENDIF

     ;; do we convert to pdf?
     IF keyword_set(pdf) THEN BEGIN
        ;; test for some tools that we could use for converting the
        ;; (e)ps to pdf
        spawn,'which epstopdf',null,null,exit_status=epstopdf_status
        spawn,'which ps2pdf',null,null,exit_status=ps2pdf_status
        spawn,'which convert',null,null,exit_status=convert_status
        
        case 1 of
           epstopdf_status eq 0: begin
              spawn,'epstopdf '+ $
                    f_dirname+path_sep()+f_name+f_extension+ $
                    ' --outfile='+ $
                    f_dirname+path_sep()+f_name+'.pdf', $
                    exit_status=exit_status
           end
           ps2pdf_status eq 0: begin
              spawn,'ps2pdf '+ $
                    f_dirname+path_sep()+f_name+f_extension+ $
                    ' '+ $
                    f_dirname+path_sep()+f_name+'.pdf', $
                    exit_status=exit_status
           end
           convert_status eq 0: begin
              spawn,'convert '+ $
                    f_dirname+path_sep()+f_name+f_extension+ $
                    ' '+ $
                    f_dirname+path_sep()+f_name+'.pdf', $
                    exit_status=exit_status
           end
        endcase
        
        if exit_status eq 0 then begin
           file_delete,f_dirname+path_sep()+f_name+f_extension
           f_extension='.pdf'
        endif

     ENDIF

;     IF keyword_set(fixbb) THEN BEGIN
;        if f_extension eq '.pdf' then begin
;           ;; just try it. if the command is not available nothing bad
;           ;; will happen
;           spawn,'pdfcrop '+ $
;                 f_dirname+path_sep()+f_name+f_extension+ $
;                 ' '+ $
;                 f_dirname+path_sep()+f_name+f_extension
;        endif else begin
;           ;; just try it. if the command is not available nothing too
;           ;; bad will happen
;           spawn,'psfix '+f_dirname+path_sep()+f_name+f_extension
;           spawn,'psfixbb -ef '+f_dirname+path_sep()+f_name+f_extension, $
;                 exit_status=status
;
;           ;; psfixxbb returned no error
;           ;; the old file was called .ps
;           ;; and a file .eps exist
;           if (status eq 0) and $
;              (strlowcase(f_extension) eq '.ps') and $
;              (file_test(f_dirname+path_sep()+f_name+'.eps') eq 1) $
;           then begin
;              f_extension='.eps'
;           endif
;        endelse
;     ENDIF
;
     IF keyword_set(view) THEN BEGIN
        ;; test for some tools that we could use to view the result
        spawn,'which gv',null,null,exit_status=gv_status
        spawn,'which okular',null,null,exit_status=okular_status
        spawn,'which xdg-open',null,null,exit_status=xdg_open_status
        spawn,'which open',null,null,exit_status=open_status
        case 1 of
           gv_status eq 0: spawn,'gv -swap '+f_dirname+path_sep()+f_name+f_extension+'&'
           okular_status eq 0: spawn,'okular '+f_dirname+path_sep()+f_name+f_extension+'&'
           xdg_open_status eq 0: spawn,'xdg_open '+f_dirname+path_sep()+f_name+f_extension+'&'
           open_status eq 0: spawn,'open -swap '+f_dirname+path_sep()+f_name+f_extension+'&'
        endcase
     ENDIF
     set_plot,'x'
     IF table_loaded THEN BEGIN
        tvlct,old_r,old_g,old_b
     ENDIF
     cleanplot
     return
  ENDIF
  
  IF n_params() EQ 0 THEN BEGIN
     IF keyword_set(filename) THEN BEGIN
        f = filename
     ENDIF ELSE begin
        message,/info,'No filename given using default filename: idl.ps'
        f = 'idl.ps'
     ENDELSE
  ENDIF
  
  IF NOT keyword_set(font) THEN font = 'Times'
  
  CASE 1 OF
     keyword_set(a4port):suffix=',xsize=19.0,ysize=26.0,yoffset= 2.0,xoffset=1.0,/portrait'
     keyword_set(a4land):suffix=',xsize=26.0,ysize=19.0,yoffset= 28.0,xoffset=1.0,/landscape'
     keyword_set(aa): BEGIN
        suffix   =',xsize= 8.8,ysize= 8.8,yoffset=10.0,xoffset=5.0,/portrait'
        !p.region=[0.01,0.01,.99,.99]
        IF !p.charsize THEN tpchar = !p.charsize ELSE tpchar = 1.0
        IF !x.charsize THEN txchar = !x.charsize ELSE txchar = 1.0
        IF !y.charsize THEN tychar = !y.charsize ELSE tychar = 1.0
        !x.margin=[1.1*3*tpchar*txchar,0]
        !y.margin=[1.1*3*tpchar*tychar,0]
     END
     ELSE: suffix = ''
  ENDCASE
  
  IF NOT keyword_set(nott) THEN BEGIN
      !P.FONT = 1
      suffix = suffix+',set_font=font,/TT_FONT'
  ENDIF
  
  set_plot, 'ps'
  foo = execute('device,filename=f'+suffix+',_extra=_extra')
  message,/info,'Opened file: '+f
  
  IF n_elements(loadtable) eq 1 THEN BEGIN
      tvlct,old_r,old_g,old_b,/get
      loadct,loadtable,/silent
      table_loaded = 1
  ENDIF ELSE BEGIN
      table_loaded = 0
  ENDELSE
  
  IF keyword_set(thick) THEN BEGIN
     !p.thick=thick
     !x.thick=thick
     !y.thick=thick
  ENDIF
  
  IF keyword_set(charsize) THEN BEGIN
     !p.charsize=charsize
  ENDIF
  
END
