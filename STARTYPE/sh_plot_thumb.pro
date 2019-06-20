PRO sh_plot_thumb,files,start=start,npage=npage,nrow=nrow, $
                  ncol=ncol,prefix=prefix,test_pah=test_pah, $
                  _extra=_extra
  on_error,0

  default,npage,4
  default,nrow,3
  default,ncol,3
  default,prefix,''
  default,start_index,0L
  default,test_pah,0

  IF n_elements(files) EQ 0 THEN files = get_files(/num,_extra=_extra)
  
  nplots = ncol*nrow*npage
  
  nf=n_elements(files)
  
; We want to enumerate the psfiles in order
; number of psfiles that will be made:
  num_ps = (nf / nplots)+1
; Needed width for the enumeration:  
  wform_ps = n2s(ceil(alog10(num_ps+1)))
  psnum = 0
  
  ;; Now start a new ps files
  set_plot,'ps'
  
  i = start_index
  FOR j=start_index,nf-1 DO BEGIN
      
      aar = read_fsp(files[j],_extra=_extra)
      
      do_plot = 1
      IF test_pah THEN BEGIN
          IF NOT has_pah(aar) THEN BEGIN
              do_plot = 0
         ENDIF
     ENDIF
     
     IF do_plot THEN BEGIN
         IF i MOD nplots EQ 0 THEN BEGIN
             psnum = psnum+1
;      psfile_1=prefix+strmid(strtrim(string(10000+i),2),1,4)+'.ps'
             psfile_1=prefix+n2s(psnum,format='(I'+wform_ps+'.'+wform_ps+')')+'.ps'
             device,file=psfile_1,/landscape
             !p.multi=[0,ncol,nrow]
         ENDIF
         
         pl,aar,/b,/w,title='+object:  '+files[i],/autoy,_extra=_extra
         
         IF (i MOD ncol*nrow) EQ 0 THEN BEGIN
             xyouts,.5,.01,'page '+n2s(1+i / (ncol*nrow)),/normal,align=0.5
         ENDIF
         
         IF i MOD nplots EQ nplots-1 THEN BEGIN
             device,/close
             print,'Made psfile: '+prefix+$
               n2s(psnum,format='(I'+wform_ps+'.'+wform_ps+')')+'.ps'
         ENDIF
         i = i+1
     ENDIF  
 ENDFOR
 
 device,/close
 set_plot,'x'
END
