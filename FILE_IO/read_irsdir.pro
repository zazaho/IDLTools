;; read a set of IRS fits files from a directory into an AAR
function read_irsdir,dirname
  bands=['SL1','SL2','SL3','LL1','LL2','LL3']

  for i=0,n_elements(bands)-1 do begin
     files=file_search(dirname+'/*_'+bands[i]+'.fits')
     if files[0] ne "" then begin
        img=mrdfits(files[0],0,header)
        spec=sh_define_aar(len=n_elements(img[0,*]))
        spec.data.wave=reform(img[0,*])
        spec.data.flux=reform(img[1,*])
        spec.data.stdev=reform(img[2,*])
        spec.data.line=i+1

        ;; head work
        sxaddpar,header,'TELESCOPE','SPITZER'
        sxaddpar,header,'INSTRUMENT','IRS'
        sxaddpar,header,'OBSERVER',sxpar(header,'OBSRVR')
        sxaddpar,header,'ATTRA',sxpar(header,'RA_SM')
        sxaddpar,header,'ATTDEC',sxpar(header,'DEC_SM')
        sxaddpar,header,'OBJECT',sxpar(header,'OBJECT')+' ('+sxpar(header,'RA_HMS')+';'+sxpar(header,'DEC_DMS')+')'
        spec.header=string(header,format='('+strtrim(n_elements(header))+'A80)')

        foo=execute(bands[i]+"=spec")
        
    endif
  endfor

  return,sh_combine(SL1,SL2,SL3,LL1,LL2,LL3)

end
