function copy_header_params,header_in,sourcefile=sourcefile

  header=header_in 

  ;; blank lines are annoying
  idx=where(stregex(header,'[a-z]',/fold_case,/boolean),cnt)
  if cnt ne 0 then header=header[idx]
  
  sourceheader=headfits(sourcefile,/silent)
  
  keywords=['BKG','SIGMABKG','BUNIT','WAVELNTH','OBJECT','ZUNITS','JANSCALE','LINE']
  for i=0,n_elements(keywords)-1 do begin
     val=sxpar(sourceheader,keywords[i],comment=cmnt,count=cnt)
     if cnt ne 0 then begin
        if n_elements(cmnt) eq 0 then cmnt=''
        sxaddpar,header,keywords[i],val[0],cmnt[0]
     endif
  endfor
  
  return,header
end
