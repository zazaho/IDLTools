function readfitscube,file

  fits_info, file,/SILENT,N_ext=n_ext,extname=extname

  extname=strupcase(strcompress(/remove_all,extname))

  ;; we assume this is always there
  cube = mrdfits(file,0,header)

  foundwave=0
  foundmask=0
  foundorder=0

  if n_ext ge 1 then begin
     ;; now hunt for a wavelength extension
     wave_ext_no = (where(extname eq 'WCS-TAB'))[0]
     if wave_ext_no ne -1 then begin
        wave = mrdfits(file,wave_ext_no,hwave)
        foundwave=1
     endif else begin
        wave_ext_no = (where(extname eq 'WAVE-TAB'))[0]
        if wave_ext_no ne -1 then begin
           wave = mrdfits(file,wave_ext_no,hwave)
           foundwave=1
        endif else begin
           ;; default order
           wave = mrdfits(file,1,hwave)
           foundwave=1
        endelse
     endelse
     
     ;; now hunt for a mask extension
     mask_ext_no = (where(extname eq 'MASK'))[0]
     if mask_ext_no ne -1 then begin
        mask = mrdfits(file,mask_ext_no)
        foundmask=1
     endif else begin
        ;; default order
        if n_ext ge 2 then begin
           mask = mrdfits(file,2)
           foundmask=1
        endif
     endelse
     
     ;; now hunt for a order extension
     order_ext_no = (where(extname eq 'ORDER'))[0]
     if order_ext_no ne -1 then begin
        order = mrdfits(file,order_ext_no)
        foundorder=1
     endif
     
  endif

  if foundwave then begin
     ;; make sure wave is a simple array
     ;; check routines for problems because of this change
     if size(wave,/tname) eq 'STRUCT' then wave = reform(wave.(0)) else wave = reform(wave)
  endif
     
  strng = "{cube:cube,header:header"
  if foundwave then strng=strng+",wave:wave,hwave:hwave"
  if foundmask then strng=strng+",mask:mask"
  if foundorder then strng=strng+",order:order"
  strng=strng+"}"
  
  foo=execute("out="+strng)

  return,out
  
end
