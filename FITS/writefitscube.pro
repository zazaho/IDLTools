pro writefitscube,struct,file,filename=filename,usemask=usemask, $
                  nofixheader=nofixheader
  
  if n_params() eq 1 then file = filename
  if not keyword_set(usemask) then usemask=0
  
  cube = struct.cube
  header=struct.header

  if where(tag_names(struct) eq 'MASK') ne -1 then begin
     has_mask=1
     mask = struct.mask
  endif else begin
     has_mask=0
  endelse

  if (has_mask eq 1) and (usemask ne 0) then begin
     collapsex = total(mask,2)
     collapsey = total(mask,1)
     xmin = min(where(collapsex ne 0),max=xmax)
     ymin = min(where(collapsey ne 0),max=ymax)
     cube = cube[xmin:xmax,ymin:ymax,*]
     mask = mask[xmin:xmax,ymin:ymax]
     sxaddpar,header,"crpix1",sxpar(header,"crpix1")-xmin
     sxaddpar,header,"crpix2",sxpar(header,"crpix2")-ymin
  endif

  if not keyword_set(nofixheader) then begin
     ;; this is some logic to make sure the header is complete
     equinox = sxpar(header,'EQUINOX')
     if (equinox eq 0) or (not finite(equinox)) then sxaddpar,header,'EQUINOX',2000.
     epoch = sxpar(header,'EPOCH')
     if (epoch eq 0) or (not finite(epoch)) then sxaddpar,header,'EPOCH',2000.
  endif
  
  mwrfits,float(cube),file,header,/create
  
  if where(tag_names(struct) eq 'WAVE') ne -1 then begin
     mwrfits,{WAVELENGTH:transpose(float(struct.wave))},file,struct.hwave
  endif
  
  if where(tag_names(struct) eq 'MASK') ne -1 then begin
     sxaddpar,hmask,"EXTNAME","MASK"
     mwrfits,mask,file,hmask
  endif
  
  if where(tag_names(struct) eq 'ORDER') ne -1 then begin
     sxaddpar,horder,"EXTNAME","ORDER"
     mwrfits,struct.order,file,horder
  endif

end
