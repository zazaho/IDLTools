function fixmopra_sliding_median,array,width
  
  narray = n_elements(array)

  array_long = [reverse(array),array,reverse(array)]
  median_long = median(array_long,width)

  return,median_long[narray:narray*2-1]
end
  
pro fixmopra,files, $
             band3c=band3c, $
             remove_first=remove_first, $
             remove_last=remove_last, $
             outdir=outdir, $
             plot=plot, $ $
             domore=domore, $
             _extra=_extra
  
  default,remove_first,8
  default,remove_last,0
  default,outdir,'./defringed'
  spawn,'install -d '+outdir

  case 1 of
     size(band3c,/tname) eq 'STRUCT': begin
        b3c=band3c
     end
     size(band3c,/tname) eq 'STRING': begin
        b3c = sh_read_faar(band3c)
     end
     else: begin
        b3c = sh_read_faar('band3c.fits')
     end
  endcase
  
  for i=0,n_elements(files)-1 do begin

     dir  =file_dirname (files[i])
     bname=file_basename(files[i])
     
     fitsstruct = readfits(files[i],h,/silent)
     nj = n_elements(fitsstruct[*,0,0])
     nk = n_elements(fitsstruct[0,*,0])
     nl = n_elements(fitsstruct[0,0,*])
     
     outfitsstruct = make_array(nj, $
                                nk, $
                                nl-remove_last-remove_first, $
                                value=0d0)

     x = sxpar(h,'CRVAL1')+sxpar(h,'CDELT1',/silent)*dindgen(nl)
     xx = x[remove_first:(nl-1-remove_last)]
     
     hh = h
     sxaddpar,hh,'CRPIX1',sxpar(h,'CRPIX1')-remove_first
     
     
     for k=0,nk-1 do begin
        for j=0,nj-1 do begin
           
           ;; remove some bogus points
           yy = reform(fitsstruct[j,k,remove_first:(nl-1-remove_last)])
           
           ;; now call the function to defringe
           corrected_yy = mopra_defringe(yy,b3c,_extra=_extra)

           if keyword_set(domore) then begin
              corrected_yy = corrected_yy-fixmopra_sliding_median(corrected_yy,31)
           endif

           outfitsstruct[j,k,*] = corrected_yy

           if keyword_set(plot) then begin
              pl,xx,yy,ps=0,_extra=_extra
              pl,xx,corrected_yy,/opl,ps=0,_extra=_extra
           endif

        endfor
     endfor

     writefits,outdir+'/'+bname,outfitsstruct,hh
           
  endfor

end
