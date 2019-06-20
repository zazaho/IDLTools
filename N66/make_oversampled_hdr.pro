function make_oversampled_hdr,hdr_in,factor_in

  hdr=hdr_in
  factor = double(factor_in)
  naxis1 = sxpar(hdr,'naxis1',comment=naxis1_comment)
  naxis2 = sxpar(hdr,'naxis2',comment=naxis2_comment)
  
  fine_hdr = hdr

  extast,hdr,astr,noparams
  fine_astr=astr
  fine_astr.crpix = astr.crpix*factor
  case noparams of
     1: begin
        fine_astr.cdelt=astr.cdelt/factor
     end
     2: begin
        fine_astr.cd=astr.cd/factor
     end
     else: begin
        message,'no recognised astrometry in header'
        return,hdr
     end
  endcase
  putast,fine_hdr,fine_astr
  
  sxaddpar,fine_hdr,'NAXIS1',naxis1*factor,naxis1_comment
  sxaddpar,fine_hdr,'NAXIS2',naxis2*factor,naxis2_comment

  return,fine_hdr
end
