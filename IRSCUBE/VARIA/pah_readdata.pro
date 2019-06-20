function pah_readdata, $
   region=region, $
   rlabel=rlabel, $
   measurements=measurements, $
   mlabels=mlabels, $
   ulabels=ulabels

  if n_elements(rlabel) ne n_elements(region) then rlabel = strcompress(string(region))
  if n_elements(mlabels) ne n_elements(measurements) then mlabels = strcompress(string(measurements))
  case 1 of
     n_elements(ulabels) eq n_elements(measurements): break
     n_elements(ulabels) eq 1: ulabels = make_array(n_elements(measurements),value=ulabels[0])
     else: ulabels = make_array(n_elements(measurements),value='')
  endcase
  
  struct = {name:region,label:rlabel}
  
  ;; for each measurement, make a sub structure with the following info
  ;; data
  ;; stdev
  ;; label in tex make-up
  ;; units in tex make-up
  ;; the substructure is called V_+measurement

  for mm=0,n_elements(measurements)-1 do begin
     measurement = measurements[mm]
     fname = region+"/"+"im"+measurement+"_"+region+".fits"
     fname_stddev = region+"/"+"im"+measurement+"_"+region+"_stddev.fits"
     data = readfits(fname,header)
     stdev= readfits(fname_stddev)

     ;; for the first measurement add the header to the global
     ;; structure and make x, y, ra and dec variables
     if mm eq 0 then begin
        nx = n_elements(data[*,0])
        ny = n_elements(data[0,*])
        foo = lindgen(nx,ny)
        x = foo mod nx
        y = foo  / nx
        xyad,header,x,y,ra,dec
        struct=sh_add_tag(struct,"header",header)
        struct=sh_add_tag(struct,"X",x)
        struct=sh_add_tag(struct,"Y",y)
        struct=sh_add_tag(struct,"RA",ra)
        struct=sh_add_tag(struct,"DEC",dec)
     endif

     ;; for the first measurement add the PACS 100 image (fudge)
     if mm eq 0 then begin
        pacs100name = region+"/impacs100_"+region+".fits"
        pacs100 = readfits(pacs100name)

        ;; here we convert PACS 100 values into the same units that fred has
        ;; for strengths (W/m2/sr)
        ;; At the basis we have units of MJy/sr
        delta_nu_pacs100 = 5.09d11                   ;; Hz

        Wm2_per_MJy_pacs100 = 1d6 * 1d-26 * delta_nu_pacs100

        pacs100 = pacs100 * Wm2_per_MJy_pacs100 

;        ;; here we convert PACS 100 values into the same units that fred has
;        ;; for strengths (LSun/pc^2)
;        ;; At the basis we have units of MJy/sr
;        dist_lmc_pc = 50d3                        ;; pc
;        au = 1.49597870691d11                     ;; Astronomical unit [SI]
;        pc = 3.085677582d16                       ;; Parsec [SI]
;        W_per_LSun = 3.82d26                      ;; Solar Luminosity [SI]
;        delta_nu_pacs100 = 5.09d11                   ;; Hz
;
;        ;; which means that one steradian contains this much pc^2
;        pc2_per_sr_lmc = dist_lmc_pc^2
;
;        Wm2_per_MJy_pacs100 = 1d6 * 1d-26 * delta_nu_pacs100
;
;        LSun_per_MJy_pacs100 = Wm2_per_MJy_pacs100 * 4d0*!dpi*(dist_lmc_pc*pc)^2 / W_per_LSun
;
;        pacs100 = pacs100 * LSun_per_MJy_pacs100 / pc2_per_sr_lmc

        var = {data:pacs100,stdev:pacs100*0.05,label:'PACS 100',units:'LSun/pc2'}
        struct=sh_add_tag(struct,"V_PACS100",var)
     endif

     var = {data:data,stdev:stdev,label:mlabels[mm],units:ulabels[mm]}
     struct=sh_add_tag(struct,"V_"+measurement,var)
  endfor

  return,struct

end
