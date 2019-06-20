;; We want to call timmi2s_shiftadd for all files listed in listname
;; Syntax: 10330704 on one line in listname will do the stuff for
;; raw_10330704_espe.fits
;; Then we will align all spectra with the first spectrum in the inputlist
;; doing a shift and add. In this, way, the atmospheric lines are all in the
;; same position and this should remove the fringing.

@timmi2_2D_shift

PRO timmi_all,listname
  
  ;; list is a structure that contains all stuff in inputlist
  list = read_ascii(listname)
  ;; x is the first (and only) field in this structure, an array of numbers
  x=list.field1
  ;; an array of Nsources numbers ...
  Nsources=n_elements(x)
  print,'there are ',string(format='(I0)',Nsources),' sources in ',listname

  ;;making the master journal file
  masterjournal = 'timmi2s_shiftadd_master_journal_'+strcompress(string(listname),/remove_all)+'.txt'
  datum = bin_date(systime())
  datum = strmid(string(datum[0],f='(I4.4)'),2,2)+ $
    string(datum[1],f='(I2.2)')+string(datum[2],f='(I2.2)')
  get_lun,masterlun
  openw,masterlun,masterjournal
  printf,masterlun,';;TIMMI2S_SHIFTADD: Master journal file: '+datum
  printf,masterlun,'Opened file ',listname
  printf,masterlun,''
  printf,masterlun,'Shifts applied to the spectra: '
  printf,masterlun,'source        dx1    dy1     dx2    dy2   !shift2  delta X      delta Y'
  printf,masterlun,''

  for q=0,(Nsources-1) do begin
    ;;source is a string that has the 'basename' of the exposure in it
    source=string(format='(I0)',x[q])
    print,'doing timmi2s_shiftadd(',source,')'
    ;;do the actual shift and add.

    printf,masterlun,format='(a0,a0,$)','sa2_',source
    if (q eq 0) then begin
      ;the first spectrum will be the reference for aligning the sky lines
      reference=timmi2s_shiftadd(source,masterfile=masterlun)
    endif else begin
      dummy=timmi2s_shiftadd(source,masterfile=masterlun)
    endelse
    if (q eq 0) then begin
      ref_shifted=timmi2_2D_shift(reference,0.25,0)
      align_spectra,ref_shifted,reference,source,masterlun
      ;;shifting the reference image with the average shift in x, so all images
      ;;have undergone the approximately the same amount of resampling.
    endif
    if (q ne 0) then begin
      align_spectra,ref_shifted,dummy,source,masterlun
    endif
  endfor

  close,masterlun
  free_lun,masterlun
END
