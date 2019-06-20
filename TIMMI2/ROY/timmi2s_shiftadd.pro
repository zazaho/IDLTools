@others
@timmi2_shift_procedures

FUNCTION timmi2s_shiftadd,basename,Nimage=Nimage,skip=skip,journal=journal,fits_out=fits_out,masterfile=masterfile

;;;(SH Jul 25 2001)
;;; We only find the optimal shift along the Y direction after
;;; removing the slope. After discussions with RvB we decided to use a
;;; tabulated dispersion relation. For which I leave room.
;;; See also timmi2s_disp_offset
  
  file = 'raw_'+strcompress(string(basename),/remove_all)+'_espe.fits'
  ;;Init
  default,Nimage,1000
  default,skip,0
  if not keyword_set(fits_out) then begin
     default,journal,'timmi2s_shiftadd_journal_'+strcompress(string(basename),/remove_all)+'.txt'
  endif else begin
     default,journal,'timmi2s_shiftadd_journal_'+strcompress(string(fits_out),/remove_all)+'.txt'
  endelse

  datum = bin_date(systime())
  datum = strmid(string(datum[0],f='(I4.4)'),2,2)+ $
    string(datum[1],f='(I2.2)')+string(datum[2],f='(I2.2)')
  ;;End Init
  
  ;;For a journaling file
  get_lun,jourlun
  openw,jourlun,journal
  printf,jourlun,';;TIMMI2S_SHIFTADD: Journal file: '+datum
  printf,jourlun,'Opened file ',file
  printf,jourlun,'Shifts applied to the chopped frames:'
  printf,jourlun,'           x               y      nod position'
  printf,jourlun,''

  LastImage=Nimage+skip
  i = skip
  img_text=[' imgs[*,*,0]','-imgs[*,*,0]']

  openr,lun,file,/get_lun
  foo = mrdfits(lun,0,header)
  asciitable = mrdfits(lun)

  out = 0d0*dblarr(320,240)
  out[*,120]=1d0 ;; to align the first chopped frame with the central row

    CASE 1 OF
    skip EQ 1: BEGIN
      foo = mrdfits(lun)
      printf,jourlun,'foo = mrdfits(lun)'
    END
    skip GT 1: BEGIN
      foo = mrdfits(lun,skip-1)
      printf,jourlun,'foo = mrdfits(lun,'+n2s(skip-1)+')'
    END
    ELSE: BEGIN 
    END
  ENDCASE

  iNod = 0 ;keeps track of the nod position we're in (1 or 2)
  nNod = 0 ;counts the number of times the nod position changes   

  WHILE (NOT EOF(lun)) AND (i LT LastImage) DO BEGIN
  ;WHILE (NOT EOF(lun)) AND (i LT 2) DO BEGIN
    imgs = double(mrdfits(lun))
    ;printf,jourlun,'imgs=double(mrdfits(lun))'

    foo = execute('img='+img_text[0])

    ;We want to multiply all images in nod position 1 with -1, so
    ;that when they are added to the images from nod 2, result = nod2 - nod1.
    ;we have nod amplitude = 10 arcsec, which is about 22 pixels. 
    ;If shift < 11 pixels, image is from nod 1,

    old_iNod=iNod

    img=-img
    if i eq 0 then begin
      iNod=1
      shifts = timmi2_1D_fitshift(out,img)
      nod_1_shift = shifts[1]
      print,'shifts in nod position 1: ',nod_1_shift
      out[*,120]=0d0
      out=out+timmi2_2D_shift(img,0,nod_1_shift)
      ;;the first image is now centered on y=120
    endif

    if i ne 0 then begin
      shifts = timmi2_1D_fitshift(out,img)
      if (abs(shifts[1]-nod_1_shift) le 11) then iNod=1
    endif
      
    ;;make images of nod position 2 positive again ...
    if ((i ne 0) and (abs(shifts[1]-nod_1_shift) ge 11)) then begin
      if iNod ne 2 then iNod = 2
      img = -img
      shifts = timmi2_1D_fitshift(out,img)
    endif

    if old_iNod ne iNod then Nnod = Nnod+1
    if ((old_iNod eq 2) and (iNod eq 1)) then Nnod = Nnod+1 ;;one extra
    if ((old_iNod eq 1) and (iNod eq 2) and (Nnod ge 3)) then Nnod = Nnod+1 ;;one extra, except first time
    
    print,'image: '+n2s(i+1),iNod,nNod

    ;;since shifts[0]=0 we essentially do a 1D shift (along y)
    ;tvscl,img
    ;plots,[160-10,160+10,160,160,160],[120,120,120,120+10,120-10],/device
    img=timmi2_2D_shift(img,shifts[0],shifts[1])
    ;tvscl,img
    ;plots,[160-10,160+10,160,160,160],[120,120,120,120+10,120-10],/device
    empty
    ;; Remove the dummy data
    if i ne 0 then out = out + img
    ;tvscl,out
    ;wait,0.5
    printf,jourlun,shifts[0],shifts[1],iNod
    i=i+1

  ENDWHILE

  ;;now find the approximate locations of the apertures
  ;;note that this is quite specific for the 10" nod step (which seems to correspond
  ;;to 25 pixels, hence the regions to search for maxima resp minima.
  apertures = total(out,1)
  dummy=max(apertures,ap1)  ;;the approximate location of the 'positive' spectrum
  dummy=min(apertures[(ap1-35):(ap1-15)],ap2)
  ap2=ap2+ap1-35
  if Nnod ge 2 then begin  ;;in case there is no Nod 2 position.
    dummy=min(apertures[(ap1+15):(ap1+35)],ap3)
    ap3=ap3+ap1+15
  endif
  if Nnod ge 2 then print,'Approximate locations of the apertures (pixel): ',ap2,ap1,ap3
  if Nnod eq 1 then print,'Approximate locations of the apertures (pixel): ',ap2,ap1

  ;;now, create "new" images centered on the apertures
  spec1=out[*,(ap1-15):(ap1+15)]
  spec2=-out[*,(ap2-15):(ap2+15)] ;;now all the 3 images
  if Nnod ge 2 then spec3=-out[*,(ap3-15):(ap3+15)] ;;are positive spectra (320x30 pixels)
  ;;do a shift and ad in a 30 by 30 pixel centered on the "dip" in the ozone band

  lspec1=spec1[117:147,*]
  lspec2=spec2[117:147,*]
  if Nnod ge 2 then lspec3=spec3[117:147,*]
  center=15  ;;'central' pixel, [15,15]
  size=10     ;;do the fit in [5:25,5:25], if size is small fit is not robust
  edgesize=2 ;;with 2 pixels of edge

;  lspec1=spec1[12:42,*]
;  lspec2=spec2[12:42,*]
;  lspec3=spec3[12:42,*]
;  center=15  ;;'central' pixel, [15,15]
;  size=9     ;;do the fit in [5:25,5:25], if size is small fit is not robust
;  edgesize=2 ;;with 2 pixels of edge

  printf,jourlun,''
  printf,jourlun,'shifts applied to the 2 negative apertures: '
  printf,jourlun,'           x               y'
  shifts = timmi2_2D_fitshift(lspec1,lspec2,center=center,size=size,edgesize=edgesize)
  if keyword_set(masterfile) then printf,masterfile,format='(f7.3,f7.3,$)',shifts[0],shifts[1]
  print,'shifts: ',shifts[0],shifts[1]
  printf,jourlun,shifts[0],shifts[1]
  xshift1=shifts[0]
  spec2 = timmi2_2D_shift(spec2,shifts[0],shifts[1])
  if Nnod ge 2 then begin 
    shifts = timmi2_2D_fitshift(lspec1,lspec3,center=center,size=size,edgesize=edgesize)
    ;;THIS IS VERY SPECIFIC: NOTE THAT THE 'SECOND' NEGATIVE APERTURE NORMALLY CONTAINS
    ;;MUCH LESS SIGNAL. SOMETIMES FOR FAINT SOURCES, THIS CAUSES A BAD FIT. THE X SHIFT
    ;;IN BOTH THE NEGATIVE APERTURES SHOULD BE ABOUT THE SAME, BUT WITH OPPOSITE SIGN.
    ;;(IT SHOULD ALSO BE SMALL, ABOUT 0.5 PIXEL). IF THE SECOND SHIFT IS LARGE, THEN DON'T
    ;;USE IT, USE -(THE FIRST SHIFT) INSTEAD. 
    alarm = 0
    if abs(xshift1+shifts[0]) ge 1 then begin 
      shifts[0] = -xshift1
      alarm = 1
    endif
    if keyword_set(masterfile) then begin
      if alarm eq 0 then printf,masterfile,format='(f8.3,f7.3,a0,$)',shifts[0],shifts[1],'    '
      if alarm eq 1 then printf,masterfile,format='(f8.3,f7.3,a0,$)',shifts[0],shifts[1],'  ! '
    endif
    print,'shifts: ',shifts[0],shifts[1]
    printf,jourlun,shifts[0],shifts[1]
    spec3 = timmi2_2D_shift(spec3,shifts[0],shifts[1])
  endif
  if Nnod ge 2 then out=spec1+spec2+spec3
  if Nnod eq 1 then out=spec1+spec2

  ;;last chip in timmi detector is bad
  out = out[0:303,*]
  
  close,lun
  free_lun,lun

  if not keyword_set(fits_out) then begin
     default,outputfilename,'sa_'+strcompress(string(basename),/remove_all)+'.fits'
  endif else begin
     default,outputfilename,'sa_'+strcompress(string(fits_out),/remove_all)+'.fits'
  endelse
  MWRFITS,out,outputfilename,/Create
  print,'wrote ',outputfilename
  printf,jourlun,''
  printf,jourlun,'wrote ',outputfilename

  close,jourlun
  free_lun,jourlun

  return,out
END
