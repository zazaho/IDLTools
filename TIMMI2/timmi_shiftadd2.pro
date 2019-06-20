FUNCTION fff,in
  return,string(in,format='(F7.2)')
END

FUNCTION timmi_shiftadd2,file,Nimage=Nimage,skip=skip,journal=journal, $
                         interactive=interactive
  
  ;;Init
  default,Nimage,1000
  default,skip,0
  default,journal,'timmi_shiftadd2_journal.txt'
  default,interactive,0
  
  LastImage=Nimage+skip
  out = 0d0*dblarr(320,240)
;;  put the central pixel on the center OF the output image
  out[160,120]=1d0
  i = skip
  img_text=['abs(imgs[*,*,0]>0d0)','abs(imgs[*,*,0]<0d0)']
  
  datum = bin_date(systime())
  datum = strmid(string(datum[0],f='(I4.4)'),2,2)+ $
    string(datum[1],f='(I2.2)')+string(datum[2],f='(I2.2)')
  ;;End Init
  
  ;;For a journaling file
  get_lun,jourlun
  openw,jourlun,journal
  printf,jourlun,';;TIMMI_SHIFTADD2: Journal file: '+datum
  printf,jourlun,'openr,lun,"'+file+'",/get_lun'
  printf,jourlun,'foo = mrdfits(lun,0,header)'
  printf,jourlun,'asciitable = mrdfits(lun)'
  printf,jourlun,'out=0d0*dblarr(320,240)'
  printf,jourlun,';;;'
  
  ;;We need to do several things
  ;; Open the file
  ;; Read the first header
  ;; Read each of the following extensions
  ;; Put them in an array
  ;; Convert the B-A to positives
  
  openr,lun,file,/get_lun
  foo = mrdfits(lun,0,header)
  asciitable = mrdfits(lun)
  
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
      
  WHILE (NOT EOF(lun)) AND (i LT LastImage) DO BEGIN
    imgs = double(mrdfits(lun))
    printf,jourlun,'imgs=double(mrdfits(lun))'
    ;;Now we DO the thing FOR each image
    FOR j=0,1 DO BEGIN
      foo = execute('img='+img_text[j])
      
      shifts = timmi_fitshift(out,img)
      ;; Now we found the shift required
      print,'image: '+n2s(i*2+j)
      ;; shift the image to the right place
      img=timmi_shift(img,shifts[0],shifts[1])
      ;; Show it on the screen
      tvscl,img
      plots,[160-10,160+10,160,160,160],[120,120,120,120+10,120-10],/device
      empty
      IF (i+j) EQ 0 THEN out[160,120]=0d0
      out = out + img
      printf,jourlun,'out=out+timmi_shift('+img_text[j]+','+fff(shifts[0])+ $
        ','+fff(shifts[1])+') ;;image: '+n2s(i*2+j)
    ENDFOR
    i=i+1
  ENDWHILE
  close,lun
  free_lun,lun
  
  printf,jourlun,'close,lun'
  printf,jourlun,'free_lun,lun'
  close,jourlun
  free_lun,jourlun
  
  return,out
  
END
