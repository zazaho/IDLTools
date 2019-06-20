FUNCTION timmi_read_fits_array,file,Nimage=Nimage,xr=xrange,yr=yrange, $
                               max=max,start=start
  
  default,Nimage,0
  default,xr,[0,319]
  default,yr,[0,239]
  default,max,10
  default,start,1
  
  ;;We need to do several things
  ;; Open the file
  ;; Read the first header
  ;; Read each of the following extensions
  ;; Put them in an array
  ;; Convert the B-A to positives
  
  openr,lun,file,/get_lun
  foo = mrdfits(lun,0,header)
  asciitable = mrdfits(lun)
  i = 0
  WHILE (NOT EOF(lun)) AND (i LT max) DO BEGIN
    foo = execute('image_'+n2s(i)+' = float(mrdfits(lun))')
    i=i+1
  ENDWHILE
  close,lun
  free_lun,lun
  
  IF n_elements(image_0) NE 0 THEN BEGIN
    image = {TIMMI2_IMAGE,image:fltarr(xr[1]-xr[0]+1,yr[1]-yr[0]+1)}
    out   = {TIMMI2_IMAGE_ARRAY, $
             DATA:replicate(image,i*2)}
;;    showstruct,out
    FOR j=0,i-1 DO BEGIN
      foo=execute('out.data[2*j].image='+ $
                  'image_'+n2s(j)+'[xr[0]:xr[1],yr[0]:yr[1],1]-'+ $
                  'image_'+n2s(j)+'[xr[0]:xr[1],yr[0]:yr[1],0]')
      foo=execute('out.data[2*j+1].image=image_'+n2s(j)+ $
                '[xr[0]:xr[1],yr[0]:yr[1],1]')
    ENDFOR
  ENDIF
  return,out
  
END

