FUNCTION fff,in
  return,string(in,format='(F7.2)')
END

;; To do a shift and add on timmi2 image data
FUNCTION timmi2_shiftadd,dir,nods=nods,journal=journal,box=box, $
                         ref=ref,filename=filename,noplot=noplot, $
                         beam=beam,auto=auto
  
  
  ;;Init
  foo = strsplit(dir,'/',/extract)
  obs_name= foo[n_elements(foo)-1]

  default,journal,obs_name+'_shiftadd_journal.txt'
  default,filename,obs_name+'_shiftadd_result'
  shifts = make_array(2000,2,value=0d0)
  idx_shift = 0
  default,noplot,0
  default,box,30
  default,gaus,0
  default,inter,0

  IF n_elements(ref) NE 0 THEN BEGIN
      use_ref=1
  ENDIF ELSE BEGIN
      use_ref=0
      ref = 0d0*dblarr(320,240)
      ;;  put the central pixel on the center OF the output image
      ref[160,120]=1d0
  ENDELSE

  IF n_elements(nods) EQ 0 THEN BEGIN
      ;; find the nod positions from the subdirectories
      nods =  findfile(dir,count=cnt)
      ;; Only take those 
      nods = nods[where(strcmp(nods,'00',2) EQ 1)]
      nods=fix(nods)
  ENDIF

  datum = bin_date(systime())
  datum = strmid(string(datum[0],f='(I4.4)'),2,2)+ $
    string(datum[1],f='(I2.2)')+string(datum[2],f='(I2.2)')
  ;;End Init
  
  ;; First read in the data cubes and stack the images per nod position
  FOR i=0,n_elements(nods)-1 DO BEGIN
      readplanes,dir,nods[i],stack,cube,tint
      print,string(format='("Nod:",I3," #frames:",I3," Tint:",F9.2)', $
                   nods[i],n_elements(cube[0,0,*]),tint)
      foo=execute('stack'+n2s(nods[i])+'=stack')
      foo=execute('cube'+n2s(nods[i])+'=cube')
      foo=execute('tint'+n2s(nods[i])+'=tint')
      foo=execute('Nframes'+n2s(nods[i])+'=n_elements(cube[0,0,*])')
  ENDFOR 

  ;; Nod make total per nod position
  stack_pos1 = stack1*0d0
  stack_pos2 = stack_pos1
  tint_pos1 = 0d0
  tint_pos2 = 0d0
  Nframes_pos1 = 0d0
  Nframes_pos2 = 0d0

  FOR i=0,n_elements(nods)-1 DO BEGIN
      IF ((nods[i] MOD 4) EQ 0) OR ((nods[i] MOD 4) EQ 1) THEN BEGIN
          foo=execute('stack_pos1 = stack_pos1 + stack'+n2s(nods[i])) 
          foo=execute('tint_pos1 = tint_pos1 + tint'+n2s(nods[i])) 
          foo=execute('Nframes_pos1 = Nframes_pos1 + Nframes'+n2s(nods[i])) 
      ENDIF ELSE BEGIN
          foo=execute('stack_pos2 = stack_pos2 + stack'+n2s(nods[i])) 
          foo=execute('tint_pos2 = tint_pos2 + tint'+n2s(nods[i])) 
          foo=execute('Nframes_pos2 = Nframes_pos2 + Nframes'+n2s(nods[i])) 
      ENDELSE 
  ENDFOR
  
  ;; Normalize on the number of frames
  aver_stack_pos1 = stack_pos1/Nframes_pos1
  aver_stack_pos2 = stack_pos2/Nframes_pos2

  ;; Now get the positive and negative beam positions
  IF n_elements(beam) EQ 0 THEN BEGIN
      IF keyword_set(auto) THEN BEGIN
          smooth =rebin(rebin(aver_stack_pos1,80,60),320,240)
          foo= max(smooth, peak1)
          foo= min(smooth, peak2)
          x1 = peak1 MOD 320
          y1 = peak1  /  320
          x2 = peak2 MOD 320
          y2 = peak2  /  320
          smooth =rebin(rebin(aver_stack_pos2,80,60),320,240)
          foo= max(smooth, peak3)
          foo= min(smooth, peak4)
          x3 = peak3 MOD 320
          y3 = peak3  /  320
          x4 = peak4 MOD 320
          y4 = peak4  /  320
      ENDIF ELSE BEGIN
          tvscl,aver_stack_pos1
          print,'indicate the POSITIVE beam with the left mouse button'
          Cursor,x1,y1,2,/down,/device
          print,'indicate the NEGATIVE beam with the left mouse button'
          Cursor,x2,y2,2,/down,/device
          
          tvscl,aver_stack_pos2
          print,'indicate the POSITIVE beam with the left mouse button'
          Cursor,x3,y3,2,/down,/device
          print,'indicate the NEGATIVE beam with the left mouse button'
          Cursor,x4,y4,2,/down,/device
      ENDELSE 
      beam=[[x1,y1],[x2,y2],[x3,y3],[x4,y4]]
  ENDIF 
  
  ;; Now find the optimal shifts:
  FOR i=0,n_elements(nods)-1 DO BEGIN
      foo=execute('cube=cube'+n2s(nods[i]))
      foo=execute('Nframes=Nframes'+n2s(nods[i]))
      IF ((nods[i] MOD 4) EQ 0) OR ((nods[i] MOD 4) EQ 1) THEN BEGIN
          aver_other_nod = aver_stack_pos2
          posx = beam[0,0]
          posy = beam[1,0]
          negx = beam[0,1]
          negy = beam[1,1]
      ENDIF ELSE BEGIN
          aver_other_nod = aver_stack_pos1
          posx = beam[0,2]
          posy = beam[1,2]
          negx = beam[0,3]
          negy = beam[1,3]
      ENDELSE 
      FOR j=0,n_elements(cube[0,0,*])-1 DO BEGIN
          img = reform(cube[*,*,j],320,240)
          img = img - aver_other_nod

;; The positive beam
          shift = timmi2_fitshift(ref,img,x=posx,y=posy,box=box)
          shifted_img = timmi2_shift(img,shift[0],shift[1])

          IF NOT noplot THEN BEGIN
              tvscale,shifted_img
          ENDIF 

          IF use_ref EQ 0 THEN BEGIN
              IF idx_shift EQ 0 THEN ref=ref-ref
              ref = ref + shifted_img
          ENDIF 
;; Store the result
          shifts[idx_shift,*]=shift
          idx_shift = idx_shift+1

;; The negative beam
          shift = timmi2_fitshift(ref,-1d0*img,x=negx,y=negy,box=box)
          shifted_img = timmi2_shift(-1d0*img,shift[0],shift[1])
          IF use_ref EQ 0 THEN BEGIN
              ref = ref + shifted_img
          ENDIF 

          IF NOT noplot THEN BEGIN
              tvscale,shifted_img
          ENDIF 

;; Store the result
          shifts[idx_shift,*]=shift
          idx_shift = idx_shift+1

      ENDFOR ;;j
  ENDFOR ;;i

  ;; Write the results in the journaling file
  ;;For a journaling file
  get_lun,jourlun
  openw,jourlun,journal

  printf,jourlun,';;; '+journal
  printf,jourlun,';;TIMMI2_SHIFTADD: Journal file: '+datum
  printf,jourlun,';;; --------------------------- ;;;'
  printf,jourlun,';;; --Initialise arrays-------- ;;;'
  printf,jourlun,';;; --------------------------- ;;;'
  printf,jourlun,'final=0d0*dblarr(320,240)'
  printf,jourlun,'stack_pos1=final'
  printf,jourlun,'stack_pos2=final'
  FOR i=0,n_elements(nods)-1 DO BEGIN
      printf,jourlun,'shifted_stack'+n2s(nods[i])+'pos=final'
      printf,jourlun,'shifted_stack'+n2s(nods[i])+'neg=final'
  ENDFOR
  printf,jourlun,';;; --------------------------- ;;;'
  printf,jourlun,';;; --Read in data------------- ;;;'
  printf,jourlun,';;; --------------------------- ;;;'
  ;; First read in the data cubes and stack the images per nod position
  FOR i=0,n_elements(nods)-1 DO BEGIN
      printf,jourlun,"readplanes,'"+dir+"',"+n2s(nods[i])+ $
             ',stack'+n2s(nods[i])+',cube'+n2s(nods[i])+',tint'+n2s(nods[i])
  ENDFOR 

  FOR i=0,n_elements(nods)-1 DO BEGIN
      IF ((nods[i] MOD 4) EQ 0) OR ((nods[i] MOD 4) EQ 1) THEN BEGIN
          printf,jourlun,'stack_pos1 = stack_pos1 + stack'+n2s(nods[i])
      ENDIF ELSE BEGIN
          printf,jourlun,'stack_pos2 = stack_pos2 + stack'+n2s(nods[i])
      ENDELSE 
  ENDFOR
  
  printf,jourlun,'aver_stack_pos1 = stack_pos1/'+n2s(Nframes_pos1)+'d0'
  printf,jourlun,'aver_stack_pos2 = stack_pos2/'+n2s(Nframes_pos2)+'d0'
  
  idx_shift=0d0
  FOR i=0,n_elements(nods)-1 DO BEGIN
      IF ((nods[i] MOD 4) EQ 0) OR ((nods[i] MOD 4) EQ 1) THEN BEGIN
          backg='2'
      ENDIF ELSE BEGIN
          backg='1'
      ENDELSE

      foo=execute('cube=cube'+n2s(nods[i]))
      FOR j=0,n_elements(cube[0,0,*])-1 DO BEGIN
          printf,jourlun,'shifted_stack'+n2s(nods[i])+'pos=shifted_stack'+ $
                 n2s(nods[i])+'pos+timmi2_shift(cube'+n2s(nods[i])+ $
                 '[*,*,'+n2s(j)+']-aver_stack_pos'+backg+','+ $
                 f2s(shifts[idx_shift,0])+','+f2s(shifts[idx_shift,1])+')'
          idx_shift = idx_shift+1
          printf,jourlun,'shifted_stack'+n2s(nods[i])+'neg=shifted_stack'+ $
                 n2s(nods[i])+'neg+timmi2_shift(cube'+n2s(nods[i])+ $
                 '[*,*,'+n2s(j)+']-aver_stack_pos'+backg+','+ $
                 f2s(shifts[idx_shift,0])+','+f2s(shifts[idx_shift,1])+')'
          idx_shift = idx_shift+1
      ENDFOR 
  ENDFOR

  FOR i=0,n_elements(nods)-1 DO BEGIN
      printf,jourlun,'final=final+shifted_stack'+n2s(nods[i])+'pos'
      printf,jourlun,'final=final-shifted_stack'+n2s(nods[i])+'neg'
  ENDFOR

;;      printf,jourlun,'final=final>0d0'
      printf,jourlun,"writefits,'"+filename+".fits',final"
      
      close,jourlun
      free_lun,jourlun

;;;  return,out
return,ref
  
END

      
;  
;
;  IF n_elements(old_shifts) EQ 0 THEN BEGIN
;      openr,lun,file,/get_lun
;      foo = mrdfits(lun,0,header)
;      asciitable = mrdfits(lun)
;      
;      WHILE (NOT EOF(lun)) AND (i LT NImage) DO BEGIN
;          imgs = double(mrdfits(lun))
;          ;;Now we DO the thing FOR each image
;          FOR j=0,1 DO BEGIN
;              foo = execute('img='+img_text[j])
;              
;              shift = timmi_fitshift(ref,img)
;              ;; Now we found the shift required
;              message,'image: '+n2s(i*2+j),/inform
;              shifts[*,2*i+j]=shift
;              ;; shift the image to the right place
;              img=timmi_shift(img,shift[0],shift[1])
;              IF noplot EQ 0 THEN BEGIN
;                  ;; Show it on the screen
;                  tvscl,img
;                  plots,[160-10,160+10,160,160,160], $
;                        [120,120,120,120+10,120-10],/device
;                  empty
;              ENDIF 
;              IF use_ref EQ 0 THEN BEGIN
;                  IF (i+j) EQ 0 THEN ref[160,120]=0d0
;                  ref = ref + img
;              ENDIF 
;          ENDFOR
;          i=i+1
;      ENDWHILE
;      close,lun
;      free_lun,lun
;
;      nshifts = 2*i
;      shifts = shifts[*,0:nshifts-1]
;      save,shifts,file=filename+'_shifts.sav'
;  ENDIF ELSE BEGIN
;      shifts = old_shifts
;      nshifts = n_elements(shifts[0,*])
;  ENDELSE 
;
;  ;; now we split these into nod positions for this we add the shifts
;  ;; of the chop this cancels the chop direction
;
;  i=indgen(nshifts/2)*2
;  nodpos_x = (shifts[0,i]+shifts[0,i+1])/2d0
;  nodpos_y = (shifts[1,i]+shifts[1,i+1])/2d0
;  
;  ;; we want to split along x or y
;  IF (moment(nodpos_x))[1] GT (moment(nodpos_y))[1] THEN $
;    nodpos = nodpos_x ELSE $
;    nodpos = nodpos_y
;
;  ;; now find the different nods
;  nod1 = where(nodpos LT mean(nodpos))
;  nod2 = where(nodpos GE mean(nodpos))
;  
;  n1c1 = nod1*2
;  n1c2 = nod1*2+1
;  n2c1 = nod2*2
;  n2c2 = nod2*2+1
;
;  ;; Calculate the mean 
;  m_n1c1_x = mean(shifts[0,n1c1])
;  m_n1c2_x = mean(shifts[0,n1c2])
;  m_n2c1_x = mean(shifts[0,n2c1])
;  m_n2c2_x = mean(shifts[0,n2c2])
;
;  m_n1c1_y = mean(shifts[1,n1c1])
;  m_n1c2_y = mean(shifts[1,n1c2])
;  m_n2c1_y = mean(shifts[1,n2c1])
;  m_n2c2_y = mean(shifts[1,n2c2])
;
;  res_shifts = shifts
;  res_shifts[0,n1c1] = shifts[0,n1c1] - m_n1c1_x
;  res_shifts[0,n1c2] = shifts[0,n1c2] - m_n1c2_x
;  res_shifts[0,n2c1] = shifts[0,n2c1] - m_n2c1_x
;  res_shifts[0,n2c2] = shifts[0,n2c2] - m_n2c2_x
;
;  res_shifts[1,n1c1] = shifts[1,n1c1] - m_n1c1_y
;  res_shifts[1,n1c2] = shifts[1,n1c2] - m_n1c2_y
;  res_shifts[1,n2c1] = shifts[1,n2c1] - m_n2c1_y
;  res_shifts[1,n2c2] = shifts[1,n2c2] - m_n2c2_y
;  
;  ;; To check on the distance from the mean
;  dist_res_shifts = sqrt(res_shifts[0,*]^2d0+res_shifts[1,*]^2d0)
;  foo = moment(dist_res_shifts)
;  
;  ibad = where(dist_res_shifts GT 5d0*foo[1]^0.5)
;  ;; To be able to comment out the bad points
;  comtxt = make_array(nshifts,value='   ')
;  IF ibad[0] NE -1 THEN comtxt[ibad]=';;;'
;
;  postxt = make_array(nshifts,value='posx')
;  postxt[n1c1]='n1c1'
;  postxt[n1c2]='n1c2'
;  postxt[n2c1]='n2c1'
;  postxt[n2c2]='n2c2'
;
;  ;; for the background image (the nodding image)
;  baktxt = make_array(nshifts,value='bak')
;  baktxt[n1c1,n1c2]='bak1=bak1+img'
;  baktxt[n2c1,n2c2]='bak2=bak2+img'
;
