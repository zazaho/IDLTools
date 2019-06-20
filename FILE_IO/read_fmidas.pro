;S Hony
; Read aar fluxes from a midas spectrum fits file

FUNCTION read_fmidas,fname,help=help,table=table,_extra=_extra

;no parameters or help requested
  if keyword_set(help) then begin
    print,'function read_fmidas:'
    print,'usage:'
    print,'test = read_fmidas(filename[,/help,comment=comment,skip=skip]'
    print,'filename: name of midas fits spectrum to read from'
    print,'/help provides this help'
    print,'/table: output a table instead of an aar-struct'
    return,0
  endif
  
  default,table,0
    
  if n_elements(fname) eq 0 then begin
     fname = dialog_pickfile(FILTER='*.fits',/MUST_EXIST)
  endif
  
;make sure the file exists
;;  foo = findfile(fname,count=count)
  foo = file_search(fname,count=count)
  if (count eq 0) then begin
     print,'the file ',fname,' does not exist'
     return,0
  endif
  
  flux = mrdfits(fname,0,header,_extra=_extra)
  size = n_elements(flux)

  ;; reference pixel
  CRPIX1= FXPAR( Header, 'CRPIX1')
  ;; reference wavelength in mum
  CRVAL1= FXPAR( Header, 'CRVAL1')*1d-4
  ;; step per pixel in mum
  CDELT1= FXPAR( Header, 'CDELT1')*1d-4
  
  wave = crval1+cdelt1*(dindgen(size)-crpix1)
  
  IF table THEN BEGIN
     out = transpose([[wave],[flux]])
  ENDIF ELSE BEGIN
     ;;store in an aar
     out = sh_define_aar(length=size)
     out.data.wave = wave
     out.data.flux = flux
  ENDELSE
  
  return,out
END
