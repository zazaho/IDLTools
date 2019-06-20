;(SH Jan 10 2000)
; Funcion to convert post-helium aar to aar readable by isap
; The header is not copied
function convert_aar,in
  
  ; The speed of light in micron
  c =  2.99792458d14
  ; Always use temp variables because otherwise we will changes the
  ; input variables
  tmp = in
  ; changes the units from W/cm^2/um to Jy 
  tmp.data.flux = 1d4*1d26*tmp.data.flux*(tmp.data.wave)^2d0/c
  
  ; Find out the length of the aar 
  ndata = n_elements(tmp.data)
  
  out = define_aar(length=ndata)
  
  ; Get the subkeys from the source and target aar
  tmp_keys = STRUPCASE(tag_names(tmp.data))
  out_keys = STRUPCASE(tag_names(out.data))
  
  ; Copy each key to the out-aar
  FOR i = 0,n_elements(tmp_keys)-1 DO BEGIN
    ; Check that the target key exists
    IF ((where(out_keys EQ tmp_keys(i)))(0) NE -1) THEN BEGIN
      f = execute('out.data.'+tmp_keys(i)+'=tmp.data.'+tmp_keys(i))
    ENDIF
  ENDFOR
  
 ;Now copy some of the header info to the new aar:
    tmp_header = tmp.header
    out_header = out.header
    
    kwd = ['ORIGIN','TELESCOP','INSTRUME','FILENAME','DATE','FILEVERS','OLPVERS','CALGVERS','USERNAME']
    kwd = [kwd,['OBJECT','OBSERVER','EOHAUTCS','EOHAUTCE','EOHAAOTN','EOHAPLID','EOHAOSN','EOHAPSN','EOHAPCAT']]
    kwd = [kwd,['EOHACIND','EOHATTYP','AOTVERS','ATTTYPE','VERS1','VERS2','VERS3','VERS4','VERS5','VERS6']]
    kwd = [kwd,['VERS7','VERS8','VERS9','VERS10','VERS11','VERS12','VERS13','VERS14','VERS15','VERS16','VERS17']]
    kwd = [kwd,['VERS18','VERS19','VERS20','VERS21','VERS22','VERS99','VERS97','VERS98','EOHAAOTV']]

    FOR i = 0,n_elements(kwd)-1 DO BEGIN
      status = read_fits_key(tmp_header,kwd(i),old_value,old_comm)
      IF (status NE 2) THEN $
        out_header = write_fits_key(out_header,kwd(i),old_value,'S',old_comm,status)
    ENDFOR
  
    kwd = ['TMRATE','ATTGUIDE','ATTERROR','ATTUTCSL','ATTUTCS','TREFUTC1','TREFUTC2','TREFUTK','TREFITK']
    kwd = [kwd,['TREFCOR1','TREFCOR2','TREFCOR3']         ]
    
    FOR i = 0,n_elements(kwd)-1 DO BEGIN
      status = read_fits_key(tmp_header,kwd(i),old_value,old_comm)
      IF (status NE 2) THEN $
        out_header = write_fits_key(out_header,kwd(i),old_value,'I',old_comm,status)
    ENDFOR  
    
    kwd = ['INSTRA','INSTDEC','INSTROLL','CINSTRA','CINSTDEC','CINSTROL','EQUINOX','ATTOTFTH','ATTRA']
    kwd=[kwd,['ATTDEC','ATTSAANG','TREFITKU','TREFHEL1','TREFDOP1','TREFHEL2','TREFDOP2','TREFHEL3','TREFDOP3']]
    
    FOR i = 0,n_elements(kwd)-1 DO BEGIN
      status = read_fits_key(tmp_header,kwd(i),old_value,old_comm)
      IF (status NE 2) THEN $
        out_header = write_fits_key(out_header,kwd(i),old_value,'R',old_comm,status)
    ENDFOR  
        
    out.header = out_header
    return,out
END
