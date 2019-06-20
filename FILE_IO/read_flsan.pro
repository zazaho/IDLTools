; Function to read an lws fits lsan file into a sws/aar
; copies all data structures that are available
; also simulates the sws structure by moving some bits around 

FUNCTION read_flsan,fname
if file_test(fname) eq 1 then begin
    stat = sap_rfits(fname,tmp)
    len = n_elements(tmp.data)
    out = sh_define_aar(len=len)
    out.header  = tmp.header
    out.history = tmp.history
    keys = STRUPCASE(tag_names(tmp.data))
    aar_keys = STRUPCASE(tag_names(out.data))
    
    FOR i = 0,n_elements(keys)-1 DO BEGIN
        IF ((where(aar_keys EQ keys[i]))(0) NE -1) THEN BEGIN
            f = execute('out.data.'+keys[i]+'=tmp.data.'+keys[i])
        ENDIF
    ENDFOR

    ;; Change to SWS standard (dets->lines;scans->dets)
    out.data.line = out.data.det
    out.data.det  = out.data.scnt
    ;; SWS units
    return,sh_calcaar(out,fl=-1,fact=1e4)
endif else begin
    print,'READ_FLSAN: Fatal  (F): Could not open '+fname
    return,0
endelse
END
