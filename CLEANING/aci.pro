FUNCTION aci,in,_extra=_extra,noguess=noguess,line=line
IF keyword_set(noguess) THEN BEGIN
    return,aar_clean_itk(in,_extra=_extra)
ENDIF ELSE BEGIN
    if not keyword_set(line) then begin
        line= sh_uniq(in.data.line)
    endif
    out=0
    FOR i = 0, n_elements(line)-1 DO BEGIN
        tmp = sh_select(in,in.data.line eq line[i])
        if not is_int(tmp) then begin
            name = 'a'+(sh_whichband(tmp))[0]
            tmp = aar_clean_itk(tmp,name=name,_extra=_extra)
            out=sh_combine(tmp,out,/quiet)
        endif
    ENDFOR
    return,out
ENDELSE
END
