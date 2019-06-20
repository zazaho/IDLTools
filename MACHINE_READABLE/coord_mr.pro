;+
; NAME:
;  coord_mr.pro
;
; PURPOSE:
;  Given a machine readable tables it will return ra and dec in J2000
;  calculated from the positional information in the table.
;
; CATEGORY:
;  MACHINEREADBLE DATA MANIPULATION
;
; CALLING SEQUENCE:
;  coords = coord_mr(mstruct,cols,/help,precess=precess,suffix=suffix)
;
; INPUTS:
;  mstruct: Machine readable tables that contain positional info.
;
; KEYWORD PARAMETERS:
;  /help: if set show the help
;  cols: array of columns from the table that should be used
;    as positions eq ['ra','dec'] or [[1,2,3,-1],[12,13,14,21]] where the
;    latter indicates that [1,2,3,-1] form the RA. Some special forms are
;    allowed: 
;    cols = ['RA','DEC'] (default)
;    'radec' = ['RA','DEC']
;    'deg' = ['RAdeg','DEdeg']
;    'sex' = [['RAh','RAm','RAs',''],['DE-','DEd','DEm','DEs']]
;    Note that [['RAh','RAm','RAs'],['DEdeg']] is not allowed by IDL.
;    Use [['RAh','RAm','RAs'],['DEdeg','','']] or
;    Use [[1,2,3],[25,-1,-1]] instead
;  suffix=suffix: string to append to cols. This is useful for merged
;     tables. E.g. coord =coord_mr(merged,'deg',suffix=':2')
;  precess=precess:
;   if set and equal to 1: the coordinates are transformed from 1950
;    to 2000 before matching.
;   if set and a single value: the coordinates are transformed from value
;    to 2000 before matching.
;   if set and a pair of values: the coordinates are transformed from value[0]
;    to value[1] before matching.
;
; EXAMPLE:
;  coords = coord_mr(tab1)
;      or
;  coord = coord_mr(struct,'sex',suffix=':2',/precess1950)
;   
; MODIFICATION HISTORY:
;  Version 1:
;  (SH Jan 26 2006)
;  (SH Mar  7 2013) Generalised precession option to be able to treat
;  precess from A to B. By default A = 1950 and B =2000
;-

FUNCTION coord_mr,mr,cols_in,suffix=suffix,help=help, $
                  precess=precess

  IF keyword_set(help) THEN BEGIN
      doc_library,'coord_mr'
      return,-1
  ENDIF

  IF NOT is_mr(mr) THEN BEGIN
      message,'This routine requires an machine readable data structure as input.'
  ENDIF

  default,cols_in,['RA','DEC']
  
  ;; protect the input from being changed
  cols = reform(strupcase([cols_in]),/overwrite)
  
  ctype = size(cols,/tname)
  cdims = size(cols,/dimensions)
  IF ctype EQ 'STRING' THEN BEGIN
      IF cdims EQ 1 THEN BEGIN
          CASE cols OF 
              'RADEC': BEGIN
                  cols = ['RA','DEC']
                  cdims = 2
              END
              'DEG': BEGIN
                  cols = ['RAdeg','DEdeg']
                  cdims = 2
              END
              'SEX': BEGIN
                  cols = [['RAh','RAm','RAs',''],['DE-','DEd','DEm','DEs']]
                  cdims = 8
              END
              'ELSE': BEGIN
              END
          ENDCASE
      ENDIF
      IF n_elements(suffix) NE 0 THEN BEGIN
          idx = where(cols NE '',cnt)
          IF cnt NE 0 THEN BEGIN
              cols[idx] = cols[idx]+strtrim(string(suffix[0]),2)
          ENDIF ELSE BEGIN
              message,'empty column names'
          ENDELSE
      ENDIF
  ENDIF
  
  CASE cdims OF
;; This would require some error checking on the output of col_mr
      2: BEGIN
          ra = col_mr(mr,cols[0])
          de = col_mr(mr,cols[1])
      END
      8: BEGIN
          IF (ctype EQ 'STRING') THEN BEGIN
              IF (cols[1] NE '') THEN BEGIN
                  rah = col_mr(mr,cols[0])
                  ram = col_mr(mr,cols[1])
                  ras = col_mr(mr,cols[2])
                  ra = 15d0*(rah+ram/60d0+ras/3600d0)
              ENDIF ELSE BEGIN
                  ra = col_mr(mr,cols[0])
              ENDELSE
              
              IF (cols[5] NE '') THEN BEGIN
                  design = col_mr(mr,cols[4])
                  ded = col_mr(mr,cols[5])
                  dem = col_mr(mr,cols[6])
                  des = col_mr(mr,cols[7])
                  de = ([1d0,-1d0])[design eq '-'] * (double(ded)+dem/60d0+des/3600d0)
              ENDIF ELSE BEGIN
                  de = col_mr(mr,cols[3])
              ENDELSE
          ENDIF ELSE BEGIN
              IF (cols[1] NE -1) THEN BEGIN
                  rah = col_mr(mr,cols[0])
                  ram = col_mr(mr,cols[1])
                  ras = col_mr(mr,cols[2])
                  ra = 15d0*(rah+ram/60d0+ras/3600d0)
              ENDIF ELSE BEGIN
                  ra = col_mr(mr,cols[0])
              ENDELSE
              
              IF (cols[5] NE -1) THEN BEGIN
                  design = col_mr(mr,cols[4])
                  ded = col_mr(mr,cols[5])
                  dem = col_mr(mr,cols[6])
                  des = col_mr(mr,cols[7])
                  de = ([1d0,-1d0])[design eq '-'] * (double(ded)+dem/60d0+des/3600d0)
              ENDIF ELSE BEGIN
                  de = col_mr(mr,cols[3])
              ENDELSE
          ENDELSE 
      END
      ELSE: BEGIN
          message,'the format of the column is not correct'
      END
  ENDCASE
  
  if size(ra,/tname) eq 'STRING' and size(de,/tname) eq 'STRING' then begin
     nra = n_elements(ra)
     tmp_ra = make_array(nra,value=0d0)
     tmp_de = make_array(nra,value=0d0)
     for ii=0,nra-1 do begin
        foo = sex2deg(ra[ii]+';'+de[ii])
        tmp_ra[ii] = foo[0]
        tmp_de[ii] = foo[1]
     endfor
     ra= tmp_ra
     de= tmp_de
  endif
  
  IF keyword_set(precess) THEN BEGIN
     case n_elements(precess) of
        1: begin
           if precess eq 1 then begin
              equinox1=1950.0
           endif else begin
              equinox1=precess
           endelse
           equinox2=2000.0
        end
        2: begin
           equinox1=precess[0]
           equinox2=precess[1]
        end
        else: begin
           message,/info,'precess takes 1 or 2 values, more were given'
           message,/info,'assuming default 1950 to 2000'
           equinox1=1950.0
           equinox2=2000.0
        end
     endcase
     precess,ra,de,equinox1,equinox2
  ENDIF
  
  return,transpose([[ra],[de]])
END
