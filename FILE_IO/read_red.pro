;+
; NAME: read_red
;
; PURPOSE: read a reduced fits file into memory given a tdt or source name
;
; CATEGORY: FILE_IO
;
; CALLING SEQUENCE: aar = read_red(query)
;
; INPUTS: query: tdt number or name string
;
; OPTIONAL INPUTS: second parameter: mr_structure of SWS observations.
; If this does not exist read in the default: $SWS_DATABASE
;
; KEYWORD PARAMETERS:
;  database: mr_structure of SWS observations
;  ssp: if set read the ssp*.fits file
;  photometry: if set then put the IRAS/2MASS and optical photometric
;   points in line 0
;  help: if set display the help
;  combined: if set attempt to read the combined spectrum as found in
;   ~/IA_FILES/MACHINE_READABLE/sws_combined.dat
;  lrs: if set attempt to read the IRAS/LRS spectrum
;  /nosimbdad: if set do not query the simbad archive if needed
;  
; OUTPUTS:
;  aar structure
;
; PROCEDURE:
;
; EXAMPLE:
;  spec = read_red(81101203)
;  plotaar,spec
;-
  
FUNCTION read_red,query_in,dbase, $
                  database=database, $
                  ssp=ssp, $
                  help=help, $
                  photometry=photometry, $
                  combined=combined, $
                  lrs=lrs, $
                  nosimbad=nosimbad
  
  query=query_in

  ;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'read_red'
      return,-1
  ENDIF

  IF n_elements(dbase) EQ 0 THEN BEGIN
      IF keyword_set(database) THEN BEGIN
          dbase=database
      ENDIF ELSE BEGIN
          dbase=read_fmr(shell_expand('$SWS_DATABASE'))
      ENDELSE
  ENDIF

  IF NOT keyword_set(query) THEN BEGIN
      message,'please supply query (tdt-number or source-name) of file to retrieve',/info
      doc_library,'read_red'
      return,-1
  ENDIF ELSE BEGIN
      query=query[0]
      IF size(query,/tname) NE 'STRING' THEN BEGIN
          query = strmid('00000000'+n2s(Long(query)),7,8,/reverse_off)
      ENDIF
  ENDELSE

  IF NOT is_mr(dbase) THEN BEGIN
      message,'The database is not a mr_structure',/info
      doc_library,'read_red'
      return,-1
  ENDIF ELSE BEGIN
      tdts = col_mr(dbase,'tdt')
      IF size(tdts,/tname) NE 'STRING' THEN BEGIN
          message,'The database is does not contain tdt numbers',/info
          doc_library,'read_red'
          return,-1
      ENDIF
      
      files = col_mr(dbase,'file')
      IF size(files,/tname) NE 'STRING' THEN BEGIN
          message,'The database is does not contain file names',/info
          doc_library,'read_red'
          return,-1
      ENDIF
  ENDELSE
  
  ;; now try to find the matching record
  ;; Do so in the following order:
  ;; TDT number or string
  ;; Source name matching ISO archive NAME
  ;; Source name matching IRAS number as string
  ;; by position using SIMBAD query


  found_source = 0

  ;; TDT matching
  idx = (where(strcompress(strupcase(tdts)) EQ $
               strcompress(strupcase(query))))[0]
  IF idx NE -1 THEN BEGIN
      tdt = tdts[idx]
      found_source = 1
  ENDIF
  
  ;; Not a TDT -> query SIMBAD for position
  IF (found_source NE 1) AND (NOT keyword_set(nosimbad)) THEN BEGIN
      
      ras = col_mr(dbase,'radeg')
      IF size(ras,/tname) NE 'DOUBLE' THEN BEGIN
          message,'The database is does not contain RA in degree',/info
          doc_library,'read_red'
          return,-1
      ENDIF
      
      decs = col_mr(dbase,'dedeg')
      IF size(decs,/tname) NE 'DOUBLE' THEN BEGIN
          message,'The database is does not contain DEC in degree',/info
          doc_library,'read_red'
          return,-1
      ENDIF
      
      message,'Try to resolve the source using SIMBAD',/info
      querysimbad,query,ra,dec,errmsg=errmsg,found=found
      IF found NE 1 THEN BEGIN
          message,'The simbad query was unsuccesful and returned the following message:', $
                  /informational
          FOR i=0,n_elements(errmsg)-1 DO message,errmsg[i],/informational
      ENDIF ELSE BEGIN
;; determine the distances in arcsec
          dist = 3600d0*sqrt( $
                 ((ras-ra)*cos(!dpi/180d0*(decs+dec)/2d0))^2d0+ $
                 (decs-dec)^2d0 $
                            )
;; Find the closest match 
          mindist = min(dist,idx_mindist)
          
;; Try and be sensible here. So let's limit ourselves to 60 arcsec
          IF mindist GT 60d0 THEN BEGIN
              message,'There is no match at the SIMBAD position within 60 arcsec', $
                      /informational
          ENDIF ELSE BEGIN
;; Now consider everything within 3 times mindist an equally good
;; matching source
              idx = where(dist LE 3d0*mindist,cnt)
              CASE cnt OF
                  0: BEGIN
                      found_source = 0
                  END
                  1: BEGIN
                      idx=idx[0]
                      tdt = tdts[idx]
                      message,'The given string matches tdt: '+tdt,/info
                      message,'The distance between ISO and SIMBAD is: '+string(dist[idx])+' arcsec',/info
                      found_source = 1
                  END
                  ELSE: BEGIN
                      message,'The following TDT numbers match this position',/info
                      FOR i=0,cnt-1 DO BEGIN
                          print,n2s(i)+': '+tdts[idx[i]]+' - '+files[idx[i]]+' ('+string(dist[idx[i]])+'")'
                      ENDFOR
                      print,'Which one shall I read::'
                      ans=0
                      read,ans
                      IF ans LT cnt THEN idx = idx[ans] ELSE idx = idx[0]
                      tdt = tdts[idx]
                      found_source = 1
                  END
              ENDCASE
          ENDELSE
      ENDELSE
  ENDIF



  ;; Not a SIMBAD -> test for Name
  IF found_source NE 1 THEN BEGIN
      ;; perhaps we are looking for a source name: 
      names = col_mr(dbase,'name')
      IF size(names,/tname) NE 'STRING' THEN BEGIN
          message,'The database is does not contain source names',/info
          doc_library,'read_red'
          return,-1
      ENDIF
      
      idx = where(strcompress(strupcase(names)) EQ $
                  strcompress(strupcase(query)),cnt)
      CASE cnt OF
          0: BEGIN
              found_source = 0
          END
          1: BEGIN
              idx=idx[0]
              tdt = tdts[idx]
              message,'The given string is interpreted as a source name for tdt: '+tdt,/info
              found_source = 1
          END
          ELSE: BEGIN
              message,'The following TDT numbers match the source name',/info
              FOR i=0,cnt-1 DO BEGIN
                  print,n2s(i)+': '+tdts[idx[i]]+' - '+files[idx[i]]
              ENDFOR
              print,'Which one shall I read::'
              ans=0
              read,ans
              IF ans LT cnt THEN idx = idx[ans] ELSE idx = idx[0]
              tdt = tdts[idx]
              found_source = 1
          END
      ENDCASE
  ENDIF
       
  ;; Not a source name -> test for IRAS Name
  IF found_source NE 1 THEN BEGIN
      ;; remove IRAS prefix and white space before the search
      irasname = strcompress(strsplit(query,'iras',/extract,/fold_case,/regex),/remove_all)

      irasnames = col_mr(dbase,'IRAS')
      IF size(irasnames,/tname) NE 'STRING' THEN BEGIN
          message,'The database is does not contain IRAS names',/info
          doc_library,'read_red'
          return,-1
      ENDIF
      
      idx = where(strcompress(strupcase(irasnames)) EQ $
                  irasname,cnt)
      CASE cnt OF
          0: BEGIN
              found_source = 0
          END
          1: BEGIN
              idx=idx[0]
              tdt = tdts[idx]
              message,'The given string is interpreted as the IRAS name of tdt: '+tdt,/info
              found_source = 1
          END
          ELSE: BEGIN
              message,'The following TDT numbers match this IRAS name',/info
              FOR i=0,cnt-1 DO BEGIN
                  print,n2s(i)+': '+tdts[idx[i]]+' - '+files[idx[i]]
              ENDFOR
              print,'Which one shall I read::'
              ans=0
              read,ans
              IF ans LT cnt THEN idx = idx[ans] ELSE idx = idx[0]
              tdt = tdts[idx]
              found_source = 1
          END
      ENDCASE
  END
  
;;; SO we tried many things and none worked perhaps the input is
;;; bogus?
  IF found_source NE 1 THEN BEGIN
      message,'I have tried to match your input to:',/info
      message,'TDT, NAME, IRAS and SIMBAD but none worked',/info
      return,-1
  ENDIF
      
  ;; No we do the reading this is in the order of:
  ;; combined if asked for
  ;; my pipeline reduced file according to the sws_database
  ;; the ssp*.fits file from the standard pipeline

  ;; nothing read yet
  read_file = 0
  
  ;;If asked for a combined reduced file try that first
  IF keyword_set(combined) THEN BEGIN
      temp = { $
             VERSION:1.00000, $
             DATASTART:0L, $
             DELIMITER:32B, $
             MISSINGVALUE:!VALUES.F_NaN, $
             COMMENTSYMBOL:'', $
             FIELDCOUNT:2L, $
             FIELDTYPES:[3L,7L], $
             FIELDNAMES:['tdts','files'], $
             FIELDLOCATIONS:[0L,9L], $
             FIELDGROUPS:[0L,1L] $
             }
      
      comb = read_ascii(shell_expand('~/IA_FILES/MACHINE_READABLE/sws_combined.dat'), $
                        temp=temp)
      imatch = (where(tdt EQ comb.tdts,cnt))[0]
      IF cnt NE 0 THEN BEGIN
          out = sh_read_faar(expand_string(comb.files[imatch]))
      ENDIF
      IF NOT sh_is_aar(out) THEN BEGIN
          message,'Could not find a combined file for tdt:'+string(tdt),/info
      ENDIF ELSE BEGIN
          message,'read: '+comb.files[imatch],/info
          read_file = 1
      ENDELSE
  ENDIF

  ;; Now try the reduced file
  IF NOT read_file THEN BEGIN
      IF keyword_set(ssp) THEN BEGIN
          out=read_fssp(tdt)
          read_file = 1
      ENDIF ELSE BEGIN
          file = expand_string(strcompress(files[idx]))
          IF file NE '' THEN BEGIN
              out = sh_read_faar(expand_string(strcompress(files[idx])))
              IF sh_is_aar(out) THEN BEGIN
                  message,'read: '+files[idx],/info
                  read_file = 1
              ENDIF ELSE BEGIN
                  message,'could not read: '+files[idx]
              ENDELSE
          ENDIF 
      ENDELSE
  ENDIF
  
  ;; last resort read the ssp file
  IF NOT read_file THEN BEGIN
      out=read_fssp(tdt)
      IF NOT sh_is_aar(out) THEN BEGIN
          message,'could not read: ssp'+i2s(tdt)+'.fits'
      ENDIF
  ENDIF

  IF keyword_set(photometry) THEN BEGIN
      rec = row_mr(dbase,idx)
      B   = col_mr(rec,'Bmag')
      V   = col_mr(rec,'Vmag')
      R   = col_mr(rec,'Rmag')
      I   = col_mr(rec,'Imag')
      J   = col_mr(rec,'Jmag')
      H   = col_mr(rec,'Hmag')
      K   = col_mr(rec,'Kmag')
      EI  = col_mr(rec,'e_Imag')
      EJ  = col_mr(rec,'e_Jmag')
      EH  = col_mr(rec,'e_Hmag')
      EK  = col_mr(rec,'e_Kmag')
      EV  = col_mr(rec,'e_Vmag')
      F12 = col_mr(rec,'F12')
      F25 = col_mr(rec,'F25')
      F60 = col_mr(rec,'F60')
      F100= col_mr(rec,'F100')
      E12 = col_mr(rec,'e_F12')
      E25 = col_mr(rec,'e_F25')
      E60 = col_mr(rec,'e_F60')
      E100= col_mr(rec,'e_F100')
      A   = col_mr(rec,'A')
      B1  = col_mr(rec,'B1')
      B2  = col_mr(rec,'B2')
      C   = col_mr(rec,'C')
      D   = col_mr(rec,'D')
      E   = col_mr(rec,'E')
      eA   = col_mr(rec,'e_A')
      eB1  = col_mr(rec,'e_B1')
      eB2  = col_mr(rec,'e_B2')
      eC   = col_mr(rec,'e_C')
      eD   = col_mr(rec,'e_D')
      eE   = col_mr(rec,'e_E')

      VT   = col_mr(rec,'VTmag')
      BT   = col_mr(rec,'BTmag')
      eVT  = col_mr(rec,'e_VTmag')
      eBT  = col_mr(rec,'e_BTmag')

      wave = [0d0]
      flux = [0d0]
      eflux =[0d0]
      line = [0d0]
      ;           B   V    I   J      H      K
      WV     = [0.44,0.55,0.90,1.235,1.662,2.159]
      ZP_lam = [7.19d-8,3.92d-8,8.30d-9,3.129d-9,1.133d-9,4.283d-10]
      ZP_jy = ZP_lam * WV^2d0 / 3d14 * 1d26
      
      IF finite(B) AND (B NE 0.0) THEN BEGIN
          wave = [ wave,wv[0]]
          flux = [ flux,ZP_jy[0]*1d1^(-1d0*B/2.5d0)]
          eflux= [eflux,0d0]
          line = [line ,60]
      ENDIF

      IF finite(VT) THEN BEGIN
                                ;Either BTmag or VTmag is always given.
                                ;Approximate Johnson photometry
                                ;may be obtained as:
                                ;   V = VT -0.090*(BT-VT) 
                                ;   B-V = 0.850*(BT-VT) 
          VTEMP = 1.090*VT-0.090*BT
          BTEMP = 0.760*BT+0.24*VT
          eVTEMP = 1.090*eVT + 0.090*eBT
          eBTEMP = 0.760*eBT + 0.24*eVT
          
          wave = [ wave,wv[0],wv[1]]
          flux = [ flux,ZP_jy[0]*1d1^(-1d0*BTEMP/2.5d0),ZP_jy[1]*1d1^(-1d0*VTEMP/2.5d0)]
          eflux= [eflux,ZP_jy[0]*1d1^(-1d0*BTEMP/2.5d0)*(1d1^(EBTEMP/2.5d0)-1d1^(-EBTEMP/2.5d0))/2d0,ZP_jy[1]*1d1^(-1d0*VTEMP/2.5d0)*(1d1^(EVTEMP/2.5d0)-1d1^(-EVTEMP/2.5d0))/2d0]
          line = [line ,60,60]
      ENDIF

      IF finite(V) THEN BEGIN
          IF NOT finite(EV) THEN EV=2.5d0
          wave = [ wave,wv[1]]
          flux = [ flux,ZP_jy[1]*1d1^(-1d0*V/2.5d0)]
          eflux= [eflux,ZP_jy[1]*1d1^(-1d0*V/2.5d0)*(1d1^(EV/2.5d0)-1d1^(-EV/2.5d0))/2d0]
          line = [line ,60]
      ENDIF

;;    although it is called R (this USNO-A1.0 value is actualy close
;;    to Johnson V
      IF finite(R) AND (R NE 0.0) THEN BEGIN
          wave = [ wave,wv[1]]
          flux = [ flux,ZP_jy[1]*1d1^(-1d0*R/2.5d0)]
          eflux= [eflux,0d0]
          line = [line ,60]
      ENDIF

      IF finite(I) THEN BEGIN
          IF NOT finite(EI) THEN EI=2.5d0
          wave = [ wave,wv[2]]
          flux = [ flux,ZP_jy[2]*1d1^(-1d0*I/2.5d0)]
          eflux= [eflux,ZP_jy[2]*1d1^(-1d0*I/2.5d0)*(1d1^(EI/2.5d0)-1d1^(-EI/2.5d0))/2d0]
          line = [line ,60]
      ENDIF

      IF finite(J) THEN BEGIN
          IF NOT finite(EJ) THEN EJ=2.5d0
          wave = [ wave,wv[3]]
          flux = [ flux,ZP_jy[3]*1d1^(-1d0*J/2.5d0)]
          eflux= [eflux,ZP_jy[3]*1d1^(-1d0*J/2.5d0)*(1d1^(EJ/2.5d0)-1d1^(-EJ/2.5d0))/2d0]
          line = [line ,61]
      ENDIF

      IF finite(H) THEN BEGIN
          IF NOT finite(EH) then EH=2.5d0
          wave = [ wave,wv[4]]
          flux = [ flux,ZP_jy[4]*1d1^(-1d0*H/2.5d0)]
          eflux= [eflux,ZP_jy[4]*1d1^(-1d0*H/2.5d0)*(1d1^(EH/2.5d0)-1d1^(-EH/2.5d0))/2d]
          line = [line ,61]
      ENDIF

      IF finite(K) THEN BEGIN
          IF NOT finite(EK) then EK=2.5d0
          wave = [ wave,wv[5]]
          flux = [ flux,ZP_jy[5]*1d1^(-1d0*K/2.5d0)]
          eflux= [eflux,ZP_jy[5]*1d1^(-1d0*K/2.5d0)*(1d1^(EK/2.5d0)-1d1^(-EK/2.5d0))/2d]
          line = [line ,61]
      ENDIF

      IF finite(F12) THEN BEGIN
          wave = [ wave,12d0]
          flux = [ flux,F12 ]
          eflux= [eflux,E12*F12*1d-2]
          line = [line ,62]
      ENDIF

      IF finite(F25) THEN BEGIN
          wave = [ wave,25d0]
          flux = [ flux,F25 ]
          eflux= [eflux,E25*F25*1d-2]
          line = [line ,62]
      ENDIF

      IF finite(F60) THEN BEGIN
          wave = [ wave,60d0]
          flux = [ flux,F60 ]
          eflux= [eflux,E60*F60*1d-2]
          line = [line ,62]
      ENDIF

      IF finite(F100) THEN BEGIN
          wave = [ wave,100d0]
          flux = [ flux,F100 ]
          eflux= [eflux,E100*F100*1d-2]
          line = [line ,62]
      ENDIF

      IF finite(A) THEN BEGIN
          wave = [ wave,8.28]
          flux = [ flux,A ]
          eflux= [eflux,EA*A*1d-2]
          line = [line ,63]
      ENDIF

      IF finite(B1) THEN BEGIN
          wave = [ wave,4.29]
          flux = [ flux,B1]
          eflux= [eflux,EB1*B1*1d-2]
          line = [line ,63]
      ENDIF

      IF finite(B2) THEN BEGIN
          wave = [ wave,4.25]
          flux = [ flux,B2]
          eflux= [eflux,EB2*B2*1d-2]
          line = [line ,63]
      ENDIF

      IF finite(C) THEN BEGIN
          wave = [ wave,12.13]
          flux = [ flux,C ]
          eflux= [eflux,EC*C*1d-2]
          line = [line ,63]
      ENDIF

      IF finite(D) THEN BEGIN
          wave = [ wave,14.65]
          flux = [ flux,D ]
          eflux= [eflux,ED*D*1d-2]
          line = [line ,63]
      ENDIF

      IF finite(E) THEN BEGIN
          wave = [ wave,21.34]
          flux = [ flux,E ]
          eflux= [eflux,EE*E*1d-2]
          line = [line ,63]
      ENDIF

      IF n_elements(wave) GT 1 THEN BEGIN
          wave =wave  [1:*]
          flux =flux  [1:*]
          eflux=eflux [1:*]
          line =line  [1:*]
          phot = sh_define_aar(length=n_elements(wave))
          phot.data.line = line
          phot.data.wave = wave
          phot.data.flux = flux
          phot.data.stdev = eflux
          out = sh_combine(out,phot)
      ENDIF

  ENDIF

  IF keyword_set(lrs) THEN BEGIN
      lrs = read_flrs(col_mr(row_mr(dbase,idx),'tdt'))
      out = sh_combine(out,lrs)
  ENDIF

  return,out

END
