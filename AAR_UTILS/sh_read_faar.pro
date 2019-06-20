;#> srf_error.dc2
; Identifier   error
;
; Purpose      Setting a error or handle the error after the CALL_EXTERNAL      
;
; Call         srf_error [, code, message]
;
; Arguments    Name   I/O  Type:       Description:
;              ------------------------------------------------------------
;              code    I    string      Aux code ('O','D','M','W','E','F')
;                                       for defining the  OK, Debug, Message, 
;                                       Waring, Serious, Fatal status
;              message I    string      Message string
;
; Returns      none
;
; Description  This routine can work on two differend ways:
;              1)  When code and message are defined:
;                  The code and message string are displayed on the
;                  screen.
;
;              2)  When code and message are not defined:
;                  The routine sets the default error levels.
;
;              The routine returns normaly to the caller i.e. go on, or 
;              returns to the highest IDL level i.e. stop execution.
;
; Comment      This routine can be easy upgraded for:
;                Debug mode:
;                  If the level is defined as fatal it doesn't return
;                  to the higest level, it stay's in the routine where the error
;                  occurs.
;                  In this envirionment data analysis is possible  (variables
;                  can be read and or changed for debugging) as well as storage
;                  of data before going on.
;                Redirect of the messages (screen/log file):
;                  (standard logging can be switched on/off in IDL)
;
; Example      EXAMPLE 1: Handle error routine of only IDL routines
;
;              t=10 & n=0
;              if (n eq 0 ) then begin
;                srf_error,'F', ' Error, dividing by zero '
;              endif else begin
;                div=t/n
;              endelse
;
;              EXAMPLE 2:  Handle error of a non IDL routine 
;                          (activated by call_external)
;
;              mean  = external(/F_VALUE, 'MYN_AVG', FINDGEN(N), n )
;              error
;
; Category     UTIL
;
; Filename     srf_error
;
; Author       N.J.M. Sijm
;
; Version      3.0
;
; History      1.0  21-06-94 -->/NS  Creation date
;              1.01 12-07-94    /NS  Change code for display 
;              1.1  04-08-94    /GB  Change call_external for new linking
;                                       strategy on VAX VMS
;              1.2  13-09-94    DRB  Change call_external for new definition
;                                       of SPS_CHECK_ERROR
;              1.3  13-09-94    DRB  Initialisation of nerror = 0
;              1.4  14-09-94    DRB  Initialisation of error to a string
;                                       that is long enough for return values
;              1.5  15-09-94    DK   IA.A.6 Improve error handling
;              1.6  22-02-95  ++rh   Adapted to Unix
;              1.7  14-03-95    HB   Unified Arguments
;              1.8  15.09.95  ++rh   Patch for correct extension on HP-UX/SunOS
;              1.9  18-01-96    FL   Adapted for Alpha-OSF
;              2.0  12-12-00    FL   default lib_ext .so
;              3.0  19-04-01    FL   remove Fortran
;#<

pro srf_error, code, message

; common block to communicate with IDL proc set_error
common error_level, ret_level, mes_level, debug_level

; default values 
if ( n_elements(ret_level) EQ 0 )    then ret_level   = 5
if ( n_elements(mes_level) EQ 0 )    then mes_level   = 2
if ( n_elements(debug_level) EQ 0 )  then debug_level = 0

if n_params(0) lt 2 then return

erlev='ODMWEFS'		; OK, Debug, Message, Warning, SErious, Fatal
ermes=[' ','Debug  (','Message(','Warning(','Serious(','Fatal  (']

if ( strtrim( code ) EQ '' ) then return
level = strpos( erlev, strupcase( strmid( code, 0, 1 ) ) )
if ( level EQ 6 ) then level = 4	; fold 'S' on 'E'
len = strlen( strmid( code, 1, 3 ) )
if ( len GT 0 ) then dbl = 0 $
else dbl = debug_level + 1           ; make then different
ksign = 1
for k = 1, len do begin
  kd = strpos( '0123456789-+', strmid( code, k, 1 ) )
  if ( kd LT 0 ) then begin
     dbl = debug_level + 1		; make them different
     GOTO, NONUMBER			; exit the loop
  endif
  if ( kd EQ 10 ) then ksign = -1
  if ( kd GE 0 AND kd LE 9 ) then dbl = 10 * dbl + kd
endfor
dbl = ksign * dbl
NONUMBER: 
 if ( ( level GE mes_level ) OR ( debug_level EQ dbl ) ) then $
          print, ermes(level), code, '): ', message

case level of
  0      : return
  1      : if ( debug_level NE 0 ) then stop $
           else if ( level LT ret_level ) then return else retall
  2      : if ( level LT ret_level ) then return else retall
  3      : if ( level LT ret_level ) then return else retall
  4      : if ( level LT ret_level ) then return else retall
  5      : if ( level LT ret_level ) then return else retall
  else   : begin
               print," Intern program error: -",level,"- Not Yet Implemented "
               return                               ; intern program bug
           end
endcase

return
end
;#> srf_define_aar.dc3
; Identifier   srf_define_aar   
;
; Purpose      Create a AAR structure
;
; Synopsis     AAR = srf_define_aar([, header=header] [, length=length]
;                               [, data=data]     [, history=history])
;
; Arguments    Name   I/O Type:     Description:
;              -------------------------------------------------
;              header  I  string    Header definition [default header available]
;              history I  strarr    History information
;              length  I  integer   Size of the structure [default=0]
;              data    I  struc     Data structure 
;
; Returns      AAR structure
;
; Description  Definition of the AAR structure
;              This routine creates an AAR structure with given header
;              and a empty data structure of length.
;              Also it can give, the given data structure a AAR header
;              to make it a AAR structure.
;              If option data is used, option length is ignored.
;
; Comment      ---
;
; Example      AAR = srf_define_aar(header=hdr, length=200)
;
; Category     util
;
; Filename     srf_define_aar.pro
;
; Author       N.J.M. Sijm
;
; Version      1.7.2
;
; History      1.0  27-06-94   NS  Creation date
;              1.1  14-06-94   NS  Delete struc name, add type field
;                   14-06-94   NS  Add data  keyword 
;                   15-06-94   NS  Change set_keyword in n_elements
;              1.2  26-07-94   DRB Changed name of one called routine
;                                  (create_fits_header->srf_makeheader)
;              1.3  22-02-95 ++rh  Removed IAS_INC from include statement
;              1.4  14-03-95   HB  Unified Arguments
;              1.5  04-10-95   NS  update to SAAR type
;              1.52 19-12-95   NS  Update NAXIS2 to real data length
;              1.6  17-10-96   EkW S0092 , correct NAXIS2 handling 
;              1.7  07-02-97   EkW SPR_S0157 [header] instead of header
;              1.7.107-03-97   EkW Ekki, don't sleep [history] not header !
;              1.7.230-03-98   EkW error handling
;#<

function srf_define_aar , header=header, length=length, data=data, history=history

A = { aar_rec_struct         ,$   
      wave   : 0.0           ,$   ; wavelength   [micrometer]
      flux   : 0.0           ,$   ; flux         [Jy]
      stdev  : 0.0           ,$   ; standard deviation
      tint   : 0L            ,$   ; total integration time for calculation
      det    : 0L            ,$   ; detector number
      itk    : 0L            ,$   ; Instrument time key
      utk    : 0L            ,$   ; universal time key
      rpid   : bytarr(2)     ,$   ; rasterpoint id
      spare  : bytarr(2)     ,$   ; spare bytes
      line   : 0L            ,$   ; line number
      sdir   : 0L            ,$   ; scan direction
      scnt   : 0L            ,$   ; scan count
      status : 0L            ,$   ; status word
      flag   : 0L            }    ; flag word

B = { laar_rec_struc         ,$   
      wave   : 0.0           ,$   ; wavelength   [micrometer]
      flux   : 0.0           ,$   ; flux         [Jy]
      stdev  : 0.0           ,$   ; standard deviation
      waveu  : 0L            ,$   ; total integration time for calculation
      det    : 0L            ,$   ; detector number
      itk    : 0L            ,$   ; Instrument time key
      utk    : 0L            ,$   ; universal time key
      rpid   : 0             ,$   ; rasterpoint id
      line   : 0L            ,$   ; line number
      sdir   : 0L            ,$   ; scan direction
      scnt   : 0L            ,$   ; scan count
      status : 0L            ,$   ; status word
      flag   : 0L            }    ; flag word

;check length
if n_elements(length) le 0 then begin
  if n_elements (data) le 0 then begin
    length = 1 
  endif else length = n_elements(data)
endif

if n_elements(header) le 0 then begin                 ;check header
    if srf_makeheader(header, 'AAR') eq -1 then begin
      srf_aas_error,'F','Cannot create AAR header',"SAAH"
      AAR=''
      return, AAR
    endif
endif

;update NAXIS
  header = srf_write_fits_key(header,'NAXIS2',length,'I','number of rows',/second)

if n_elements(history) le 0 then history=strarr(1)    ;check history

if n_elements(data) gt 0 then begin                   
  AAR= {type   : 'SAAR'        ,$                     ; structure type
        history: [history]     ,$                     ; the given history
        header : header        ,$                     ; the header string
        data   : data }                               ; the given data  
endif else begin
  AAR= {type   : 'SAAR'      ,$                     ; structure type
        history: [history]   ,$                     ; history string
        header : header      ,$                     ; the header string
        data   : replicate({aar_rec_struct},length) } ; (empty) data  
endelse


return, AAR
end
;#> srf_fits2host.dc3
; Identifier   srf_fits2host
;
; Purpose      Convert numbers from FITS to HOST 
;
; Synopsis     host = srf_fits2host( fits )
;
; Arguments    name    I/O   Type:    Description:
;              ---------------------------------------------------------------
;              fits     I    intarr   Array of short integers in FITS byteorder
;                       I    lonarr   Array of long integers in FITS byteorder
;                       I    fltarr   Array of floats in IEEE format
;                       I    dblarr   Array of doubles in IEEE format
;
; Returns      Array of numbers with the same type as the input array
;
; Description  This routine converts short or long integers from
;              FITS byte-ordering to HOST byte-ordering.
;              This is nothing more than a byteswapping of these
;              integers depending on the byte-ordering on the target
;              machine.
;              Occassionally, this routine is its own inverse, so
;              there's no need for a host2fits.
;
; Example      print, srf_fits2host(25)   ; The result is 6400 on f.e. VMS
;                                     ; and 25 under unix
;              print, srf_fits2host(25L)  ; The result is 419430400L on f.e. VMS
;                                     ; and 25L under unix
;
; Comment      ---
;
; Category     UTIL
;
; Filename     srf_fits2host.pro
;
; Author       D.R.Boxhoorn
;
; Version      1.3
;
; History      1.0  17-10-95 DRB Created
;              1.1  30-11-95 DRB IEEE floats also catered for
;              1.2  05-12-95 DRB IEEE doubles also catered for
;              1.3  28-10-96 FL  SPR_S0103, also byteswap for OSF
;              1.4  27-11-00 BV  SPR_S0590, also byteswap for linux
;#<

FUNCTION srf_fits2host, k
   
; Until now only byteswap for 'vms'
; and also ALPHA-OSF and linux
   if ( !version.os eq 'vms' or !version.os eq 'OSF' or $
        !version.os eq 'linux' ) then begin
      sz = size( k )
      numdims = sz(0)
      case sz( numdims + 1 ) of
      2: byteorder, k, /sswap
      3: byteorder, k, /lswap
      4: byteorder, k, /xdrtof
      5: byteorder, k, /xdrtod
      else :
      endcase
   endif

   return, k

END
;#> srf_rd_fitsfile_hdr.dc3
; Identifier   srf_rd_fitsfile_hdr
; 
; Purpose      Read the header from a FITS file
; 
; Synopsis     header = srf_rd_fitsfile_hdr( filename [, /keep_history ] [,/mc]
;                       [,mpath=mpath] )
; 
; Arguments    Name         I/O  Type:    Description:
;              -------------------------------------------------------
;              filename      I   string   Name of a FITS file or
;                                         TDT OSN in the case of MC usage
;              /keep_history I   --       Switch to keep HISTORY lines
;              mpath=mpath   I   string   mc path
;              fileid        I   string   filid in case of PIPELINE
;
; Returns      String that contains the entire FITS header
; 
; Description  A FITS file is opened and the header is extracted.
;              The extraction is based on the fact that the end of
;              a FITS header is marked by a line that starts with
;              'END'. If a FITS extension is present then two
;              of these 'END' lines are present.
;              The header is copied in blocks of 2880 bytes
;              until the block with the last 'END' line is found.
;              The default is to ignore all blocks that only
;              contain lines with the HISTORY keyword.
;              The switch /keep_history will ensure that those blocks
;              that contain only HISTORY lines will not be removed.
; 
; Comment      --
; 
; Example      print,strmid(srf_rd_fitsfile_hdr('CAL01.fits'), 560, 80)   returns
;              DATE    = '13/12/93'           / Date of writting: DD/MM/YY
; 
; Category     I/O
; 
; Filename     srf_rd_fitsfile_hdr.pro
; 
; Author       D.R.Boxhoorn
; 
; Version      2.2
; 
; History      1.0  01-08-94 DRB Copied the few working parts from READFITS
;                                in the ASTROLIB and added the rest
;              1.1  31-03-95 HB  Unified header
;              1.2  05-12-95 DRB Give FATAL message for non-existing files
;              2.0  10-11-97 EkW adaption for MC usage
;              2.1  27-11-97 EkW error handling
;              2.2  08-08-00 EkW build in MC interface
;              2.3  08-08-00 EkW filename - fileinput
;#<

function srf_rd_fitsfile_hdr, fileinput, keep_history = keep_history,$
         mpath=mpath, fileid=fileid

ON_ERROR, 2   ; Return to the routine calling this one. For offline use.

COMMON pipeline, PIPELINE

; Check for filename input
   if n_params() lt 1 then begin
      srf_aas_error, 'M', "Syntax - header = SRF_RD_FITSFILE_HDR( fileinput )",'SASP'
      return, -1
   endif

; Open file and read header information
   IF PIPELINE EQ 1 THEN BEGIN
;file to search  arc_dat and old_dat
     IF KEYWORD_SET (fileid) THEN BEGIN
       filename = mpath+fileid+fileinput+'.hdr'
       OPENR, unit, filename, error=err, /GET_LUN
       IF (err NE 0 ) THEN BEGIN       
         filename = 'old_dat:'+fileid+fileinput+'.hdr'
         OPENR, unit, filename, error=err, /GET_LUN
         IF (err NE 0) THEN $
           srf_aas_error, 'F', 'Could not open ' + filename,'SAOF'
       ENDIF
;straight file inclusive path
     ENDIF ELSE BEGIN
       openr, unit, fileinput, error=err, /GET_LUN
       IF (err NE 0) THEN $
         srf_aas_error, 'F', 'Could not open ' + fileinput,'SAOF'
     ENDELSE
;IA environment
   ENDIF ELSE BEGIN   
     OPENR, unit, fileinput, error=err, /GET_LUN, /BLOCK
     IF ( err NE 0 ) THEN srf_aas_error, 'F', 'Could not open ' + fileinput,'SAOF'
   ENDELSE

; On VMS, FITS file must be fixed record length (actual length doesn't matter)
   if !VERSION.OS eq 'vms' then begin
      file = fstat( unit )
      if file.rec_len eq 0 then $
         srf_aas_error, 'W', 'WARNING - ' + strupcase(filename) + $
                 ' is not a fixed record length file','SANF'
   endif

   if PIPELINE eq 1 then $
     block = bytarr( 80, 1, /NOZERO ) $
   else $
     block = bytarr( 80, 36, /NOZERO )

   header = ""                             ; Start with an empty header
   NumEnds = 1                             ; Find at least one 'END' FITS-item
   while NumEnds gt 0 do begin
      ON_IOERROR,end_of_file 

      readu, unit, block

      curblk = string( block )

; Find 'END' and 'EXTEND' FITS-header items in this block if present
      endline = where( strmid(curblk,0,8) eq 'END     ', Nend )
      extline = where( strmid(curblk,0,8) eq 'EXTEND  ', Next )

; If an 'END' FITS-header item is found in this block one less is needed
      if Nend ne 0 then NumEnds = NumEnds - 1

; If an 'EXTEND' FITS-header item is found to be 'TRUE' in this block
; then one more 'END' FITS-header item is needed
      if Next ne 0 then begin
         val = strpos(curblk(extline), 'T', 10)
         if (val(0) ge 0) and (val(0) lt 30) then $  ; EXTEND = T
            NumEnds = NumEnds + 1                    ; Need one more 'END' line
      endif

; Append the current block to the header
      dummy = where( strmid(curblk, 0, 8) eq 'HISTORY ', NumHist )

      if PIPELINE eq 1 then begin
        if ((NumHist ne 36) or keyword_set(keep_history)) then $
          header = header + string( reform( block, 80 ) )
      endif else begin
        if ((NumHist ne 36) or keyword_set(keep_history)) then $
           header = header + string( reform( block, 2880 ) )
      endelse
   endwhile

; Close the file
   free_lun, unit

; Return header string
   return,  header

end_of_file:
   free_lun, unit
   IF PIPELINE ne 1 THEN $
     srf_aas_error, 'W', 'EOF encountered while reading FITS header','SAEO'

   return,header

end
;#> srf_aas_error.dc2
; Identifier   srf_aas_error
;
; Purpose      Depending on the environment call IA error or treat errors      
;
; Call         srf_aas_error, code, message, pcode
;
; Arguments    Name   I/O  Type:       Description:
;              ------------------------------------------------------------
;              code    I    string      Aux code ('O','D','M','W','E','F')
;                                       for defining the  OK, Debug, Message, 
;                                       Waring, Serious, Fatal status
;              message I    string      Message string
;              pcode   I    string      pipeline code (4char)                        
;
; Returns      none
;
; Description  If the PIPELINE environment is setup the program searchs
;              in the message_text. file for the associated message and
;              displays it. In the case of the status SERIOUS and FATLAL
;              all opened files will ne closed and the program returns
;              onto the main IDL level
;
; Comment      ---
;
; Example      ---
;
; Category     AAS
;
; Filename     srf_aas_error
;
; Author       E.Wieprecht
;
; Version      1.2
;
; History      1.0  27-11-97  EkW  first draft
;              1.1  02-dec-97 EkW  message_text. path 
;              1.2  03-aug-00 EkW 
;#<

PRO srf_aas_error, code, message, pcode

COMMON pipeline, PIPELINE

ON_ERROR, 2   ; Return to the routine calling this one. For offline use.

IF PIPELINE EQ 1 THEN BEGIN
;Read error list
  stringl = 1200

;endless loop will be finish by read error
  WHILE(1) DO BEGIN                      
;define string array to store input
    errorstr = STRARR(stringl)

;open the file, in the case of problem issue error which is not in the list !!
    OPENR,lu,'ISO_DAT:message_text.', /GET_LUN, ERR=err
    IF err NE 0 THEN BEGIN
      PRINT,'SAMO : message_text. can not be opened !'
      CLOSE,/ALL
      RETALL
    ENDIF
;finish loop if string array was bigger than actual file input
    ON_IOERROR, goon
    READF,lu,errorstr
;if necessary make sting array bigger and start again
    stringl = stringl + 500
    FREE_LUN,lu
  ENDWHILE  

goon:
  FREE_LUN,lu

;search string 
  errorstr      = STRUPCASE(errorstr)
  pcode         = STRUPCASE(pcode) 

  idx           = WHERE( STRPOS(errorstr,pcode ) eq 0,cnt)

  CASE cnt of 
     0     : BEGIN
             ;ignore DEBUG messages
             IF STRUPCASE (code) NE 'D' THEN $
               PRINT,'SANL This error message is not in the list !'
               error_message = pcode + ' ' + message 
             END
     1     : BEGIN
               idx           = idx(0)
               strerror      = errorstr(idx) 
               error_message = pcode + ' ' + message
             END
     ELSE  : BEGIN
               PRINT,'SAML : This message is more times in the list !'
               error_message = pcode + ' ' + message 
             END
  ENDCASE

;ignore DEBUG messages
  IF STRUPCASE (code) NE 'D' THEN $
     PRINT,error_message

  CASE STRUPCASE (code) OF 
      'E' : BEGIN
              CLOSE,/ALL
              RETALL
            END 
      'F' : BEGIN
              CLOSE,/ALL
              RETALL
            END
       ELSE : BEGIN
              ENDELSE
  ENDCASE
    
;in the case of non pipeline env call IA error module
ENDIF ELSE $
  srf_error,code, message

END
;#> srf_makeheader.dc1
; Identifier     srf_makeheader
;
; Purpose        Create an empty or a real FITS header for the given data
;
; Synopsis       status = srf_makeheader( header [,type] [,data=data] )
;
; Arguments      Name    I/O  Type     Description
;                -----------------------------------------------------
;                header   O   String   header
;                type     I   String   Type of header. Allowable values:
;                                      'ERD' , 'SPD', 'GLL' , 'AAR', 'APH', 
;                                      'CSH' , 'IRPH', 'IIPH', 'FLUXTAB'
;                                      Any other value gives a basic header
;                data     I   any      Struc/array
;
; Returns        Status giving header length or fail code (-1) as integer
;
; Description    - Create an empty header if no data is specified.
;                - Create a header with filled in items when data is specified
;
; Comment        - Version 2.0 is compaible with the older version!
;                  Thats why the parameters in the routine IS NOT CHANGED In the
;                  more logical order:
;                     Status= SRF_MAKEHEADER(Header, [,data] [,type=type])
;
; Example        STATUS = SRF_MAKEHEADER( HEAD,'ERD')
;                   Generate an ERD header
;
;                STATUS = SRF_MAKEHEADER( HEAD,'')
;                   Create a header with just the basic keywords in
;
;                STATUS = SRF_MAKEHEADER( HEAD,DATA=READCAL('16A'))
;                   Create a header for the founded tag_names and array size
;
;                LIST=DEFINE_LINELIST(20)
;                S=SRF_MAKEHEADER(HEAD,DATA=LIST)
;                LIST.HEADER=HEAD
;                   Create a line_list header
;
;
; Category       UTIL
;
; Filename       srf_makeheader.pro
;
; Author         K. Leech ---> KL
;
; Version        4.2
;
; History        1.0   6-07-94  KL  First draft of .dc2 written by KL
;                1.1  14-07-94  KL  Modified and readied for CoCo by KL
;                1.11 20-07-94  KL  Modified Name changed from 
;                                   create_fits_header to srf_makeheader to 
;                1.12 29-07-94  KL  Corrected keyword definitions
;                1.13           NS  Change dc2 level to dc1
;                1.14 08-12-94  EkW add CSH and APH
;                2.0  15-06-95  NS  add data for fill in the keywords
;                2.1  28-07-95  EkW complete check and correction for different
;                                   data products 
;                                   add IRPH, IIPH and FLUXTAB headers
;                                   introduction of units
;                3.0  08-11-95  KL  Corrected ERD, SPD & AAR headers
;                3.2  01-12-95  NS  Clear default IA header
;		 3.3  16-04-96  DK  Adapted to changes in srf_write_fits_key
;		 3.4  23-04-96  DK  Put BITPIX in 2nd headerblock too
;                3.5  26-09-96  FL  SPR_S0061 Create a proper header for
;                                   o.a. CAL19, also added a few extra
;                                   keywords to the default list
;                3.6  25-10-96  FL  SPR_S0100 error for array entries in struct.
;                3.7  30-08-99  EkW SPR_S0486 make DATE keyword Y2K compliant
;                3.8  06-12-99  RoT add GLL
;                3.9  10-05-00  EkW update SPD part for PIPELINE 9
;                4.0  03-08-00  EkW adapt for pipeline 9.5 (I*2 in GLL)
;                4.1  11-10-00  EkW add proper NAXIS1 treatment for calfiles
;                               FL  correct type check, default use SRF_GET_NAXIS1
;                4.2  17-08-01  FL  default unit of stdev in AAR is uV/s
;#<

;========================================================================

;  Small routine to extend a header to a multiple of 2880 bytes by
;  padding it with blanks

pro extend_head, head

while ( ( strlen(head) MOD 2880) ne 0 ) do head = head + ' '
   
end

;========================================================================


function srf_makeheader, head, type, data=data, tunit=tunit

;  Headers present in all files
srf_get_date2,dte
base_head $
        = [ 'SIMPLE  =                    T / Standard FITS format', $
            'BITPIX  =                    8 / 8-bits unsigned integers', $
            'NAXIS   =                    0 / Empty Prime data matrix', $
            'EXTEND  =                    T / FITS extension may be present',$
            'ORIGIN  = ''ESA     ''         / Written by MAKEHEADER ', $
            'DATE    = '''+ dte +'''        / Date of writing: YYYY-MM-DD']




; Extra base for ERD, SPD and AAR files
base_b= [ $
'TELESCOP= ''ISO     ''           / Infrared Space Observatory          ', $
'INSTRUME= ''SWS     ''           / Instrument used                     ', $
'COMMENT    SWS Standard Processed Data                                 ', $
'FILENAME= ''            ''       / File name in ISO archive            ', $
'FILEVERS= ''        ''           / Version ID in ISO archive           ', $
'OLPVERS = ''        ''           / SOC OLP system version              ', $
'CALGVERS= ''        ''           / SOC OLP CAL-G version               ', $
'USERNAME= ''        ''           / Unofficial data product             ', $
'OBJECT  = ''        ''           / Target ID as given by proposer      ', $
'OBSERVER= ''        ''           / Proposer ID in ISO Mission DB       ', $
'EQUINOX =               2000.0 / Equinox                               ', $
'TMRATE  =                   32 / Telemetry rate in Kbps (Kbits/sec)    ', $
'EOHAUTCS= ''           ''        / Approx. UTC of start of observation ', $
'EOHAUTCE= ''           ''        / Approx. UTC of end of observation   ', $
'EOHAAOTN= ''        ''           / AOT name                            ', $
'EOHAPLID= ''        ''           / Proposal ID                         ', $
'EOHAOSN = ''        ''           / Observation sequence number         ', $
'EOHAPSN = ''        ''           / Pointing sequence number            ', $
'EOHAPCAT= ''        ''           / Proposal category                   ', $
'EOHACIND= ''        ''           / Calibration indicator               ', $
'EOHATTYP= ''        ''           / Target type                         ', $
'ATTUTCSL= ''           ''        / UTC of start time of slew to intended target   ', $
'ATTUTCS = ''           ''        / UTC of time of first arrival at intended target', $
'ATTOTFTH=                 10.0 / On-target flag threshold (arc secs)   ', $
'ATTRA   =                  0.0 / Intended Right Ascension of instrument viewing ', $
'ATTDEC  =                  0.0 / Intended DEClination (with ATTRA)     ', $
'ATTTYPE = ''        ''           / Type of attitude operation (P/R/T)  ', $
'ATTGUIDE=               150002 / Guide star reference number           ', $
'ATTSAANG=                  0.0 / Solar aspect angle (degrees)          ', $
'ATTERROR=                    0 / Contingency flag(0=success; 1=target) ', $
'TREFUTC1=            235177948 / UTC (whole seconds since 01-01-1989)  ', $
'TREFUTC2=              8818650 / UTC (remaining fraction of second)    ', $
'TREFUTK =           1857877152 / ISO Uniform Time Key (UTK)            ', $
'TREFITK =           1857877152 / ISO INSTRUMENT Time Key (ITK)         ', $
'TREFITKU=     0.04166666666667 / ITK unit length in seconds            ' ]




; Base c for SPD and AAR
base_c = [ $
'TREFCOR1=                    0 /UTC of 1st reference time                 ', $
'TREFPHA1=                  0.0 /Orbital phase at TREFCOR1                 ', $
'TREFHEL1=                  0.0 /Heliocentric correction +(s) at TREFCOR1  ', $
'TREFDOP1=                  0.0 /ISO velocity towards target (km/s) at TC1 ', $
'TREFCOR2=                    0 /UTC of 2nd reference time                 ', $
'TREFPHA2=                  0.0 /Orbital phase at TREFCOR2                 ', $
'TREFHEL2=                  0.0 /Heliocentric correction +(s) at TREFCOR2  ', $
'TREFDOP2=                  0.0 /ISO velocity towards target (km/s) at TC2 ', $
'TREFCOR3=                    0 /UTC of 3rd reference time                 ', $
'TREFPHA3=                  0.0 /Orbital phase at TREFCOR3                 ', $
'TREFHEL3=                  0.0 /Heliocentric correction +(s) at TREFCOR3  ', $
'TREFDOP3=                  0.0 /ISO velocity towards target (km/s) at TC3 ', $
'ISRRSEV =                    1 /RTA maximum severity level = WA           ', $
'ISRQSEV =                    1 /QLA maximum severity level = WA           ', $
'ISRRWARN= ''OK       ''        /RTA warning flag from IS user             ', $
'ISRNOOSL=                    0 /No. of out-of-soft-limit errors           ', $
'ISRNOOHL=                    0 /No. of out-of-hard-limit errors           ', $
'ISRNMW  =                    0 /No. of monitor warnings                   ', $
'ISRNCVW =                    0 /No. of command verification errors        ', $
'ISRNBTW =                    0 /No. of bad telemetry errors               ', $
'ISRNMVW =                    0 /No. of memory verification errors         ', $
'ISRNSQLA=                    0 /No. of severe QLA errors                  ', $
'INSTRA  =                  0.0 /Reference instrument J2000 right asce (deg) ',$
'INSTDEC =                  0.0 /Reference instrument J2000 declination (deg)',$
'INSTROLL=                  0.0 /Reference instrument J2000 roll angle (deg) ',$
'CINSTRA =                  0.0 /Corrected reference instrument J2000 ri as  ',$
'CINSTDEC=                  0.0 /Corrected reference instrument J2000 declina',$
'CINSTROL=                  0.0 /Corrected reference instrument J2000 roll ang']

; Extra base for GLL files
base_d= [ $
'TELESCOP= ''ISO     ''           / Infrared Space Observatory          ', $
'INSTRUME= ''SWS     ''           / Instrument used                     ', $
'COMMENT    SWS Standard Processed Data                                 ', $
'FILENAME= ''            ''       / File name in ISO archive            ', $
'FILEVERS= ''        ''           / Version ID in ISO archive           ', $
'OLPVERS = ''        ''           / SOC OLP system version              ', $
'USERNAME= ''        ''           / Unofficial data product             ' ]


; End for ERD, SPD and AAR
end_head = [ 'END' ]



; ERD specific binary headers
erd_bin  = [ $
'XTENSION= ''BINTABLE''           / Binary table FITS extension  ', $
'BITPIX  =                    8 /                                ', $
'NAXIS   =                    2 /                                ', $
'NAXIS1  =                  144 /                                ', $
'NAXIS2  =                26592 / Number of rows                 ', $
'PCOUNT  =                    0 /                                ', $
'GCOUNT  =                    1 /                                ', $
'TFIELDS =                   24 /                                ', $
'TFORM1  = ''1J      ''           /                              ', $
'TTYPE1  = ''GPSCTKEY''           / Instrument time key          ', $
'TFORM2  = ''2B      ''           /                              ', $
'TTYPE2  = ''GPSCRPID''           / Raster point ID              ', $
'TFORM3  = ''1I      ''           /                              ', $
'TTYPE3  = ''GPSCFILL''           / Filler                       ', $
'TFORM4  = ''12I     ''           /                              ', $
'TTYPE4  = ''SWERDR5 ''           / 5-micron array readouts      ', $
'TFORM5  = ''12I     ''           /                              ', $
'TTYPE5  = ''SWERDR10''           / 10-micron array readouts     ', $
'TFORM6  = ''12I     ''           /                              ', $
'TTYPE6  = ''SWERDR20''           / 20-micron array readouts     ', $
'TFORM7  = ''12I     ''           /                              ', $
'TTYPE7  = ''SWERDR35''           / 35-micron array readouts     ', $
'TFORM8  = ''4I      ''           /                              ', $
'TTYPE8  = ''SWERDRFP''           / FP detector readouts         ', $
'TFORM9  = ''1I      ''           /                              ', $
'TTYPE9  = ''SWERHK1 ''           / SW scanner - actual position ', $
'TFORM10 = ''1I      ''           /                              ', $
'TTYPE10 = ''SWERHK2 ''           / SW scanner - LVDT output     ', $
'TFORM11 = ''1I      ''           /                              ', $
'TTYPE11 = ''SWERHK3 ''           / SW scanner motor current     ', $
'TFORM12 = ''1I      ''           /                              ', $
'TTYPE12 = ''SWERHK4 ''           / SW scanner DAC output        ', $
'TFORM13 = ''1I      ''           /                              ', $
'TTYPE13 = ''SWERHK5 ''           / LW scanner - actual position ', $
'TFORM14 = ''1I      ''           /                              ', $
'TTYPE14 = ''SWERHK6 ''           / LW scanner - LVDT output     ', $
'TFORM15 = ''1I      ''           /                              ', $
'TTYPE15 = ''SWERHK7 ''           / LW scanner motor current     ', $
'TFORM16 = ''1I      ''           /                              ', $
'TTYPE16 = ''SWERHK8 ''           / LW scanner DAC output        ', $
'TFORM17 = ''1I      ''           /                              ', $
'TTYPE17 = ''SWERHK9 ''           / last received internal command ', $
'TFORM18 = ''1I      ''           /                                ', $
'TTYPE18 = ''SWERHK10''           / temp. sensor FP cal. source    ', $
'TFORM19 = ''1I      ''           /                                ', $
'TTYPE19 = ''SWERHK11''           / Fabry-Perot main current       ', $
'TFORM20 = ''1I      ''           /                                ', $
'TTYPE20 = ''SWERHK12''           / Fabry-Perot correction current #1 ', $
'TFORM21 = ''1I      ''           /                                   ', $
'TTYPE21 = ''SWERHK13''           / Fabry-Perot correction current #2 ', $
'TFORM22 = ''1I      ''           /                                   ', $
'TTYPE22 = ''SWERSLHK''           / Slowly commutated HK: whereas all ', $
'TFORM23 = ''1I      ''           /                                   ', $
'TTYPE23 = ''SWERIRC ''           / Internal readout counter          ', $
'TFORM24 = ''1I      ''           /                                   ', $
'TTYPE24 = ''SWEREXEC''           / "Execute" flags                   ', $
'TNULL9  =               -32768 / Null value for column SWERHK1       ', $
'TNULL10 =               -32768 / Null value for column SWERHK2       ', $
'TNULL11 =               -32768 / Null value for column SWERHK3       ', $
'TNULL12 =               -32768 / Null value for column SWERHK4       ', $
'TNULL13 =               -32768 / Null value for column SWERHK5       ', $
'TNULL14 =               -32768 / Null value for column SWERHK6       ', $
'TNULL15 =               -32768 / Null value for column SWERHK7       ', $
'TNULL16 =               -32768 / Null value for column SWERHK8       ', $
'TNULL17 =               -32768 / Null value for column SWERHK9       ', $
'TNULL18 =               -32768 / Null value for column SWERHK10      ', $
'END' ]



; SPD only binary headers
spd_bin  = [ $
'XTENSION= ''BINTABLE''           / Binary table FITS extension  ', $
'BITPIX  =                    8 /                                ', $
'NAXIS   =                    2 /                                ', $
'NAXIS1  =                 1092 /                                ', $
'NAXIS2  =                 1108 /                                ', $
'PCOUNT  =                    0 /                                ', $
'GCOUNT  =                    1 /                                ', $
'TFIELDS =                   14 /                                ', $
'TFORM1  = ''1J      ''           /                              ', $
'TTYPE1  = ''GPSCTKEY''           / Instrument time key          ', $
'TUNIT1  = ''        ''           /                              ', $
'TFORM2  = ''2B      ''           /                              ', $
'TTYPE2  = ''GPSCRPID''           / Raster point ID              ', $
'TUNIT2  = ''        ''           /                              ', $
'TFORM3  = ''1I      ''           /                              ', $
'TTYPE3  = ''GPSCFILL''           / Filler                       ', $
'TFORM4  = ''1J      ''           /                              ', $
'TTYPE4  = ''SWSPSTAT''           / Status of this record        ', $
'TUNIT4  = ''        ''           /                              ', $
'TFORM5  = ''2E      ''           /                              ', $
'TTYPE5  = ''SWSPGPOS''           / Average positions for gratings 1/2 ', $
'TUNIT5  = ''        ''           /                              ', $
'TFORM6  = ''2E      ''           /                              ', $
'TTYPE6  = ''SWSPGANG''           / Angles for gratings 1/2      ', $
'TUNIT6  = ''deg     ''           /                              ', $
'TFORM7  = ''1J      ''           /                              ', $
'TTYPE7  = ''SWSPFPOS''           / Position of FP               ', $
'TUNIT7  = ''        ''           /                              ', $
'TFORM8  = ''3E      ''           /                              ', $
'TTYPE8  = ''SWSPFCUR''           / Average main current for FP coils ', $
'TUNIT8  = ''chan    ''           /                              ', $
'TFORM9  = ''2E      ''           /                              ', $
'TTYPE9  = ''SWSPFGAP''           / Gaps for FP                  ', $
'TUNIT9  = ''um      ''           /                              ', $
'TFORM10 = ''52E     ''           /                              ', $
'TTYPE10 = ''SWSPWAVE''           / Detector wavelength          ', $
'TUNIT10 = ''um      ''           /                              ', $
'TFORM11 = ''52E     ''           /                              ', $
'TTYPE11 = ''SWSPFLUX''           / Detector flux                ', $
'TUNIT11 = ''uV/s    ''           /                              ', $
'TFORM12 = ''52E     ''           /                              ', $
'TTYPE12 = ''SWSPOFFS''           / Offsets of slopes            ', $
'TUNIT12 = ''uV      ''           /                              ', $
'TFORM13 = ''52E     ''           /                              ', $
'TTYPE13 = ''SWSPSTDV''           / Standard deviation           ', $
'TUNIT13 = ''uV/s    ''           /                              ', $
'TFORM14 = ''52J     ''           /                              ', $
'TTYPE14 = ''SWSPFLAG''           / Flags per detector           ', $
'TUNIT14 = ''        ''           /                              ', $
'END' ]         


; GLL only binary headers
gll_bin  = [ $
'XTENSION= ''BINTABLE''           / Binary table FITS extension  ', $
'BITPIX  =                    8 /                                ', $
'NAXIS   =                    2 /                                ', $
'NAXIS1  =                  940 /                                ', $
'NAXIS2  =                    0 / this is filled by    fitstable ', $
'PCOUNT  =                    0 /                                ', $
'GCOUNT  =                    1 /                                ', $
'TFIELDS =                    8 /                                ', $
'TFORM1  = ''1J      ''           /                              ', $
'TTYPE1  = ''GPSCTKEY''           / Instrument time key          ', $
'TUNIT1  = ''        ''           /                              ', $
'TFORM2  = ''52I     ''           /                              ', $
'TTYPE2  = ''SAMPLES1''           / 1st Sample start             ', $
'TUNIT2  = ''        ''           /                              ', $
'TFORM3  = ''52I     ''           /                              ', $
'TTYPE3  = ''SAMPLEE1''           / 1st Sample end               ', $
'TUNIT3  = ''        ''           /                              ', $
'TFORM4  = ''52E     ''           /                              ', $
'TTYPE4  = ''HEIGHT1 ''           / 1st Sample height            ', $
'TUNIT4  = ''        ''           /                              ', $ 
'TFORM5  = ''52I     ''           /                              ', $
'TTYPE5  = ''SAMPLES2''           / 2nd Sample start             ', $
'TUNIT5  = ''        ''           /                              ', $
'TFORM6  = ''52I     ''           /                              ', $
'TTYPE6  = ''SAMPLEE2''           / 2nd Sample end               ', $
'TUNIT6  = ''        ''           /                              ', $
'TFORM7  = ''52E     ''           /                              ', $
'TTYPE7  = ''HEIGHT2 ''           / 2nd Sample height            ', $
'TUNIT7  = ''        ''           /                              ', $
'TFORM8  = ''52I     ''           /                              ', $
'TTYPE8  = ''GLNUMBER''           / Total glitches               ', $
'TUNIT8  = ''        ''           /                              ', $
'END' ]         


;  AAR only binary headers
aar_bin  = [ $
'XTENSION= ''BINTABLE''           / Binary table FITS extension  ', $
'BITPIX  =                    8 /                                ', $
'NAXIS   =                    2 /                                ', $
'NAXIS1  =                   52 /                                ', $
'NAXIS2  =                46384 /                                ', $
'PCOUNT  =                    0 /                                ', $
'GCOUNT  =                    1 /                                ', $
'TFIELDS =                   14 /                                ', $
'TFORM1  = ''1E      ''           /                              ', $
'TTYPE1  = ''SWAAWAVE''           / wavelength of data point     ', $
'TUNIT1  = ''um      ''           /                              ', $
'TFORM2  = ''1E      ''           /                              ', $
'TTYPE2  = ''SWAAFLUX''           / flux                         ', $
'TUNIT2  = ''Jy      ''           /                              ', $
'TFORM3  = ''1E      ''           /                              ', $
'TTYPE3  = ''SWAASTDV''           / standard deviation           ', $
'TUNIT3  = ''uV/s    ''           /                              ', $
'TFORM4  = ''1J      ''           /                              ', $
'TTYPE4  = ''SWAATINT''           / total integration time       ', $
'TUNIT4  = ''s       ''           /                              ', $
'TFORM5  = ''1J      ''           /                              ', $
'TTYPE5  = ''SWAADETN''           / detector number              ', $
'TUNIT5  = ''        ''           /                              ', $
'TFORM6  = ''1J      ''           /                              ', $
'TTYPE6  = ''SWAAITK ''           / SWS instrument time key      ', $
'TFORM7  = ''1J      ''           /                              ', $
'TTYPE7  = ''SWAAUTK ''           / ISO uniform time key         ', $
'TFORM8  = ''2B      ''           /                              ', $
'TTYPE8  = ''SWAARPID''           / raster point id              ', $
'TUNIT8  = ''        ''           /                              ', $
'TFORM9  = ''2B      ''           /                              ', $
'TTYPE9  = ''SWAASPAR''           / spare                        ', $
'TFORM10 = ''1J      ''           /                              ', $
'TTYPE10 = ''SWAALINE''           / line number                  ', $
'TUNIT10 = ''        ''           /                              ', $
'TFORM11 = ''1J      ''           /                              ', $
'TTYPE11 = ''SWAASDIR''           / scan direction               ', $
'TUNIT11 = ''        ''           /                              ', $
'TFORM12 = ''1J      ''           /                              ', $
'TTYPE12 = ''SWAASCNT''           / scan number                  ', $
'TUNIT12 = ''        ''           /                              ', $
'TFORM13 = ''1J      ''           /                              ', $
'TTYPE13 = ''SWAASTAT''           / status word                  ', $
'TUNIT13 = ''        ''           /                              ', $
'TFORM14 = ''1J      ''           /                              ', $
'TTYPE14 = ''SWAAFLAG''           / flag word                    ', $
'TUNIT14 = ''        ''           /                              ', $
'END' ]         




; APH specific header
aph_head  = [ 'XTENSION= ''BINTABLE''      / Binary Table FITS Extension', $
            'NAXIS   =                      2 /', $
            'NAXIS1  =                    168 /', $
            'NAXIS2  =                      0 /', $
            'PCOUNT  =                      0 /', $
            'GCOUNT  =                      1 /', $
            'TFIELDS =                     27 /', $
            'TFORM1  = ''2J      ''           /', $
            'TTYPE1  = ''SLEWSTA ''           / start time of slew to target', $
            'TUNIT1  = ''SEC SINCE 1989.0''   /', $
            'TFORM2  = ''2J      ''           /', $
            'TTYPE2  = ''SLEWEND ''           / end time of slew to target', $
            'TUNIT2  = ''SEC SINCE 1989.0''   /', $
            'TFORM3  = ''1D      ''           /', $
            'TTYPE3  = ''RA      ''           / RA of intended view dir.', $
            'TUNIT3  = ''DEGREE  ''           /', $
            'TFORM4  = ''1D      ''           /', $
            'TTYPE4  = ''DEC     ''           / DEC of intended view dir.', $
            'TUNIT4  = ''DEGREE  ''           /', $
            'TFORM5  = ''1E      ''           /', $
            'TTYPE5  = ''OTFTHRES''           / On target flag threshold', $
            'TUNIT5  = ''ARCSECONDS''         /', $
            'TFORM6  = ''1A      ''           /', $
            'TTYPE6  = ''INSTID  ''           / Instrument id (C,L,P,S)', $
            'TUNIT6  = ''        ''           /', $
            'TFORM7  = ''1A      ''           /', $
            'TTYPE7  = ''REQTYPE ''           / Type of request', $
            'TUNIT7  = ''        ''           /', $
            'TFORM8  = ''1I      ''           /', $
            'TTYPE8  = ''APERTID ''           / Aperture ID', $
            'TUNIT8  = ''        ''           /', $
            'TFORM9  = ''1I      ''           /', $
            'TTYPE9  = ''NGRID   ''           / Number of grid points', $
            'TUNIT9  = ''        ''           /', $
            'TFORM10 = ''1I      ''           /', $
            'TTYPE10 = ''NRASTER ''           / Number of lines in the raster', $
            'TUNIT10 = ''        ''           /', $
            'TFORM11 = ''1I      ''           /', $
            'TTYPE11 = ''TSLEW1  ''           / Time to slew/dwell on 1th point',$
            'TUNIT11 = ''SECONDS ''           /', $
            'TFORM12 = ''1I      ''           /', $
            'TTYPE12 = ''TSLEW2  ''           / Ditto for a point on cur.line',$
            'TUNIT12 = ''        ''           /', $
            'TFORM13 = ''1I      ''           /', $
            'TTYPE13 = ''TSLEW3  ''           / Ditto for 1st point on next line',$
            'TUNIT13 = ''        ''           /', $
            'TFORM14 = ''1I      ''           /', $
            'TTYPE14 = ''SCANDIST''           / Dist. betw. points on scan line',$
            'TUNIT14 = ''ARCSECONDS''         /', $
            'TFORM15 = ''1I      ''           /', $
            'TTYPE15 = ''LINEDIST''           / Dist. between scan lines',$
            'TUNIT15 = ''ARCSECONDS''         /', $
            'TFORM16 = ''1B      ''           /', $
            'TTYPE16 = ''ORIENT  ''           / Orientation raster flag',$
            'TUNIT16 = ''        ''           /', $
            'TFORM17 = ''5B      ''           /', $
            'TTYPE17 = ''SPARE   ''           /',$
            'TUNIT17 = ''        ''           /', $
            'TFORM18 = ''1E      ''           /', $
            'TTYPE18 = ''ROTATE  ''           / Rotation on raster pattern',$
            'TUNIT18 = ''DEGREE  ''           /', $
            'TFORM19 = ''4D      ''           /', $
            'TTYPE19 = ''QSSQUAT ''           / QSS quaternion',$
            'TUNIT19 = ''        ''           /', $
            'TFORM20 = ''4D      ''           /', $
            'TTYPE20 = ''MISQUAT ''           / QSS/star tracker misalignment',$
            'TUNIT20 = ''        ''           /', $
            'TFORM21 = ''4D      ''           /', $
            'TTYPE21 = ''STARVEC ''           / Guide star unit vector',$
            'TUNIT21 = ''        ''           /', $
            'TFORM22 = ''1J      ''           /', $
            'TTYPE22 = ''REFSTAR ''           / Guide star ref no in catalogue',$
            'TUNIT22 = ''        ''           /', $
            'TFORM23 = ''1E      ''           /', $
            'TTYPE23 = ''SOLASPCT''           / Solar aspect angle',$
            'TUNIT23 = ''DEGREE  ''           /', $
            'TFORM24 = ''1E      ''           /', $
            'TTYPE24 = ''RPEAMP  ''           / RPE amplitude',$
            'TUNIT24 = ''ARCSECONDS''         /', $
            'TFORM25 = ''1A      ''           /', $
            'TTYPE25 = ''ATTCONT ''           / Attitude continuation flag',$
            'TUNIT25 = ''        ''           /', $
            'TFORM26 = ''1A      ''           /', $
            'TTYPE26 = ''CONTINGE''           / Contigency flag',$
            'TUNIT26 = ''        ''           /', $
            'TFORM27 = ''2B      ''           /', $
            'TTYPE27 = ''SPARE2  ''           /', $
            'TUNIT27 = ''        ''           /',$
             'END' ]


; APH specific tail
aph_tail = [ 'TSTART  = ''       ''          / Start time of interval', $
             'TSTOP   = ''       ''          / end time of interval',$
             'GENTIM  = ''       ''          / file generation time',$
             'REVNUM  =                    0 / Revolution number' ,$
             'APHVERS =                    0 / APH version number',$
             'MIDASFTP= ''TABLE  ''          / MIDAS file type',$
             'END' ]

aph_tail=[aph_tail , strarr(36-n_elements(aph_tail)) ]

; CSH specific header
csh_head = [ 'XTENSION= ''BINTABLE''      / Binary Table FITS Extension', $
            'NAXIS   =                      2 /', $
            'NAXIS1  =                     96 /', $
            'NAXIS2  =                      0 /', $
            'PCOUNT  =                      0 /', $
            'GCOUNT  =                      1 /', $
            'TFIELDS =                      8 /', $
            'TFORM1  = ''48B     ''           /', $
            'TTYPE1  = ''SSTACSGP''           / general prefix', $
            'TUNIT1  = ''        ''           /', $
            'TFORM2  = ''1I      ''           /', $
            'TTYPE2  = ''SSTASHUT''           / shutter position', $
            'TUNIT2  = ''        ''           /', $
            'TFORM3  = ''6I      ''           /', $
            'TTYPE3  = ''SSTAGAIN''           / RA of intended view dir.', $
            'TUNIT3  = ''        ''           /', $
            'TFORM4  = ''4I      ''           /', $
            'TTYPE4  = ''SSTACALS''           / calibration source status', $
            'TUNIT4  = ''        ''           /', $
            'TFORM5  = ''2I      ''           /', $
            'TTYPE5  = ''SSTARESP''           / reset pulse interval', $
            'TUNIT5  = ''        ''           /', $
            'TFORM6  = ''1I      ''           /', $
            'TTYPE6  = ''SSTAHKMD''           / house keeping mode', $
            'TUNIT6  = ''        ''           /', $
            'TFORM7  = ''1I      ''           /', $
            'TTYPE7  = ''SSTAFPS ''           / FP selection', $
            'TUNIT7  = ''        ''           /', $
            'TFORM8  = ''9I      ''           /', $
            'TTYPE8  = ''SPARE   ''           / spare', $
            'TUNIT8  = ''        ''           /', $
            'END']

; CSH specific tail
csh_tail = [ 'CSGPUKST=                    0 / UTK start time', $
             'CSGPUKEN=                    0 / UTK end time',$
             'CSGPIKST=                    0 / ITK start time',$
             'CSGPIKEN=                    0 / ITK end time' ,$
             'MIDASFTP= ''TABLE  ''          / MIDAS file type',$
             'END' ]
;             'CSGPUTST= ???                   0 / ',$
;             'CSGPUTEN= ???                   0 / ',$
;             'CSGPOSN = ???                   0 / ',$

csh_tail=[csh_tail , strarr(36-n_elements(csh_tail)) ]

; IRPH specific header
irph_head = [ 'XTENSION= ''BINTABLE''      / Binary Table FITS Extension', $
            'NAXIS   =                      2 /', $
            'NAXIS1  =                     72 /', $
            'NAXIS2  =                      0 /', $
            'PCOUNT  =                      0 /', $
            'GCOUNT  =                      1 /', $
            'TFIELDS =                      7 /', $
            'TFORM1  = ''1J      ''           /', $
            'TTYPE1  = ''UTC     ''           / expected UTC of start point', $
            'TUNIT1  = ''        ''           /', $
            'TFORM2  = ''10B     ''           /', $
            'TTYPE2  = ''SPARE   ''           / spare', $
            'TUNIT2  = ''        ''           /', $
            'TFORM3  = ''4B      ''           /', $
            'TTYPE3  = ''RPID    ''           / raster point id', $
            'TUNIT3  = ''        ''           /', $
            'TFORM4  = ''4D      ''           /', $
            'TTYPE4  = ''RPQ     ''           / raster point quaternion', $
            'TUNIT4  = ''        ''           /', $
            'TFORM5  = ''1D      ''           /', $
            'TTYPE5  = ''RA      ''           / raster point ref. right asc.', $
            'TUNIT5  = ''DEG     ''           /', $
            'TFORM6  = ''1D      ''           /', $
            'TTYPE6  = ''DEC     ''           / raster point ref. decl.', $
            'TUNIT6  = ''DEG     ''           /', $
            'TFORM7  = ''1D      ''           /', $
            'TTYPE7  = ''ROLL    ''           / raster point ref. roll angle', $
            'TUNIT7  = ''DEG     ''           /', $
            'END']

; IRPH specific tail
irph_tail = ['ATTSTRQ1=                  0.0  / star-tracker quaternion Q1', $
             'ATTSTRQ2=                  0.0 / star-tracker quaternion Q2', $
             'ATTSTRQ3=                  0.0 / star-tracker quaternion Q3', $
             'ATTSTRQ4=                  0.0 / star-tracker quaternion Q4', $
             'ATTMISQ1=                  0.0 / STR/QSS misalignment quat. Q1',$
             'ATTMISQ2=                  0.0 / STR/QSS misalignment quat. Q2',$
             'ATTMISQ3=                  0.0 / STR/QSS misalignment quat. Q3',$
             'ATTMISQ4=                  0.0 / STR/QSS misalignment quat. Q4',$
             'ATTINSQ1=                  0.0 / QSS instr. alignment quat. Q1',$
             'ATTINSQ2=                  0.0 / QSS instr. alignment quat. Q2',$
             'ATTINSQ3=                  0.0 / QSS instr. alignment quat. Q3',$
             'ATTINSQ4=                  0.0 / QSS instr. alignment quat. Q4',$
             'END' ]

irph_tail = [ irph_tail , strarr(36-n_elements(irph_tail)) ]


; IIPH specific header
iiph_head = [ 'XTENSION= ''BINTABLE''      / Binary Table FITS Extension', $
            'NAXIS   =                      2 /', $
            'NAXIS1  =                    144 /', $
            'NAXIS2  =                      0 /', $
            'PCOUNT  =                      0 /', $
            'GCOUNT  =                      1 /', $
            'TFIELDS =                      9 /', $
            'TFORM1  = ''18I     ''           /', $
            'TTYPE1  = ''SPARE   ''           / spare', $
            'TUNIT1  = ''        ''           /', $
            'TFORM2  = ''2I      ''           /', $
            'TTYPE2  = ''RPID    ''           / raster point quaternion', $
            'TUNIT2  = ''        ''           /', $
            'TFORM3  = ''1J      ''           /', $
            'TTYPE3  = ''UTK     ''           / UTK time key', $
            'TUNIT3  = ''        ''           /', $
            'TFORM4  = ''4D      ''           /', $
            'TTYPE4  = ''RPQ     ''           / raster point quaternion', $
            'TUNIT4  = ''        ''           /', $
            'TFORM5  = ''4D      ''           /', $
            'TTYPE5  = ''STRQ    ''           / instant. star tracker quatern.', $
            'TUNIT5  = ''        ''           /', $
            'TFORM6  = ''4D      ''           /', $
            'TTYPE6  = ''MISQ    ''           / instant. STR/QSS misal. quat.', $
            'TUNIT6  = ''        ''           /', $
            'TFORM7  = ''1D      ''           /', $
            'TTYPE7  = ''RA      ''           / instant. right ascension', $
            'TUNIT7  = ''DEG     ''           /', $
            'TFORM8  = ''1D      ''           /', $
            'TTYPE8  = ''DEC     ''           / instant. declination', $
            'TUNIT8  = ''DEG     ''           /', $
            'TFORM9  = ''1D      ''           /', $
            'TTYPE9  = ''ROLL    ''           / instant. roll angle', $
            'TUNIT9  = ''DEG     ''           /', $
            'END']

;  FLUXTAB only headers
fltab_head = [ $
            'XTENSION= ''BINTABLE''         / Binary Table FITS Extension', $
            'BITPIX  =                    8 /', $
            'NAXIS   =                    2 /', $
            'NAXIS1  =                   72 /', $
            'NAXIS2  =                    1 /', $
            'PCOUNT  =                    0 /', $
            'GCOUNT  =                    1 /', $
            'TFIELDS =                   12 /', $
            'TFORM1  = ''16A     ''           /', $
            'TTYPE1  = ''OBS_DAT''            / observation date', $
            'TUNIT1  = ''        ''           /',$
            'TFORM2  = ''16A     ''           /', $
            'TTYPE2  = ''OBJ_TYPE''           / object type', $
            'TUNIT2  = ''        ''           /',$
            'TFORM3  = ''1J      ''           /', $
            'TTYPE3  = ''det''                / detector number', $
            'TUNIT3  = ''        ''           /',$
            'TFORM4  = ''1E      ''           /', $
            'TTYPE4  = ''FLUX''               / flux', $
            'TUNIT4  = ''        ''           /',$
            'TFORM5  = ''1E      ''           /', $
            'TTYPE5  = ''KEY_WAVE''           / key wavelength', $
            'TUNIT5  = ''        ''           /',$
            'TFORM6  = ''1E      ''           /', $
            'TTYPE6  = ''PASSBAND''           / passband', $
            'TUNIT6  = ''        ''           /',$
            'TFORM7  = ''1E      ''           /', $
            'TTYPE7  = ''S0D     ''           / det int. calib. dur. obs',$
            'TUNIT7  = ''        ''           /',$
            'TFORM8  = ''1E      ''           /', $
            'TTYPE8  = ''S_F''                / S0(lam_key)/F0(lam_key)', $
            'TUNIT8  = ''        ''           /',$
            'TFORM9  = ''1E      ''           /', $
            'TTYPE9  = ''REL_FLUX''           / relative flux', $
            'TUNIT9  = ''        ''           /',$
            'TFORM10 = ''1E      ''           /', $
            'TTYPE10 = ''S0D_ERR''            / error in S0D', $
            'TUNIT10 = ''        ''           /',$
            'TFORM11 = ''1E      ''           /', $
            'TTYPE11 = ''S_F_ERR''            / error in S_F', $
            'TUNIT11 = ''        ''           /',$
            'TFORM12 = ''1E      ''           /', $
            'TTYPE12 = ''REL_FLUX_ERR''       / error in relative flux',$
            'TUNIT12 = ''        ''           /']
 
;IRPH specific tail
iiph_tail = ['ATTINSQ1=                  0.0 / QSS instr. alignment quat. Q1',$
          'ATTINSQ2=                  0.0 / QSS instr. alignment quat. Q2',$
          'ATTINSQ3=                  0.0 / QSS instr. alignment quat. Q3',$
          'ATTINSQ4=                  0.0 / QSS instr. alignment quat. Q4',$
          'END' ]

iiph_tail=[iiph_tail , strarr(36-n_elements(iiph_tail)) ]


;  Zero head
head = ''

;  Copy Base keywords over
srf_cp_fits_string, head, base_head

; Decode type (is it ERD, SPD, GLL, AAR, CSH, APH or '' ?)
if n_elements(type) le 0 then $
    if n_elements(data) gt 0 then $
	if total(tag_names(data) eq 'TYPE') gt 0 then type=data.type 
if n_elements(type) le 0 then type = ''
srf_aas_error,'D','type: '+type, 'SUDI'

case strupcase( type ) of
  'ERD'   : begin
;             srf_cp_fits_string ensures that each line is 80 chars long.
              srf_cp_fits_string, head, base_b
              srf_cp_fits_string, head, end_head

; Now ensure that header is a multiple of 2880 bytes
              extend_head, head

; Now copy over binary header and again extend
              srf_cp_fits_string, head, erd_bin
              extend_head, head
              status = strlen( head )
            end
  'SPD'   : begin
              srf_cp_fits_string, head, base_b
              srf_cp_fits_string, head, base_c
              srf_cp_fits_string, head, end_head

; Now ensure that header is a multiple of 2880 bytes
              extend_head, head

; Now copy over binary header and again extend
              srf_cp_fits_string, head, spd_bin
              extend_head, head
              status = strlen( head )
            end  
  'GLL'   : begin
              srf_cp_fits_string, head, base_d
              srf_cp_fits_string, head, end_head

; Now ensure that header is a multiple of 2880 bytes
              extend_head, head

; Now copy over binary header and again extend
              srf_cp_fits_string, head, gll_bin
              extend_head, head
              status = strlen( head )
            end
  'AAR'   : begin
              srf_cp_fits_string, head, base_b
              srf_cp_fits_string, head, base_c
              srf_cp_fits_string, head, end_head

; Now ensure that header is a multiple of 2880 bytes
              extend_head, head

; Now copy over binary header and again extend
              srf_cp_fits_string, head, aar_bin
              extend_head, head
              status = strlen( head )
            end
  'APH'   : begin
               srf_cp_fits_string, head, aph_tail
               srf_cp_fits_string, head, aph_head
               status = strlen( head )
             end
  'FLUXTAB': begin
              srf_cp_fits_string, head, [ 'COMMENT    Intem. Prod. FLUXTAB' ]
              srf_cp_fits_string, head, [ 'END' ]
              while ((strlen(head) MOD 2880) NE 0) do $
                      srf_cp_fits_string, head, [ ' ' ]

              srf_cp_fits_string, head, fltab_head
              srf_cp_fits_string, head, [ 'END' ]
              while ((strlen(head) MOD 2880) NE 0) do $
                     srf_cp_fits_string, head, [ ' ' ]
              status = strlen( head )
            end

  'CSH'   : begin 
              srf_cp_fits_string, head, csh_tail
              srf_cp_fits_string, head, csh_head
              status = strlen( head )
            end   
  'IRPH'  : begin
              srf_cp_fits_string, head, irph_tail
              srf_cp_fits_string, head, irph_head
              status = strlen( head )
            end
  'IIPH'  : begin
              srf_cp_fits_string, head, iiph_tail
              srf_cp_fits_string, head, iiph_head
              status = strlen( head )
            end

  else    : begin		; Undefined so just leave base keywords in
              srf_cp_fits_string, head, 'END'
	      extend_head, head
              status = strlen( head )
            end
endcase

; fill in keywords depends of the data type and size
   
if n_elements(data) gt 0 then begin

; start with secondary header
  head = srf_write_fits_key( head, 'XTENSION', 'BINTABLE', 'S', $
			'Binary Table FITS Extension', /sec )
  head = srf_write_fits_key( head,  'BITPIX', 8, 'I', $
			'8-bits unsigned integers', /sec )

  input = data					; save input variable
  if ( total( where( tag_names( input ) eq 'DATA' ) ) ge 0 ) then $
	input = input.data

  s = size( input )
  if ( s(0) ge 1 ) then ndim = 2 else ndim = 1
  strndim = strtrim( string( ndim ), 2 )
  head = srf_write_fits_key( head, 'NAXIS', strndim, 'I', 'Tables are ' + $
			strndim + '-D char. array', /sec )
   
  if n_tags(input) gt 0 then begin		; data is a structure

    names  = strlowcase( tag_names( input ) )
    nnames = n_elements( names )

    naxis1 = srf_get_naxis1(data)
    srf_aas_error,'D','NAXIS1/NNAMES*4 :  ' + $
                  strtrim(naxis1,2) + '  ' + strtrim(nnames * 4,2), 'SUDI'
    head = srf_write_fits_key( head, 'NAXIS1', naxis1, 'I', $
  			'Characters in a row', /sec )

    for i = 0, s(0) - 1 do begin
      dim  = strtrim( string( i + 2 ), 2 )
      ndim = strtrim( string( s(i+1) ), 2 )
      head = srf_write_fits_key( head, 'NAXIS' + dim, ndim, 'I', $
			'Table Dim', /sec )
    endfor

    head = srf_write_fits_key( head, 'PCOUNT', 0, 'I', $
			'Parameter count always 0', /sec )
    head = srf_write_fits_key( head, 'GCOUNT', 1, 'I', $
			'Group count always 1', /sec )
    head = srf_write_fits_key( head, 'TFIELDS', nnames, 'I', $
			'No of columns in table', /sec )

    type = [ '', 'B', 'I', 'J', 'E', 'E', 'C', 'S', 'Str' ]
    for i = 0, nnames - 1 do begin
      s = size( input(0).(i) )
      ns = n_elements(s)
      t = type( s( s(0)+1 ) )
      case t of
        'S'  :  begin
                  n = strtrim( string( strlen( input(0).(i) ) ), 2 ) 
                  d = 'A' + n(0)
		  t = n + 'A'
                end
        'E'  :  begin
                  d = t + '14.7'
                  n = strtrim(string(s(ns-1)),2)
                  t = n + t
                end
        'B'  :  begin
                  n = strtrim( string ( strlen(input(0).(i)) ), 2 )
                  d = ''
                  n = strtrim(string(s(ns-1)),2)
		  t = n + t
                end
        'I'  :  begin
                  n = strtrim( string ( strlen(input(0).(i)) ), 2 )
                  d = t + n(0)
                  n = strtrim(string(s(ns-1)),2)
		  t = n + t
                end
        'J'  :  begin
                  n = strtrim( string ( strlen(input(0).(i)) ), 2 )
                  d = 'I' + n(0)
                  n = strtrim(string(s(ns-1)),2)
		  t = n + t
                end
      endcase

      strN = strtrim( string( i + 1 ), 2 )
      head = srf_write_fits_key( head, 'TFORM' + strN, t, 'S', $
			'Format of field', /sec )
      if ( d NE '' ) then head = srf_write_fits_key( head, $
                                           'TDISP' + strN, d, 'S', $
			                   'Display format of field', /sec )
      head = srf_write_fits_key( head, 'TTYPE' + strN, strupcase(names(i)), 'S', $
			'Field label', /sec )
      if ( n_elements( TUNIT ) le 0 ) then begin
        label=[ [ 'Tau', 'sec' ],        [ 'val', 'V' ], $
		[ 'gain', 'dimensionless' ], $
                [ 'flux', 'Jy'], [ 'stdev', 'Jy'], $
                [ 'tint', 's' ], [ 'rpid', ' ' ], [ 'spare', ' ' ], $
                [ 'sdir', ' ' ], [ 'scnt', ' ' ], $
                [ 'status', ' ' ], [ 'flag', ' ' ], $
                [ 'itk', ' ' ], [ 'utk', ' ' ], $
                [ 'noise', 'muV/sec' ],  [ 'Wave', 'um' ], $
                [ 'fwhm', 'micrometer'], [ 'aot_name', 'string' ], $
                [ 'aot_band', 'string' ], [ 'aotspeed', ' ' ], $
                [ 'theta', 'degree' ],   [ 'band', ' ' ], [ 'det', ' ' ], $
                [ 's_f', 'uV/sec/Jy' ],  [ 'cal_source', 'W/A' ], $
                [ 'relflx', 'Jy' ], [ 'rlflxe', 'Jy' ], $
                [ 'comment', 'string' ], [ 'gap', 'micrometer' ] ]
	s  = -1
	ii = -1 					; ini/not found
	n  = n_elements( label( 0, * ) )
	while ( s LT 0 AND ii LT n - 1 ) do begin
 	  ii = ii + 1
	  s  = strpos( strlowcase( names(i) ), strlowcase( label(0,ii) ) )
	endwhile
	if ( s GE 0 ) then unit = label(1,ii) else unit = '??'

        head = srf_write_fits_key( head, 'TUNIT' + strN, unit, 'S', $
			'Physical unit of field', /sec )

      endif else begin
        head = srf_write_fits_key( head, 'TUNIT' + strN, TUNIT(I), 'S', $
			'Physical unit of field', /sec )
      endelse
    endfor

  endif else begin      				; test on tags
    s = size(input)					; test on array's
    if ( s(0) GT 1 ) then begin
    endif 
  endelse
 
endif							; test on data

; Create a header in blocks of 36 lines of 80 char
extend_head, head

status = strlen( head )

return, status

END

;#> srf_get_date2.dc1
; Identifier   srf_get_date2
;
; Purpose      Return the current date in Y2K : YYYY-MM-DD  format.
;
; Synopsis     SRF_GET_DATE2, dte
;
; Arguments    Name  I/O Type:       Description:
;              ----------------------------------------------------------
;              dte    O  string(8)   An ten character scalar string 
;                                    specifying the current day (0-31),
;                                    current month (1-12), and the current 
;                                    year
;
; Returns      none
;
; Description  This is the format required by the DATE and DATE-OBS keywords 
;              in a FITS header (Y2K format !)
;
; Comment      Basing on an ASTROLIB routine
;
; Example      Add the current date to the DATE keyword in a FITS header,h
;              SRF_GET_DATE2,dte
;              sxaddpar,h,'DATE',dte
;
; Category     UTL
;
; Version      2.0
;
; History      1.0   26-03-91  W. Landsman
;              2.0   30-08-99  E.Wieprecht
;#<

pro srf_get_date2,dte
 On_error,2

 if N_params() LT 1 then begin
     print,'Syntax - Get_date, dte'
     print,'  dte - 8 character output string giving current date'
     return
 endif

 mn = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct', $
       'Nov','Dec']

 st    = !STIME
 day   = strmid(st,0,2)
 month = strmid(st,3,3)
 month = where(mn EQ month) + 1
 yr    = strmid(st,7,4)

 dte =  string(yr + '-' + string(month,f='(i2.2)') + '-' +  $
                string(day,f='(I2.2)'))

 return
 end
;#> srf_get_naxis1.dc3
; Identifier   srf_get_naxis1
;
; Purpose      Get back the NAXIS1 FITS parameter for structure
;
; Synopsis     naxis1 = srf_get_naxis1(struct, iastring=iastring) 
;
; Arguments    Name   I/O Type:     Description:
;              ------------------------------------------------------------
;              struct   I  struct    Data structure 
;              iastring I  int       set string size interactively
; 
; Returns      NAXIS1 parameter considering possible arrays
;              0 if no calfile input
;
; Description  --
;
; Comment      
;
; Example 
;
; Category     UTIL
;
; Filename     srf_get_naxis1.pro
;
; Author       E.Wieprecht
;
; Version      1.2
;
; History      0.5  10-10-00  EkW   Creation date
;              1.0  11-10-00  FL    non-standard data-types
;                                   get string length from entries
;              1.1  18-10-00  FL    DATA as array
;              1.2  31-10-00  FL    change string length warning to debug
;
;#<

FUNCTION srf_get_naxis1, istru, iastring=iastring

;prepare return value
sum = 0 

; check how much structure-data entries we have
tags = n_tags (istru.data[0])

names = tag_names(istru.data)

ndata = n_elements(istru.data)

; loop over structure tags
FOR loop = 0, tags -1 DO BEGIN
  s = size(istru.data[0].(loop))

;in case of a scalar or array ?
;NB FL I treat arrays up to 2 dimensions
;      first dimension is the arrays size of the
;      tags in the fitsfile
;      the second dimension are the rows in the table
;      (e.g. cal 35,36)
;      as yet I don't know how larger ones are handled
;
  CASE s[0] OF
   0 : BEGIN
         mult = 1
         soa  = 1
       END
   1 : BEGIN
         mult = s[1]
         soa  = 2
       END
   2 : BEGIN
         mult = s[1]
         soa  = 3
       END
   ELSE : srf_aas_error,'F','srf_get_naxis1 : multi dim array','SMDA'
  ENDCASE

; what type ?
  v = 0
  str = 'TAG: ' + names[loop] + ' TYPE: ' + strtrim(s[soa],2)
  IF ( s[0] GT 0 ) THEN str = str + ' ARRAY LENGTH: ' + strtrim(mult,2)
  srf_aas_error,'D',str,'SUDI'
  CASE s[soa] OF
    0 : srf_aas_error,'W','srf_get_naxis1 : no value','SNVA' 
    1 : v = 1    
    2 : v = 2
    3 : v = 4
    4 : v = 4
    5 : v = 8
    6 : srf_aas_error,'W','srf_get_naxis1 : complex number','SNVA' 
    7 : BEGIN
          IF ( keyword_set(iastring) ) THEN v = iastring $
          ELSE BEGIN
            v = 4
            FOR i=0,ndata-1 DO $
              v = ( 4 * ( ceil(strlen(istru.data[i].(loop))/4.) > 1 ) ) > v
            srf_aas_error,'D', $
                         'srf_get_naxis1 : ' + $
                         names[loop] + ' tag assumed to be ' + $
                         strtrim(v,2) + ' characters !', 'SNVA'
          ENDELSE
        END
    ELSE : srf_aas_error,'F','srf_get_naxis1 : cannot handle data type '+ $
                         strtrim(s[soa],2),'SNVA'
  ENDCASE
  
  sum = sum + mult * v
ENDFOR
;
; case of a two dimensional data struct
; e.g. CAL08 
;   |---DATA<Struc>(48*177)
;                          |---PEAK_POS<Float>(1)
;                          |---STRENGTH<Float>(1)
;                          |-WAVELENGTH<Float>(1)
; the tags are expanded over the first dimension
; as TAGX_1, TAGY_1, TAGX_2, TAGY_2 ......
; the second dimension are the rows in the table
;
s = size(istru.data)
IF ( s[0] EQ 2 ) THEN sum = sum * s[1]


RETURN,sum

END
;#> sh_read_faar.dc1
; Identifier     sh_read_faar.pro
;
; Purpose        Read a STANDARD FITS formatted AAR 
;
; Call           AAR_struc = sh_read_faar( filename,
;                                       [,start=start] [,number=number]) 
;
; Arguments      Name       I/O  Type     Description
;                ------------------------------------------------------
;                filename    I   string    AAR name 
;                start       I   i*4       start second  (def=1)
;                number      I   i*4       number of secods to read 
;                                          default : whole AAR
;
; Returns        aar_struc   O   struct    IA AAR structure
;
; Description    ---
;
; Comment        ---
;              
; Example        aar = sh_read_faar('arc_dat:aar.fits',s=1,n=300)
;                read first 300 seconds from aar.fits
;
; Category       I/O
;
; Filename       sh_read_faar.pro
;
; Author         E.Wieprecht (-->ekw)
;
; Version        2.1
;
; History        1.0  10-08-1995  --> ekw  first version 
;                2.0  30-11-1995  --> NJM  Rewritten in IDL
;                2.1  16-05-2003  --> FL   check is AAR file exists
;#<

function sh_read_faar, faar_file, start=start, number=number

COMMON pipeline, PIPELINE

PIPELINE = 0

; Define IA AAR record structure
A = { aar_rec_struct         ,$   
      wave   : 0.0           ,$   ; wavelength   [micrometer]
      flux   : 0.0           ,$   ; flux         [Jy]
      stdev  : 0.0           ,$   ; standard deviation
      tint   : 0L            ,$   ; total integration time for calculation
      det    : 0L            ,$   ; detector number
      itk    : 0L            ,$   ; Instrument time key
      utk    : 0L            ,$   ; universal time key
      rpid   : bytarr(2)     ,$   ; rasterpoint id
      spare  : bytarr(2)     ,$   ; spare bytes
      line   : 0L            ,$   ; line number
      sdir   : 0L            ,$   ; scan direction
      scnt   : 0L            ,$   ; scan count
      status : 0L            ,$   ; status word
      flag   : 0L            }    ; flag word

B = { laar_rec_struc         ,$   
      wave   : 0.0           ,$   ; wavelength   [micrometer]
      flux   : 0.0           ,$   ; flux         [Jy]
      stdev  : 0.0           ,$   ; standard deviation
      waveu  : 0L            ,$   ; total integration time for calculation
      det    : 0L            ,$   ; detector number
      itk    : 0L            ,$   ; Instrument time key
      utk    : 0L            ,$   ; universal time key
      rpid   : 0             ,$   ; rasterpoint id
      line   : 0L            ,$   ; line number
      sdir   : 0L            ,$   ; scan direction
      scnt   : 0L            ,$   ; scan count
      status : 0L            ,$   ; status word
      flag   : 0L            }    ; flag word

;Read and check AAR header

;   f = findfile( faar_file, count=count )
   f = file_search( faar_file, count=count )
   if count eq 0 then begin
     srf_error,'S',faar_file+' does not exist'
     return,0
   endif
   aar_head = srf_rd_fitsfile_hdr( faar_file )
  
;Get the number of rows from the file read
   status = srf_read_fits_key( aar_head, 'NAXIS2', rows )
   
;Check keywords and set defaults
   if ( keyword_set(start)  )  then firstsec = start-1  else firstsec = 0L
   if ( keyword_set(number) )  then lastsec  = number   else lastsec  = rows
   lastsec = lastsec + firstsec
   if ( firstsec lt 0L   ) then firstsec = 0L
   if ( lastsec  gt rows ) then lastsec  = rows
   lastsec = lastsec-1



;Dummy variable to skip the FITS header when reading the file
   block = bytarr( strlen(aar_head) )
   
;define an empty aar struct to be used as a template to read in the data
   aar_data = replicate( {aar_rec_struct}, rows )


;Read FITS AAR data
   openr, unit, faar_file, /get_lun, /block
   
   readu, unit, block
   readu, unit, aar_data
   
   free_lun, unit


;Byteswap the parts of the AAR where necessary
   aar_data.wave   = srf_fits2host( aar_data.wave )
   aar_data.flux   = srf_fits2host( aar_data.flux )
   aar_data.stdev  = srf_fits2host( aar_data.stdev )
   aar_data.tint   = srf_fits2host( aar_data.tint )
   aar_data.det    = srf_fits2host( aar_data.det )
   aar_data.itk    = srf_fits2host( aar_data.itk  )
   aar_data.utk    = srf_fits2host( aar_data.utk  )
   aar_data.line   = srf_fits2host( aar_data.line )
   aar_data.sdir   = srf_fits2host( aar_data.sdir )
   aar_data.scnt   = srf_fits2host( aar_data.scnt )
   aar_data.status = srf_fits2host( aar_data.status )
   aar_data.flag   = srf_fits2host( aar_data.flag )



;Return the AAR structure
   return, srf_define_aar( header=aar_head, data=aar_data(firstsec:lastsec) )


end
