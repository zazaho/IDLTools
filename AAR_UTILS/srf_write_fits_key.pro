;#> srf_write_fits_key.dc2
; Identifier   srf_write_fits_key
;
; Purpose      IDL procedure to write a fits keyword into the fake header
;
; Synopsis     new_header= srf_write_fits_key( header, keyword, value, type, 
;                                         comment, status [, FIRST=first] 
;                                         [, SECOND=second] )
;
; Arguments    Name      I/O Type    Description
;              -----------------------------------------------------
;              header     I  string  Header definition (e.g. aar.header)
;                            struct    "        "      (e.g. aar)
;              keyword    I  string  Keyword to write to
;              value      I  string  Value to write in. Either a string, a 
;                                    real, an integer or 'T' or 'F' for a 
;                                    logical
;              type       I  string  Type of keyword
;                                    S = string
;                                    I = integer
;                                    R = real
;                                    L = Logical
;              comment    I  string  Comment
;              status     O  integer Return status code
;                                    (0= success
;                                    1= old and new types differ
;                                    2= String insert called with wrong 
;                                       length string
;                                    3= fail for some other reason)
;              first      I  integer If 1 force use of primary FITS header 
;                                    (default)
;              second     I  integer If 1 force use of secondary FITS header
;
; Returns      New header as a string
;
; Description  Write a keyword and value to a header. If the header is too
;              small it gets extended and if the keyword is already 
;              present its value gets overwritten.
;              Procedure tries to ensure that you can only overwrite a 
;              keyword with one of the correct value and that strings are
;              the same length
;
; Comment      If there is an error during operation the procedure returns
;              the old header
;
; Example      temp = srf_write_fits_key( spd.header, 'EOHAAOTN', 'S01 ', 
;                                       'C', 'Test', status )
;              if ( stat eq 0 ) then spd.header = temp
;              Writes a new AOT number into a header
;
;              temp = srf_write_fits_key( spd.header, 'EXTEND', 'F', 
;                                       'L', 'Test', status )
;              if ( stat eq 0 ) then spd.header = temp
;              Changes the `EXTEND' logical to false
;
;              temp = srf_write_fits_key( spd.header, 'TFORM15', '1E', 'S', 
;                                       'Test', status, second=1 )
;              if ( stat eq 0 ) then spd.header = temp
;              Writes a TFORM into the secondary header
;
; Category     UTIL
;
; Filename     srf_write_fits_key.pro
;
; Author       K. Leech ---> KL
;
; Version      1.24
;
; History      1.0   6-07-94  KL  First draft of .dc2 file 
;              1.1  14-07-94  KL  Code written
;              1.11 14-07-94  KL  Some bugs removed
;              1.12 13-09-94  NS  Possibility of Overwriting an excisting 
;                                 keyword with a new value
;              1.12 13-09-94  NS  Go on for error levels 1 and 2  with
;                                 generation of a message.
;              1.13 19-09-94  PRR Deleted error message when new string
;                                 has different length from original
;              1.14 13-01-95  KL  Ensured searched for right keyword
;              1.15 14-03-95  HB  Unified Arguments
;                   11-04-95  HB  Unified header
;              1.16 06-11-95  DRB Add /first again for routines that use this
;              1.2  23-11-95  KL  Improved routine and corrected logical 
;                                 handling
;              1.21 24-11-95  KL  Fixed strings being less than 8 char and 
;                                 Binary header going on 2880 byte boundary
;              1.22 27-11-95  KL  Allow access to primary or binary header
;                                 by the addition of the first of second 
;                                 keywords
;              1.23 06-08-97  FL  SPR_S0260, zero values for I/R when
;                                            an empty string is passed
;              1.24 12-10-01  FL  allow for boolean logical besides 'F' or 'T'
;#<

   function srf_write_fits_key, head, keyword, value, type, comment, status, $
                            FIRST=FIRST, SECOND=SECOND

   status = 0       ; O.K. code


;  Was the input a string or a structure? Assume a string then check
   header = head
   if n_tags(header) gt 0 then $
      if total(where(tag_names(header) eq 'HEADER')) then header=head.header


;  Now cut header into two parts - primary header and binary
;  Make sure length of both is a multiple of 80 characters (though not that
;  their length is a multiple of 2880 chars - that's done later)
   prime_head = strmid( header, 0, strpos( header, 'END           ' ) + 80 )
   binar_head = strmid( header, strpos( header, 'END              ' ) + 80, $
                                strlen( header ) )
   binar_head = strtrim( binar_head, 2 )
   binar_head = strmid( binar_head, 0, strlen(binar_head)-3)
   srf_cp_fits_string, binar_head, 'END'



;  What is wanted? First or second header?
   if( n_elements( second ) eq 1 ) then $
      header = binar_head $
   else                   $
      header = prime_head


;  Can we find the requested keyword in the primary header?
   key2 = strmid( keyword + '        ', 0, 8 ) + '='
   head2 = header
   t_loc = 0

test:
   loc = strpos( head2, key2 )
   if ( loc ne -1 ) then begin
      if( loc mod 80 eq 0 ) then goto, found
      head2 = strmid( head2, fix(loc/80+1)*80, strlen(head2)-loc+80)
      t_loc = fix( loc/80 + 1 ) * 80
      goto, test
   endif


;  We've found it. Ensure loc is correct
found: 
   if ( loc ne -1 ) then begin
      loc = loc + t_loc

;     Now find type and ensure same as type passed to srf_write_fits_key
      temp = strmid( header, loc, 80 )
      loc1 = strpos( temp, '=' )
      loc2 = strpos( temp, '/' )
      t_value = strmid( temp, loc1+1, loc2 - loc1 - 1 )

;     Is the value a string? Search for '
      loc1 = strpos( t_value, '''' )
      if ( loc1 ne -1 ) then begin
;        Value is a string. Check length
         old_type = 'S'
         temp2 = strmid( t_value, loc1+1, strlen(t_value) - loc1 - 1 )
         len = strpos( temp2, '''' )
      endif else begin
;        Is the value a number or a logical?
;        Logicals are either T or F
         if ( strpos( t_value, 'F' ) ne -1 ) then begin
            old_type = 'L'
         endif else if ( strpos( t_value, 'T' ) ne -1 ) then begin
            old_type = 'L'
         endif else if ( strpos( t_value, '.' ) ne -1 ) then begin 
            old_type = 'R'
         endif else begin
            old_type = 'I'
         endelse
      endelse


;     Ensure old and new types are the same
      if( old_type ne type ) then begin
;        Old and new types differ
;         srf_aas_error, 'M', $
;         'Have overwritten ' + old_type + ' with new ' + type,'SAMM'
         status=1
      endif 
;     Strip record out of header
      temp_before = strmid( header, 0, loc )
      temp_after  = strmid( header, loc+80, strlen(header))

   endif else begin

;     Record not found, so it must go at the end 
;     END will be the last 80 character record so generate temp_before and
;     temp_after accordingly

      temp_before = strmid( header, 0, strlen( header) - 80 )
      temp_after = ''
      srf_cp_fits_string, temp_after, 'END'

   endelse


;  Generate new value (generate temp2 [=keyword+value+comment])
   case strupcase( type ) of
      'S'  : begin
                if strlen(value) lt 8 then $
                   value = strmid( value + '        ', 0, 8 )
                temp2 = key2 + ' ''' + value + ''''
                if ( strlen( value ) le 20 ) then begin
                   temp2 = temp2 + '                                        '
                   temp2 = strmid( temp2, 0, 31 )
                endif else begin
                   temp2 = temp2 + '/ ' + strtrim(comment,2)
                   temp2 = temp2 + '                                        '
                   temp2 = temp2 + '                                        '
                endelse
             end
      'I'  : begin
                if ( strlen(strtrim(value,2)) eq 0 ) then value = 0
                temp3 = '                    ' + string( value )
                temp3 = strmid( temp3, strlen( temp3 ) - 20, 20 )
                temp2 = strmid( key2 + ' ' + temp3 + ' ', 0, 31 )
             end
      'R'  : begin
                if ( strlen(strtrim(value,2)) eq 0 ) then value = 0.0
                temp3 = '                    ' + string( value )
                temp3 = strmid( temp3, strlen( temp3 ) - 20, 20 )
                temp2 = strmid( key2 + ' ' + temp3 + ' ', 0, 31 )
             end
      'L'  : begin
                if ( NOT value or strupcase(strtrim(value,2)) eq 'F' ) then begin
                   temp2 = key2 + '                    F '
                endif else begin
                   temp2 = key2 + '                    T '
                endelse
             end
      else : begin
;                srf_aas_error, 'S', 'Invalid type ' + type + ' given','SAIF'
                temp2 = ''
                status=3
             end
   endcase
   
;  Add comment, ensure correct length and generate new header
   temp2 = temp2 + '/ ' + strtrim(comment,2) + '                                      '
   temp2 = temp2 + '                                          '
   temp2 = strmid( temp2, 0, 80 )
   new_header = temp_before + temp2 + temp_after


;  Ensure new header is correct length (integer multiple of 2880 bytes)
   n = ( ( strlen(new_header) / 80 ) mod 36 )
   if n gt 0 then srf_cp_fits_string, new_header, strarr(36-n)


;  Now were we accessing the primary or binary headers?
   if( n_elements( second ) eq 1 ) then begin
;     Binary header
;     First make primary header correct length then add on secondary
      n = ( ( strlen(prime_head) / 80 ) mod 36 )
      if n gt 0 then srf_cp_fits_string, prime_head, strarr(36-n)

      new_header = prime_head + new_header
   endif else begin
;     Primary header
      new_header = new_header + binar_head

;     And again ensure correct length
      n = ( ( strlen(new_header) / 80 ) mod 36 )
      if n gt 0 then srf_cp_fits_string, new_header, strarr(36-n)
   endelse


;  Return new header
   return, new_header
end

