;#> srf_read_fits_key.dc2
; Identifier   srf_read_fits_key
;
; Purpose      Read a keyword value from a fits header
;
; Synopsis     status = srf_read_fits_key( header, keyword, value, comment $
;                                      [, first=first] 
;                                      [, type=type] )
;                                      [, second=second]
;
; Arguments    Name      I/O Type    Description
;              -----------------------------------------------------
;              header     I  string  Header definition
;              keyword    I  string  Keyword to read from 
;              value      O          Value from keyword. The type of this 
;                                    depends on the type in the keyword.
;                                    Strings stay as strings
;                                    Integers stay as integers 
;                                    Real stay as reals
;                                    Logicals are returned as integers as 
;                                    either 1 (true) or 0 (false)
;              comment    O  string  Comment from keyword
;              status     O  integer Return status code
;                                    0 success
;                                    1 keyword not present
;                                    2 keyword present but not in the 
;                                      specified header
;              type       O  string  Type of keyword
;                                    S = string
;                                    I = integer
;                                    R = real
;                                    L = logical
;              first     I/O boolean If on input defined it will look 
;                                    in the first header.
;                                    On output it will tell if the keyword
;                                    was present in first header.
;              second    I/O boolean If on input defined it will look
;                                    in the second header.
;                                    Becomes obsolete if FIRST is defined.
;                                    On output it will tell if the keyword 
;                                    was present in second header.
;
; Description  Read a keyword from a header
;
;              With FIRST and SECOND one can control the search
;              of the keyword. Specifying them forces the routine
;              to search in the first or the secondary header.
;
;              If a keyword was not found in a specified header it 
;              will be looked for in the other header and STATUS=2 
;              will be returned if found there.
;
;              By default the routine will look for the first
;              occurance of the keyword in the header.
;
; Returns      Success code in status.
;
; Comment      --    
;
; Example      status = srf_read_fits_key( aar.header, 'EOHAAOTN', value, comment )
;              Gets the AOT number and comment from an AA header
;
;              The parameters FIRST, SECOND and TYPE allow easy update of
;              a keyword without conversion or location errors.
;
;                 first = 0 & second = 0  ; look for first occurance
;                 status = srf_read_fits_key( aar.header, 'NAXIS2, value, comm, $
;                                         first=first, second=second, type=type)
;                 header = srf_write_fits_key(aar.header, 'NAXIS2, naxis2, type, $
;                                         comm, stat, first=first, $
;                                         second=second )
;                 if ( status eq 0 and stat eq 0 ) then aar.header = header
;
;              This assumes the keyword is only present once in either the 
;              main header or the extension.
;
; Category     UTIL
;
; Filename     srf_read_fits_key.pro (header_routines)
;
; Author       K. Leech ---> KL
;
; Version      1.4
;
; History      1.0   6-07-94  KL  First draft of .dc2 file 
;              1.0  14-07-94  KL  Code written 
;              1.01 29-07-94  KL  Some bugs removed
;              1.02 19-09-94  PRR Deleted keyword not found error message
;              1.03 13-01-95  KL  Ensured searched for right keyword
;              1.1  11-04-95  HB  Unified header
;              1.2  03-11-95  DRB Integers are LONG by default when reading
;              1.21 23-11-95  KL  Comments improved
;              1.3  05-03-98  FL  SPR_S0358, keyword in first or second header
;                                            and return type
;                                 SPR_S0353, problems with / in string 
;              1.4.110-03-98  FL  SPR_S0358, check on incorrect fits header
;              1.4.230-03-98  EkW increase accuracy for FLOAT by using DOUBLE
;              1.5  01-04-98  EkW changes of version 1.3 removed due to
;                                 endless looping in PIPELINE
;              1.6  02-04-98  EkW introduce v1.3 mods again after solving 
;                                 problem
;#<

   function srf_read_fits_key, header, keyword, value, comment, $
                           second=second, first=first, type=type

;  Can we find the keyword in the present header?
   key2 = strmid( keyword + '        ', 0, 8 )
   head2 = header
   status = 0
;
; find the end of the first header
;
   end_loc = 0
   end_key = strmid( 'END' + '        ', 0, 8 )
   end_loc = strpos( head2, end_key, end_loc )
   while ( fix(end_loc/80)*80 ne end_loc and end_loc ne -1 ) do begin
     end_loc = fix(end_loc/80+1) * 80
     end_loc = strpos( head2, end_key, end_loc )
   endwhile

   if ( n_elements(second) eq 0 ) then second = 0B
   if ( n_elements(first) eq 0  ) then first  = 0B
   if ( first ) then second = 0B

   if ( second ) then t_loc = end_loc else t_loc = 0
;
; USED VARIABLES
;
;   loc     absolute location of 'KEYWORD' in header
;   t_loc   location where we start the search
;   end_loc location of the end of the first header
;
;   first/second on input determine if we have to search in the
;                first or second header, on output it will tell
;                if or where we found it.
;
;
; if we look for a keyword in the second header start at the
; end of the first header
;
   read_key:
   loc = strpos( head2, key2, t_loc )
;
; not found in specified second header
; set status to 2 and continue in the first header
;
   if ( second and loc eq -1 ) then begin
     status = 2
     second = 0B
     first  = 0B
     t_loc = 0
     goto,read_key
   endif
   if ( loc ne -1 ) then begin
;
; is it at the start of a line
;
      if( fix(loc/80)*80 eq loc ) then begin
;
; if present but not in the specified first header, 
; return status 2, but do read the value
;
        if ( first and loc gt end_loc ) then begin
          first = 0B
          second = 1B
          status = 2
          goto, found
        endif
;
; present in first or second header?
;
        first = 0B & second = 0B
        if ( loc lt end_loc ) then first = 1B else second = 1B
        goto, found
      endif
;
; continue search after the previous 'false' location
;
      t_loc = fix(loc/80+1) * 80
      goto, read_key
   endif


found:
   if ( loc eq -1 ) then begin
;     Can't find keyword
      status = 1
      first = 0B
      second = 0B
   endif else begin
;     We've found it.

;     Reset value
      value = ''
;     Now extract the value field and the comment
      temp = strmid( header, loc, 80 )
      loc1 = strpos( temp, '=' )
      loc2 = strpos( temp, '/' )
      t_value = strmid( temp, loc1+1, loc2 - loc1 - 1 )
      comment = strmid( temp, loc2 + 1, 80 - loc2 )

;     Is the value a string? Search for '
      loc = strpos( t_value, '''' )
      if ( loc ne -1 ) then begin
;        Value is a string
;        re-determine the location of the / in case
;        a / exists in the string
         s_start = strpos( temp, '''', loc1 )
         s_end   = strpos( temp, '''', s_start+1 )
         value   = strmid( temp, s_start+1, s_end - s_start - 1 )
         loc2    = strpos( temp,  '/', s_end+1 )
         comment = strmid( temp, loc2 + 1, strlen(temp) - loc2 )
         type = 'S'
      endif else begin
;        Is the value a number or a logical?
;        Logicals are either T or F
         if ( strpos( t_value, 'F' ) ne -1 ) then begin
            value = 0
            type = 'L'
         endif else if ( strpos( t_value, 'T' ) ne -1 ) then begin
            value = 1
            type = 'L'
         endif else if ( strpos( t_value, '.' ) ne -1 ) then begin 
;           It's a real
            value = DOUBLE(t_value)
            type = 'R'
         endif else begin
;           It's an integer which should be LONG by default!!!!
            value = 0L   + t_value
            type = 'I'
         endelse         
      endelse         
   endelse

   return, status
end
