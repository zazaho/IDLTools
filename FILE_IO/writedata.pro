;-------------------------------------------------------------
;+
; NAME:
;        WRITEDATA
;
; PURPOSE:
;        write a 2 dimensional data array and header information to a file
;
; CATEGORY:
;        file handling routines
;
; CALLING SEQUENCE:
;        WRITEDATA,FILENAME,DATA [,HEADER,UNITS,CFACT,MCODE] [,keywords]
;
; INPUTS:
;        FILENAME --> the name of the output file. If the output file 
;            exists, the user will be prompted for a new name unless
;            keyword NO_WARN is set.
;
;        DATA --> The data array to be written. 
;
;        HEADER --> An optional string array containing variable names.
;            These will be composed to a string using the DELIM delimiter.
;            Note that the HEADER information can also be passed in the 
;            pre-formatted COMMENTS keyword parameter.
;
;        UNITS, CFACT, MCODE --> string arrays that will be added to the
;            file header concatenated with blank delimiters. These parameters 
;            are optional and merely there to facilitate creating chem1d
;            model input files.
;
; KEYWORD PARAMETERS:
;        TITLE --> A title string that will be the first header line.
;            It is also possible to pass a string array here, although for
;            more complicate file headers it is recommended to pre-format
;            teh file header and pass it in the COMMENTS keyword.
; 
;        DELIM --> A delimiter character for the HEADER (variable name) 
;            items. Default is a blank ' '.
;
;        COMMENTS --> A string array containing all the lines of the file
;            header. Note that COMMENTS overrules the input of HEADER, UNITS,
;            CFACT, and MCODE as well as TITLE.
;
;        /NO_WARN --> Suppress warning message and user prompt for a new
;            filename if the file already exists.
;
; OUTPUTS:
;        A file containing a file header and the data array written 
;        line by line.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;        DATA = findgen(3,10)
;        HEADER = ['A','B','C']
;        writedata,'test.out',DATA,HEADER,TITLE='test file',DELIM=';'
;
;        This will create a file like:
;            test file
;            A;B;C
;                  0.00000      1.00000      2.00000
;                  3.00000      4.00000      5.00000
;            ...
;
; MODIFICATION HISTORY:
;        mgs, 25 Nov 1997: VERSION 1.00
;
;-
; Copyright (C) 1997, Martin Schultz, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine writedata"
;-------------------------------------------------------------


pro writedata,filename,data,header,units,cfact,mcode, $
       title=title,delim=delim,   $
       comments=comments,  $
       no_warn=no_warn
 
 
 
    on_error,2   ; return to caller
 
; check consistency of variables, built comments string if
; not already passed
 
    if (n_params() lt 2) then begin
        print,'*** WRITEDATA : At least 2 parameters required ' + $
              '(filename and data) ! ***'
        return
    endif
 
    if (n_elements(comments) gt 0 AND comments(0) ne '' AND  $
        (n_elements(header) gt 0 OR n_elements(units) gt 0 OR $
         n_elements(cfact) gt 0 OR n_elements(mcode) gt 0) ) then begin
       print,'*** WRITEDATA : You passed data in COMMENTS and at least one '+$
             'of the parameters HEADER, UNITS, CFACT, MCODE. !'
       print,'I will use the information in COMMENTS.'
    endif
 
    if (n_elements(delim) le 0) then delim = ' '    ; blank as default
 
    if (n_elements(COMMENTS) le 0) then begin
       comments = ''     ; dummy to begin with
       if (n_elements(title) gt 0) then comments = [ comments, title ]
       if (n_elements(header) gt 0) then begin
           tmp = string(header,format='(500(A,:,"'+delim+'"))')
           comments = [ comments, tmp ]
       endif
       if (n_elements(units) gt 0) then begin
           tmp = string(units,format='(500(A,:," "))')
           comments = [ comments, tmp ]
       endif
       if (n_elements(cfact) gt 0) then begin
           tmp = string(cfact,format='(500(A,:," "))')
           comments = [ comments, tmp ]
       endif
       if (n_elements(mcode) gt 0) then begin
           tmp = string(mcode,format='(500(A,:," "))')
           comments = [ comments, tmp ]
       endif
       if (n_elements(comments) gt 1) then $
          comments = comments(1:n_elements(comments)-1)
    endif
 
 
; check if file exists and prompt user for different filename
    if (not keyword_set(NO_WARN)) then begin
       force = 0
       while (file_exist(filename) AND not force) do begin
          newname=''
          print,'## File '+filename+' exists! ' + $
                'Input new name or press enter to overwrite. A single "."' + $
                ' will abort.'
          read,newname,prompt='>>'
          newname = strcompress(newname,/remove_all)
          if (newname eq '.') then return
          if (newname eq filename OR newname eq '') then $
              force = 1 $
          else $
              filename = newname
       endwhile
    endif
 
; open file and write comments, then data line by line
 
    openw,olun,filename,/get_lun,width=8192

    if (comments(0) ne '') then $ 
       for i=0,n_elements(comments)-1 do $
           printf,olun,comments(i)
 
    ndat = n_elements(data(0,*))
    for i=0,ndat-1 do begin
        dataline = data(*,i)
        printf,olun,dataline
    endfor
 
    close,olun
    free_lun,olun
 
    print,ndat,' lines written to file '+filename+'.'
 
return
end
 
