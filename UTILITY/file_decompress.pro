;+
; NAME:
;  file_decompress
;
; PURPOSE:
;  Decompress compressed files so that they can be read
;
; CATEGORY:
;  FILE-IO
;
; CALLING SEQUENCE:
;  tmpfile = file_decompress(filename,status=status[,/help,/delete_last,/cleanup])
;
; INPUTS:
;  filename: name of file (with full path) to be decompressed
;
; KEYWORD PARAMETERS:
;  /delete_last: if set do nothing but remove the last decompressed file
;  /cleanup: if set remove all files from the temp dir that have been
;            created by this routine
;
; OUTPUTS:
;  tmpfile: name of the decompressed temporary file
;  status: flags which indicates the status of the decompression
;          0 decompressed or copied without failure
;          1 file not found
;          2 file not unqiue
;          4 compression failed
;          8 help page shown
;          16 last file deleted
;          32 cleanup performed
;
; COMMON BLOCKS:
;  COMMON_FILE_DECOMPRESS, file_decompress_last_file
;
; SIDE EFFECTS:
;  a temporary file is created or a copy of the original file
;  is made in case compression type could not be determined
;
; EXAMPLE:
;  tmpfile = file_decompress('image.fits.xz',status=status)
;  if status eq 0 then image=readfits(tmpfile)
;  if status eq 0 then file_delete(tmpfile)
;
;  With a more compact notation
;  tmpfile = file_decompress('image.fits.xz',status=status)
;  if status eq 0 then image=readfits(tmpfile)
;  file_decompress,/delete_last
;
; MODIFICATION HISTORY:
;  (SH Sep 18 2013): initial version
;-

function file_decompress,file, $
                         status=status, $
                         help=help, $
                         delete_last=delete_last, $
                         cleanup=cleanup

  COMMON COMMON_FILE_DECOMPRESS, file_decompress_last_file

  decompressed_file_prefix = 'IDL_FILE_DECOMPRESSED_'

  if keyword_set(help) then begin
     doc_library,'file_decompress'
     status=8
     return,''
  endif     

  if keyword_set(delete_last) then begin
     if file_decompress_last_file ne '' then begin
        file_delete,file_decompress_last_file
     endif
     file_decompress_last_file=''
     status=16
     return,file_decompress_last_file
  endif

  if keyword_set(cleanup) then begin
     oldfiles=file_search(getenv('IDL_TMPDIR')+decompressed_file_prefix+'*',count=count)
     if count ne 0 then begin
        file_delete,oldfiles
     endif
     file_decompress_last_file=''
     status=32
     return,file_decompress_last_file
  endif

  if n_params() eq 0 then begin
     doc_library,'file_decompress'
     status=8
     return,''
  endif     

  file_decompress_last_file=''

  foundfile = file_search(file,count=count)
  
  if count eq 0 then begin
     message,/info,'file not found: '+file
     status=1
     return,file_decompress_last_file
  endif

  if count gt 1 then begin
     message,/info,'file not unique: '+file
     status=2
     return,file_decompress_last_file
  endif

  ;; check for compressed files
  ext=file_extension(foundfile,/lower,rootname=rootname)
  tmpfile=getenv('IDL_TMPDIR')+strcompress(decompressed_file_prefix+string(floor(randomu(time)*1d6)),/remove_all)
  exit_status=0
  case ext of 
     'bz2': begin
        spawn,'bunzip2 -c '+foundfile+' > '+tmpfile,exit_status=exit_status
     end
     'xz': begin
        spawn,'xz -d -c '+foundfile+' > '+tmpfile,exit_status=exit_status
     end
     'gz': begin
        spawn,'gunzip -c '+foundfile+' > '+tmpfile,exit_status=exit_status
     end
     else: begin
        message,'file compression not recognised: copying',/info
        spawn,'cp '+foundfile+' '+tmpfile,exit_status=exit_status
     end
  endcase

  if exit_status ne 0 then begin
     message,/info,'An error occured during decompression of: '+foundfile
     status=4
     return,file_decompress_last_file
  endif else begin
     status=0
     file_decompress_last_file=tmpfile
     return,file_decompress_last_file
  endelse

end

;; simple wrapper to allow calls to cleanup and /delete_last as a procedure
pro file_decompress,_extra=_extra
  foo=file_decompress(_extra=_extra)
end
