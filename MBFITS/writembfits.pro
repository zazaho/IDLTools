;+
; NAME:
; writembfits
;
; PURPOSE:
; create a disk copy of an mbfits structure
;
; CATEGORY:
; fileio
;
; CALLING SEQUENCE:
; writembfits,'name',struct
;
; INPUTS:
; 'name': file or dir name to be created
;
; KEYWORD PARAMETERS:
; /keepold: if set then do not erase the existing file or directory
;            but rather append
;
; /writedouble: if set write the arrays as double if not convert
;               double array to single precision prior to writing
;               using tags_double2float
;
; /exploded: if set write an mbfits directory instead of a mbfits file
;
; /forcegivenname: if set use the given filename as directory name
;                  when exploding and not the origname from the header
;
; /help: if set show this help
;
; SIDE EFFECTS:
; a file or a directory is created
;
; EXAMPLE:
; writembfits,'test.fits',mb12
;
; MODIFICATION HISTORY:
;(SH Nov  9 2009) Initial release
;-

pro writembfits,filename,struct_in, $
                keepold=keepold, $
                writedouble=writedouble, $
                exploded=exploded, $
                forcegivenname=forcegivenname, $
                help=help

  needhelp=0
  if keyword_set(help) then needhelp =1
  if n_params() ne 2 then needhelp=1
  if size(filename,/tname) ne 'STRING' then needhelp=1
  if size(struct_in,/tname) ne 'STRUCT' then needhelp=1
  
  if needhelp eq 1 then begin
     doc_library,'writembfits'
     return
  end
  
  default,writedouble,0
  if not writedouble then begin
     struct = tags_double2float(struct_in)
  endif else begin
     struct = struct_in
  endelse
  
  if not keyword_set(exploded) then begin
     if not keyword_set(keepold) then spawn,'rm -f '+filename
     mwrfits,0,filename,struct.header
     mwrfits,struct.scan,filename,struct.header_scan
     mwrfits,struct.febepar,filename,struct.header_febepar
     
     nsubs = sxpar(struct.header_scan,'nsubs')
     
     for i =1,nsubs do begin
        si = n2s(i)
        foo = execute('mwrfits,struct.datapar'+si+',filename,struct.header_datapar'+si)
        foo = execute('mwrfits,struct.arraydata'+si+',filename,struct.header_arraydata'+si)
        foo = execute('mwrfits,struct.monitor'+si+',filename,struct.header_monitor'+si)
     endfor

  endif else begin
     
     if n_elements(struct) ne 0 then begin
        if keyword_set(forcegivenname) then begin
           dirname=filename
        endif else begin
           dirname = sxpar(struct.header,'origfile')
        endelse
        if not keyword_set(keepold) then spawn,'rm -rf '+dirname
        spawn,'mkdir '+dirname

        mwrfits,0,dirname+'/GROUPING.fits',struct.header
        mwrfits,struct.scan,dirname+'/SCAN.fits',struct.header_scan
        mwrfits,struct.febepar,dirname+'/ARTEMIS450-ARTBE-FEBEPAR.fits',struct.header_febepar
        nsubs = struct.nsubs
        
        for i=1,nsubs do begin
           si = n2s(i)
           spawn,'mkdir '+dirname+'/'+si
           
           foo=execute('arraydata = struct.arraydata'+si)
           foo=execute('header_arraydata = struct.header_arraydata'+si)
           mwrfits,arraydata,dirname+'/'+si+'/ARTEMIS450-ARTBE-ARRAYDATA-1.fits',header_arraydata
           
           foo=execute('monitor = struct.monitor'+si)
           foo=execute('header_monitor = struct.header_monitor'+si)
           mwrfits,monitor,dirname+'/'+si+'/MONITOR.fits',header_monitor
           
           foo=execute('datapar = struct.datapar'+si)
           foo=execute('header_datapar = struct.header_datapar'+si)
           mwrfits,datapar,dirname+'/'+si+'/ARTEMIS450-ARTBE-DATAPAR.fits',header_datapar
        endfor
     endif
  endelse
end
