;+
; NAME:
; readmbfits
;
; PURPOSE:
; read an mbfits file or directory into an IDL structure
;
; CATEGORY:
; fileio
;
; CALLING SEQUENCE:
; struct = readmbfits('name')
;
; INPUTS:
; 'name': string specifying the file or directory to read from
;
; KEYWORD PARAMETERS:
; nsubs=nsubs: maximum number of sub scan to read
; /help: if set show this help
; 
; OUTPUTS:
; the function returns a structure which holds the mb fits data
;
; EXAMPLE:
; struct = readmbfits('APEXBOL.2009-07-24T14:04:23.000.fits')
; struct = readmbfits('APEX-43961-2009-07-23-C-083.F-0184A-2009')
;
; MODIFICATION HISTORY:
;(SH Nov  9 2009) Initial release
;-

function readmbfits,filename, $
                    nsubs=nsubs, $
                    help=help

  needhelp=0
  if keyword_set(help) then needhelp =1
  if n_params() ne 1 then needhelp=1
  if size(filename,/tname) ne 'STRING' then needhelp=1
  if needhelp eq 1 then begin
     doc_library,'readmbfits'
     return,-1
  end

  fileinfo = file_info(filename)
  if fileinfo.exists ne 1 then begin
     message,'the given file '+filename+' does not correspond to an existing file or directory'
  endif
  
  if fileinfo.directory eq 0 then begin
     
;; read the structure extension by extension
;; it seems
;; the first one is global info
;; 2 SCAN-MBFITS
;; 3 FEBEPAR-MBFITS
;; after there are a number of 3 sets of extensions
;; (DATAPAR-MBFITS,ARRAYDATA-MBFITS,MONITOR-MBFITS)
;; The number of pairs is contained in the header of SCAN-MBFITS
;; as NOBS or NSUBS

;; do the reading with mrdfits which gives immediately a nice structure
     
     foo = mrdfits(filename,0,header)
     scan = mrdfits(filename,1,header_scan)
     febepar = mrdfits(filename,2,header_febepar)
     
     nsubs_max = sxpar(header_scan,'nsubs')
     
     if n_elements(nsubs) eq 0 then begin
        nsubs = nsubs_max
     endif else begin
        nsubs = (nsubs<nsubs_max)
     endelse
     
     ;; start building the command needed to create the entire structure
     make_structure_command = 'struc={header:header,scan:scan,header_scan:header_scan,febepar:febepar,header_febepar:header_febepar,nsubs:nsubs'
     
     for i =1,nsubs do begin
        si = n2s(i)
        ;; BUG: this will fail if ever the order of the extensions changes
        ;; it would be better to check the header keywords
        foo = execute('datapar'  +si+'=mrdfits(filename,i*3+0,header_datapar'   +si+')')
        foo = execute('arraydata'+si+'=mrdfits(filename,i*3+1,header_arraydata'+si+')')
        foo = execute('monitor'  +si+'=mrdfits(filename,i*3+2,header_monitor'   +si+')')
        make_structure_command = make_structure_command + $
                                 ',datapar'+si+':datapar'+si+ $
                                 ',arraydata'+si+':arraydata'+si+ $
                                 ',monitor'+si+':monitor'+si+$
                                 ',header_datapar'+si+':header_datapar'+si+ $
                                 ',header_arraydata'+si+':header_arraydata'+si+ $
                                 ',header_monitor'+si+':header_monitor'+si
     endfor
     make_structure_command = make_structure_command+'}'
     
  endif else begin
     ;; if the given filename is actually a dirname assume that it is
     ;; an exploded mbfits directory
     dirname = filename
     ;; the data organised as follows
     ;;APEX-44187-2009-07-24-E-083.B-0995A-2009/ARTEMIS450-ARTBE-FEBEPAR.fits
     ;;APEX-44187-2009-07-24-E-083.B-0995A-2009/GROUPING.fits
     ;;APEX-44187-2009-07-24-E-083.B-0995A-2009/SCAN.fits
     ;;APEX-44187-2009-07-24-E-083.B-0995A-2009/1/ARTEMIS450-ARTBE-ARRAYDATA-1.fits
     ;;APEX-44187-2009-07-24-E-083.B-0995A-2009/1/ARTEMIS450-ARTBE-DATAPAR.fits
     ;;APEX-44187-2009-07-24-E-083.B-0995A-2009/1/MONITOR.fits

     foo = mrdfits(dirname+'/GROUPING.fits',0,header)
     scan = mrdfits(dirname+'/SCAN.fits',1,header_scan)
     febepar = mrdfits(dirname+'/ARTEMIS450-ARTBE-FEBEPAR.fits',1,header_febepar)
     
     nsubs_max = sxpar(header_scan,'nsubs')
     
     if n_elements(nsubs) eq 0 then begin
        nsubs = nsubs_max
     endif else begin
        nsubs = (nsubs<nsubs_max)
     endelse
     
     make_structure_command = 'struc={header:header,scan:scan,header_scan:header_scan,febepar:febepar,header_febepar:header_febepar,nsubs:nsubs'
     
     for i =1,nsubs do begin
        si = n2s(i)
        foo = execute('datapar'  +si+'=mrdfits("'+dirname+'/'+si+'/ARTEMIS450-ARTBE-DATAPAR.fits"'+',1,header_datapar'+si+')')
        foo = execute('arraydata'+si+'=mrdfits("'+dirname+'/'+si+'/ARTEMIS450-ARTBE-ARRAYDATA-1.fits"'+',1,header_arraydata'+si+')')
        foo = execute('monitor'  +si+'=mrdfits("'+dirname+'/'+si+'/MONITOR.fits"'+',1,header_monitor'+si+')')
        make_structure_command = make_structure_command + $
                                 ',datapar'+si+':datapar'+si+ $
                                 ',arraydata'+si+':arraydata'+si+ $
                                 ',monitor'+si+':monitor'+si+$
                                 ',header_datapar'+si+':header_datapar'+si+ $
                                 ',header_arraydata'+si+':header_arraydata'+si+ $
                                 ',header_monitor'+si+':header_monitor'+si
     endfor
     make_structure_command = make_structure_command+'}'
  endelse

  foo = execute(make_structure_command)

  return,struc
end
