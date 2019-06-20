;+
; NAME:
; writefits_apex
;
; PURPOSE:
; write a fits file from an apex reduced map with a proper header
;
; CATEGORY:
; FILEIO
;
; CALLING SEQUENCE:
; writefits_apex, filename, data, header=header
;
; INPUTS:
; filename: name of file to be written
; data: structure containing image and header info to be written
;
; OPTIONAL INPUTS:
; header=header: header to use as the basis
; additional parameters are passed to writefits from astrolib
;
; SIDE EFFECTS:
; a fits file is created
;
; EXAMPLE:
; writefits_apex,'test.fits',scan4100
;
; MODIFICATION HISTORY:
; (SH Nov  5 2009) Initial version
;-

;; relevant header info
;  NAME            STRING    'APEX-41496'
;  PROJECT_NAME    STRING    'C-083.F-0184A-2009'
;  DATE            STRING    '2009-07-11'
;  IMAGE           FLOAT     Array[89, 85]
;  CRPIX1          DOUBLE           45.573210
;  CRPIX2          DOUBLE           45.274216
;  ALPHA_REF       DOUBLE           151.63875
;  DELTA_REF       DOUBLE          -29.935278
;  CDELT1          FLOAT      -0.000574074
;  CDELT2          FLOAT       0.000574074
;  SOURCE          STRING    'NGC_3125'
;  UNIT            STRING    'Jy/beam ext. corr. and flat fielded'
;;

pro writefits_apex,filename,data,header=header,help=help,_extra=_extra

;; check input
need_help =0
if keyword_set(help) then need_help=1
if n_params() ne 2 then need_help=1
if size(filename,/tname) ne 'STRING' then need_help=1
if size(data,/tname) ne 'STRUCT' then need_help=1
if need_help eq 1 then begin
   doc_library,'writefits_apex'
   return
endif

default,header,['']

;; retrieve the names in the structure
data_tag_names = strupcase(tag_names(data))

if where(data_tag_names eq 'IMAGE') eq -1 then begin
   message,/error,'the supplied data structure does not contain image data'
endif

;; now we do a stupid trick. We write a file with this basic header
writefits,filename,data.image,header,_extra=_extra

;; read it again
foofoo = readfits(filename,new_header)

;; and modify this header with the info from the data
if where(data_tag_names eq 'UNIT'     ) ne -1 then sxaddpar,new_header,'BUNIT'   , data.unit
if where(data_tag_names eq 'CDELT1'   ) ne -1 then sxaddpar,new_header,'CDELT1'  , data.cdelt1
if where(data_tag_names eq 'CDELT2'   ) ne -1 then sxaddpar,new_header,'CDELT2'  , data.cdelt2
if where(data_tag_names eq 'CRPIX1'   ) ne -1 then sxaddpar,new_header,'CRPIX1'  , data.crpix1+1.
if where(data_tag_names eq 'CRPIX2'   ) ne -1 then sxaddpar,new_header,'CRPIX2'  , data.crpix2+1.
if where(data_tag_names eq 'SOURCE'   ) ne -1 then sxaddpar,new_header,'OBJECT'  , data.source
if where(data_tag_names eq 'ALPHA_REF') ne -1 then sxaddpar,new_header,'CRVAL1'  , data.alpha_ref
if where(data_tag_names eq 'DELTA_REF') ne -1 then sxaddpar,new_header,'CRVAL2'  , data.delta_ref
if where(data_tag_names eq 'ALPHA_REF') ne -1 then sxaddpar,new_header,'RA'      , data.alpha_ref, 'Right Ascension'
if where(data_tag_names eq 'DELTA_REF') ne -1 then sxaddpar,new_header,'DEC'     , data.delta_ref, 'Declination'
if where(data_tag_names eq 'DATE'     ) ne -1 then sxaddpar,new_header,'DATE-OBS', data.date      
if where(data_tag_names eq 'NAME'     ) ne -1 then sxaddpar,new_header,'ORIGIN' , 'P-ArTeMiS Pipeline', data.name

;; and some standard values that we need
sxaddpar,new_header,'CROTA1' , 0.0
sxaddpar,new_header,'CTYPE1' , 'RA---GLS'
sxaddpar,new_header,'CTYPE2' , 'DEC--GLS'
sxaddpar,new_header,'EPOCH'  , 2000.0
sxaddpar,new_header,'EQUINOX', 2000.0

;; and write the final version

writefits,filename,data.image,new_header,_extra=_extra

end
