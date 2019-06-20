;+
; NAME: extractsubfits
;
; PURPOSE: extract a sub image from a fits image
;
; CATEGORY: Astro/FitsFiles
;
; CALLING SEQUENCE: extractsubfits,fitsfilename,center,size
;
; INPUTS:
; fitsfilenames: string (array) of fitsfiles to be treated
; center: position around which to extract (string for simbad,
;        [string,string] in sexagimal or [double,double] in degrees
; size: approximate size (sides of map) of region to extract (default 60 arcsec)
;
; OPTIONAL INPUTS:
; center=center position around which to extract (string for simbad,
;               [string,string] in sexagimal or [double,double] in degrees
; size=approximate size (sides of map in arcsec) of region to extract (default 60 arcsec)
; suffix=suffix suffix to attach to the input filename to construct
;               the output filename (default: '')
; prefix=prefix prefix to prepend to the input filename to construct
;               the output filename (default: 'sub_')
; outfilename=outfilename name of output file to  be written
;                 (default PREFIX+fitsfilename)
;
; KEYWORD PARAMETERS:
; /help: if set show this help
; /overwrite: if set overwrite the output file without further questions
; /nooverwrite: if set do not overwrite the output file without further questions
;
; SIDE EFFECTS:
; a new fitsfile is created or an existing file is overwritten
;
; RESTRICTIONS:
; needs working fitscopy command from cfitsio package
; needs astrolib routines
;
; EXAMPLE:
; makesubimage,'irac_c1.fits',[74.197609,-66.409515],300.,out='irac_c1_n11.fits'
;
; MODIFICATION HISTORY:
;(SH Jan 31 2011) initial version
;-

pro extractsubfits,fitsfilenames,cntr,sz, $
                   center=center, $
                   size=size, $
                   suffix=suffix, $
                   prefix=prefix, $
                   outfilename=outfilename, $
                   outdir=outdir, $
                   overwrite=overwrite, $
                   nooverwrite=nooverwrite, $
                   help=help, $
                   extensions=extensions, $
                   recenter=recenter, $
                   _extra=_extra
  
  ;; catch help requests before anything else
  if (keyword_set(help)) or (n_params() eq 0) then begin
     doc_library,'extractsubfits'
     return
  endif

  ;; default values
  if n_elements(prefix) eq 0 then prefix='sub_' else prefix = string(prefix[0])
  if n_elements(suffix) eq 0 then suffix=''     else suffix = string(suffix[0])

  ;; which center position to use?
  case 1 of  
     n_params() ge 2: begin
        center_position = cntr
     end
     n_elements(center) ne 0: begin
        center_position = center
     end
     else: begin
        message,/info,'FAILURE: no center position given'
        doc_library,'extractsubfits'
        return
     end
  endcase

  ;; allow for
  ;; string source name for simbad
  ;; [string,string] sex format (J2000)
  ;; [double,double] degree (J2000)
  case 1 of
     n_elements(center_position) eq 1: begin
        querysimbad, center_position, ra_requested, de_requested, id, Found=found, /SILENT
        if found ne 1 then begin
           message,/info,'FAILURE: source not found in SIMBAD database'
           return
        endif
     end
     (n_elements(center_position) eq 2) and (size(center_position,/tname) eq 'STRING'): begin
        
        rasubstrings  = stregex(center_position[0], '([^0-9]*)([0-9]*)[^0-9]*([0-9]*)[^0-9]*([0-9.,]*)[^0-9.,]*', /EXTRACT , /SUBEXPR) 
        decsubstrings = stregex(center_position[1], '([^0-9]*)([0-9]*)[^0-9]*([0-9]*)[^0-9]*([0-9.,]*)[^0-9.,]*', /EXTRACT , /SUBEXPR) 
        
        rasign = ([1d0,-1d0])[rasubstrings[1] eq '-']
        rah =   double(rasubstrings[2])
        ram =   double(rasubstrings[3])
        ras =   double(rasubstrings[4])
        
        design = ([1d0,-1d0])[decsubstrings[1] eq '-']
        ded =   double(decsubstrings[2])
        dem =   double(decsubstrings[3])
        des =   double(decsubstrings[4])
           
        ra_requested = rasign*(rah+ram/60d0+ras/3600d0)*15d0
        de_requested = design*(ded+dem/60d0+des/3600d0)
        
     end
     (n_elements(center_position) eq 2) and (size(center_position,/tname) ne 'STRING'): begin
        ra_requested = double(center_position[0])
        de_requested = double(center_position[1])
     end
     else: begin
        message,/info,'FAILURE: dont know how to convert '+center_position+' to a wcs position'
        return
     end
  endcase

  ;; which size to use
  case 1 of  
     n_params() eq 3: begin
        size_requested = sz
     end
     n_elements(size) ne 0: begin
        size_requested = size
     end
     else: begin
        message,/info,'WARNING: no requested size given, using default of 60"'
        size_requested = 60d0
     end
  endcase
  size_requested_degree = size_requested/3600d0
  
  ;; calculate the extremities of the requested box in pixel coordinates
  ;; extremes in de
  min_de_requested = (de_requested-size_requested_degree/2d0) > (-90d0)
  max_de_requested = (de_requested+size_requested_degree/2d0) < ( 90d0)
  
  ;; corresponding extremeties at given ra extremes
  min_ra_at_min_de_requested=ra_requested-size_requested_degree/2d0/cos(min_de_requested*!dpi/180d0)
  max_ra_at_min_de_requested=ra_requested+size_requested_degree/2d0/cos(min_de_requested*!dpi/180d0)
  min_ra_at_max_de_requested=ra_requested-size_requested_degree/2d0/cos(max_de_requested*!dpi/180d0)
  max_ra_at_max_de_requested=ra_requested+size_requested_degree/2d0/cos(max_de_requested*!dpi/180d0)

  ;; allow to read a different extension number than default (0)
  case n_elements(extensions) of 
     1: begin
        extnums = make_array(n_elements(fitsfilenames),value=extensions[0])
     end
     n_elements(fitsfilenames): begin
        extnums = extensions
     end
     else: begin
        extnums = make_array(n_elements(fitsfilenames),value=0)
     end
  endcase

  ;; loop over fitsfilenames
  for i =0,n_elements(fitsfilenames)-1 do begin
     fitsfilename = fitsfilenames[i]
     extnum = extnums[i]
     hasuncompressed = 0

     ;; check input fitsfilename
     result = file_search(fitsfilename)
     if result[0] eq '' then begin
        ;; try gz compressed version
        result = file_search(fitsfilename+'.gz')
        if result[0] ne '' then begin
           uncompressedfile = fitsfilename
           spawn,'gunzip -d '+fitsfilename+'.gz -c >'+uncompressedfile
           hasuncompressed=1
        endif else begin
           message,/info,'FAILURE: the given file: '+fitsfilename+' does not exist'
           continue
        endelse
     endif

     ;; construct outfilename 
     if n_elements(outfilename) eq 1 then begin
        outname = string(outfilename)
     endif else begin
        indir = file_dirname(fitsfilename)
        infullfile = file_basename(fitsfilename)
        infileparts = stregex(infullfile,'(.*)\.([^.]*)',/extract,/subexp)
        infilename = infileparts[1]
        infileext = infileparts[2]

        if n_elements(outdir) eq 1 then begin
           outdirectory = outdir+'/'
        endif else begin
           outdirectory = indir+'/'
        endelse

        outname = outdirectory+prefix+infilename+suffix+'.'+infileext
     endelse
     
     ;; read the header
     header = headfits(fitsfilename,exten=extnum,_extra=_extra)

     ;; number of row and columns in the image
     nx = sxpar(header,'naxis1')
     ny = sxpar(header,'naxis2')
     
     ;; the pixel coordinates of the requested corners
     adxy,header,min_ra_at_min_de_requested,min_de_requested,x0,y0
     adxy,header,max_ra_at_min_de_requested,min_de_requested,x1,y1
     adxy,header,min_ra_at_max_de_requested,max_de_requested,x2,y2
     adxy,header,max_ra_at_max_de_requested,max_de_requested,x3,y3
     
     ;; take the extremeties, respecting the maximum pixel values in the image
     ;; note that wcstools starts pixels at 1
     min_x = ((round(min([x0,x1,x2,x3]+1L)) > 1L ) < (nx))
     max_x = ((round(max([x0,x1,x2,x3]+1L)) > 1L ) < (nx))
     min_y = ((round(min([y0,y1,y2,y3]+1L)) > 1L ) < (ny))
     max_y = ((round(max([y0,y1,y2,y3]+1L)) > 1L ) < (ny))

     ;; check that the box is valid
     if (min_x eq max_x) or (min_y eq max_y) then begin
        message,/info,'FAILURE: the requested area does not overlap with the input image'
        continue
     endif

     ;; check to not overwrite an existing file
     if (file_search(outname))[0] ne '' then begin
        if keyword_set(nooverwrite) then begin
           message,/info,'INFO: not overwriting existing file'
           continue
        endif
        if keyword_set(overwrite) then begin
           spawn,'rm -f '+outname
        endif else begin
           print,'output file, '+outname+' already exists'
           answer=''
           read,'would you like to overwrite it? (y/n)? ',answer
           if stregex(answer,'^[yY].*',/boolean) then begin
              spawn,'rm -f '+outname
           endif else begin
              message,/info,'INFO: not overwriting existing file'
              continue
           endelse
        endelse
     endif

     ;; make sure the output directory exists
     file_mkdir,outdirectory

     ;; use fitscopy from cfitsio to do the actual work
     ;; getpix from wcstools might also do. Others?
     range = strcompress(string(format='("[",I,":",I,",",I,":",I,"]")',min_x,max_x,min_y,max_y),/remove_all)
     message,/info,'INFO: executing: fitscopy "'+fitsfilename+range+'" "'+outname+'"'
     spawn,'fitscopy "'+fitsfilename+range+'" "'+outname+'"',exit_status=exit_status

     ;; in case the command failed try to make a subfits the IDL way
     if exit_status ne 0 then begin
        message,/info,'WARNING: fitscopy command failed'
        message,/info,'WARNING: DOING things the slow way'
        image = readfits(fitsfilename,header,_extra=_extra)
        ;; remember the offsets from IDL !!
        hextract,image,header,min_x-1L,max_x-1L,min_y-1L,max_y-1L
        writefits,outname,float(image),header
     endif

     if hasuncompressed then begin
        file_delete,uncompressedfile
     endif
     
;     if keyword_set(recenter) then begin
;        outheader = headfits(outname)
;        
;        adxy,outheader,ra_requested,de_requested,x0,y0
;        print, $
;           'sethead'+ $
;           ' CRPIX1='+strcompress(string(x0),/remove_all)+ $
;           ' CRPIX2='+strcompress(string(y0),/remove_all)+ $
;           ' CRVAL1='+strcompress(string(ra_requested),/remove_all)+ $
;           ' CRVAL2='+strcompress(string(de_requested),/remove_all)+ $
;           ' "'+outname+'"'
;        spawn, $
;           'sethead'+ $
;           ' CRPIX1='+strcompress(string(x0),/remove_all)+ $
;           ' CRPIX2='+strcompress(string(y0),/remove_all)+ $
;           ' CRVAL1='+strcompress(string(ra_requested),/remove_all)+ $
;           ' CRVAL2='+strcompress(string(de_requested),/remove_all)+ $
;           ' "'+outname+'"'
;     endif

  endfor
end
