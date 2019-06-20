pro gildas, name, data, ax1, ax2, ax3, ax4, header=h, write=w, show=sh
;+
; NAME:
;    GILDAS
;
; PURPOSE:
;    Read and Write interface for the GILDAS (.gdf) file format supported
;    by the IRAM program packages (GILDAS, GRAPHIC, GREG, VECTOR, 
;    TASK, GREMLIN ...)
;
; CALLING SEQUENCE:
;    gildas,filename,variable,[axis1,axis2,axis3,axis4]
;
; KEYWORDS:
;    header=h   Input or output of the header. If the procedure is called
;               without arguments but with the header keyword, the returned
;               parameter will be a standard header structure.
;    /write     If set, the given data array will be written on disk, if not,
;               a file with the given name will be searched and red.
;    /show      Display the header.
; 
; INPUT/OUTPUT:
;    filename   Character string with the input/output filename.
;    variable   input/output data array.
;
; OPTIONAL INPUT/OUTPUT:
;    axis1...axis4    In the input mode, axis element vectors convenient 
;                     for the use in plotting routines will be created.
;                     The output mode will take the axes and use them in
;                     the CONVERT header section.
;
; RESTRICTIONS:
;    Only UNIX and VMS are currently supported, without automatic conversion.
;
; REVISION HISTORY:
;    29-Mar-1994 Creation by M. Bremer, IRAM
;    10-Feb-1995 Revised version, M.Bremer, IRAM
;-
;
; Variables that tell if a certain section is filled with useful data:
; blan, extrema, desc, posi, proj, spec.
; If data is present, those variables give the lenght of the section in bytes.
; If not, they are zero.
;
; mode=0 : read, mode=1 : write
mode = keyword_set(w)
;
; IF THE FOLLOWING  SETUP DOES NOT WORK ON YOUR MACHINE, LOOK UP THE FORMAT
; CODES IN THE FILE "format.inc" OF YOUR LOCAL GILDAS / GRAPHIC / GREG VERSION
; 
; Find out the operating system type (Unix on HP,DEC,IBM or DEC VMS):
;
chos = strlowcase(!version.os)
if (strpos(chos,'hp' ) ge 0) or   $
   (strpos(chos,'u'  ) ge 0) or   $ 
   (strpos(chos,'ibm') ge 0) then begin
  my_sys = '.'
  fmt_by = -6
  fmt_i4 = -3
  fmt_r4 = -21
  fmt_r8 = -22
endif else begin
  if strpos(chos,'vms') ge 0 then begin
    my_sys = '_' 
    fmt_by = -6
    fmt_i4 = -13
    fmt_r4 = -1
    fmt_r8 = -2
    endif else begin
;
; This might go wrong, see uppercase letter comment above if it fails.
;
      print,'Warning: Operating system not recognized, using bold guess.'
      my_sys = '.'
      fmt_by = -6
      fmt_i4 = -3
      fmt_r4 = -21
      fmt_r8 = -22
  endelse
endelse
;
if (mode eq 1) and (n_elements(h) ne 0) then h_sav = h
c12 = '            '
u12 = 'UNKNOWN     '
;
; 43+20+96+12+44+72+72+32+20+4+36+40+20+8
; 63   +108  +116  +104  +60     +68     = 519
h={header, type:'GILDAS'+my_sys+'IMAGE', form:0L, nvb:0L,  $
   fill:lonarr(5), gene:116L, ndim:1L, dim:lonarr(4)+1, $
   convert:dblarr(3,4), $
   blan:8L, blank:[0.0, -1.0], $
   extrema:0L, min:0.0, max:0.0, where:lonarr(2,4), $
   desc:72L,  $
   unit:u12, unit1:u12, unit2:u12, unit3:u12, unit4:u12, coord:u12, $
   posi:0L, source:c12, ra:0.D+00, dec:0.D+00,  $
   lii: 0.D+00, bii: 0.D+00, epoch: 0.0, $
   proj:0L, $
   ptyp:0L, a0:0.D+00, d0:0.D+00, pang:0.D+00, xaxi:0L, yaxi:0L, $
   spec:0L, line:c12, freqres:0.D+00, freqoff:0.D+00, restfre:0.D+00, $
   velres:0.0, veloff:0.0, faxis:0L, $
   reso:0L, major:0.0, minor:0.0, posa:0.0}
;
n = n_params(0)
if n lt 2 then begin
  print,'Calling sequence: gildas, filename, variable, [ax1, ax2, ax3, ax4]'
  print,'        Keywords: header, show, write'
  return
endif 
;
; Check filename ( if it has an extension, if it exists, if it is unique: )
;
name2 = name
if strpos(name2,'.') lt 0 then name2 = name2 + '.gdf'
;
if mode eq 0 then begin
;
list = findfile(name2)
sz = size(list)
if sz(0) eq 0 then begin
  print,'Error: File ' + name2 + ' not found'
  return
endif else begin
  if sz(1) gt 1 then begin
    print,'Error: Filename not unique. The following matches have been found:'
    for i = 0,sz(1)-1 do print,list(i)
    return
  endif
endelse
;
;
; Check file format (UNIX and VMS gildas supported):
;
type = '            '
openr,unit,name2,/get_lun
readu,unit,type
;
if strmid(type,6,1) ne my_sys then begin
  print,'Error: The given file is not in the format of the current operating'
  print,'       system. Please convert it first.'
  close,unit
  free_lun,unit
  return
endif
;
; rewind file and read the header:
;
point_lun,unit,0
readu,unit,h
;
;
text=''
for i=0L, h.ndim-1 do text = text + string(h.dim(i))+','
text = strcompress(text, /remove_all)
text = strmid(text, 0, strlen(text)-1)
;

case h.form of
 fmt_i4 : begin
            ok = execute( 'data = lonarr (' + text + ')' )
            ctype='INTEGER*4'
          end
 fmt_r4 : begin
            ok = execute( 'data = fltarr (' + text + ')' )
            ctype='REAL*4'
          end
 fmt_r8 : begin
            ok = execute( 'data = dblarr (' + text + ')' )
            ctype='REAL*8'
          end
 fmt_by : begin
            ok = execute( 'data = bytarr (' + text + ')' )
            ctype='BYTE'
          end
else    :  begin
  print,'Error: Data format code ', h.form, ' not recognized.'
  return
  end
endcase
;
; point on begin of data section and read the data:
;
point_lun,unit,512
readu,unit,data
;
; Prepare the axes designations, if possible:
;
c = h.convert
;
  ax1 = (findgen(h.dim(0)) - c(0,0) + 1) * c(2,0) + c(1,0) 
if h.ndim ge 2 then $
  ax2 = (findgen(h.dim(1)) - c(0,1) + 1) * c(2,1) + c(1,1) 
if h.ndim ge 3 then $
  ax3 = (findgen(h.dim(2)) - c(0,2) + 1) * c(2,2) + c(1,2) 
if h.ndim eq 4 then $
  ax4 = (findgen(h.dim(3)) - c(0,3) + 1) * c(2,3) + c(1,3) 
; 
; close the file and free the unit:
;
close,unit
free_lun,unit
;
endif else begin
;
; write an image in GILDAS format:
;
; check consistency of header, i.e. if it is a structure:
sz = size(h_sav)
if (sz(0) eq 1) and (sz(2) eq 8) then begin
;
; The specified header is a structure indeed. Ask some questions to see
; if it is a useful structure:
ok = 1b
ok = ok and execute('tmp = h_sav.type')
ok = ok and execute('tmp = h_sav.form')
ok = ok and execute('tmp = h_sav.ndim')
ok = ok and execute('tmp = h_sav.dim')
if ok ne 1b then begin
 print,'Warning: Header structure not defined by this program.'
 choice = ' '
 read,'Continue (y/n) [n] : ',choice
 if strlowcase(choice) ne 'y' then return
end
;
sz  = size(data)
;
endif else begin
;
; The input shows no header structure. Fill the default header
; with basic information.
;
  h_sav = h
endelse
;
; Write information for the data into the header but don't
; touch elements where it is not neccessary (for the case that the user 
; has provided  a structure)
;
h_sav.type = 'GILDAS.IMAGE'
sz  = size(data)
nsz = n_elements(sz)
bytelem = 1
case sz(nsz-2) of
 0 : begin
	print,'Error: first argument (data array) not defined.'
	return
     end
 1 : begin
        h_sav.form = fmt_by
        ctype='BYTE'
     end
 2 : begin
	print,'Converting data from INTEGER*2 to REAL*4.'
	data = float(data)
	h_sav.form = fmt_r4
	bytelem = 4
        ctype='REAL*4'
     end
 3 : begin
	print,'Converting data from INTEGER*4 to REAL*4.'
	data = float(data)
	h_sav.form = fmt_r4
	bytelem = 4
        ctype='REAL*4'
     end
 4 : begin
	h_sav.form = fmt_r4
	bytelem = 4
     end
 5 : begin
	h_sav.form = fmt_r8
	bytelem = 8
        ctype='REAL*8'
     end
 6 : begin
	print,'Converting data from COMPLEX to REAL*4 (absolute value).'
	data = abs(data)
	h_sav.form = fmt_r4
	bytelem = 4
        ctype='REAL*4'
     end
else : begin
	print,'Error: Automatic conversion of data not possible.'
	return
       end
endcase
;
if nsz gt 3 then begin
  h_sav.ndim = sz(0)
  h_sav.dim(0:sz(0)-1) = sz(1:sz(0))
endif
;
; header element Convert: First the basics:
;
h_sav.convert(*,0:h_sav.ndim-1) = 1.0D+00
;
; if commandline-parameters are given:
;
if n_elements(ax1) ge 2 then begin
  h_sav.convert(0,0) = 1
  h_sav.convert(1,0) = ax1(0)
  h_sav.convert(2,0) = ax1(1) - ax1(0)
endif
if (n_elements(ax2) ge 2) and (h_sav.ndim ge 2) then begin
  h_sav.convert(0,1) = 1
  h_sav.convert(1,1) = ax2(0)
  h_sav.convert(2,1) = ax2(1) - ax2(0)
endif
if (n_elements(ax3) ge 2) and (h_sav.ndim ge 3) then begin
  h_sav.convert(0,2) = 1
  h_sav.convert(1,2) = ax3(0)
  h_sav.convert(2,2) = ax3(1) - ax3(0)
endif
if (n_elements(ax4) ge 2) and (h_sav.ndim ge 4) then begin
  h_sav.convert(0,3) = 1
  h_sav.convert(1,3) = ax4(0)
  h_sav.convert(2,3) = ax4(1) - ax4(0)
endif
;
; Force 12C size of character variables:
txt = [h_sav.unit,  h_sav.unit1,  h_sav.unit2,  h_sav.unit3, $
       h_sav.unit4, h_sav.coord,  h_sav.source, h_sav.line] 
txt = strmid(txt + c12, 0, 12)
h_sav.unit  = txt(0) 
h_sav.unit1 = txt(1) 
h_sav.unit2 = txt(2)  
h_sav.unit3 = txt(3) 
h_sav.unit4 = txt(4) 
h_sav.coord = txt(5) 
h_sav.source= txt(6) 
h_sav.line  = txt(7) 
;
; Number of virtual blocks:
exact = sz(nsz-1)*bytelem/512.D+00
h_sav.nvb = long(exact) + 1
tmp = (exact+1)/16.D+00
if tmp-long(tmp) gt 0.001 then  h_sav.nvb = long(tmp+1) * 16 - 1
;
; Empirical term +60 has been added for HP.
;
; nfill = long ((h_sav.nvb - exact) * 512  ) + 60
nfill = long ((h_sav.nvb - exact) * 512  )
if nfill gt 0 then fill_up = bytarr( nfill )
;
; print,n_elements(fill_up),n_elements(data)*4, $
;       n_elements(fill_up)+n_elements(data)*4+512
;
; Check the various data sections of the header to see if it is neccessary to 
; activate them. Activation is done by assigning the length of the 
; corresponding section in bytes (blan, extrema, desc, posi, proj, spec, reso).
; blan (=8) and desc (=72) are activated by default.
; 
h_sav.extrema = 0
h_sav.posi    = 0
h_sav.proj    = 0
h_sav.spec    = 0
h_sav.reso    = 0

;
i = where([h_sav.min, h_sav.max, h_sav.where(0:7)] ne 0)
if i(0) ne -1 then h_sav.extrema = 12
;
i = where([h_sav.ra, h_sav.dec, h_sav.lii, h_sav.bii, h_sav.epoch] ne 0)
if (h_sav.source ne c12) or (i(0) ne -1) then h_sav.posi = 48
;
i = where([h_sav.ptyp, h_sav.a0, h_sav.d0, $
          h_sav.pang, h_sav.xaxi, h_sav.yaxi] ne 0)
if i(0) ne -1 then h_sav.proj = 36
;
i = where([h_sav.freqres, h_sav.freqoff, h_sav.restfre, $
   h_sav.velres, h_sav.veloff, h_sav.faxis] ne 0)
if (h_sav.line ne c12) or (i(0) ne -1) then h_sav.spec = 48
;
i = where([h_sav.major, h_sav.minor, h_sav.posa] ne 0)
if i(0) ne -1 then h_sav.reso = 12
;
;
;; Everything went fine? Create a discfile, then.
;
openw,unit,name2,/get_lun
writeu,unit,h_sav
;
; Make sure that the data starts at byte 512 indeed:
writeu,unit,bytarr(64)
point_lun,unit,512
;
writeu,unit,data
if nfill gt 0 then writeu,unit,fill_up
close,unit
free_lun,unit
;
h = h_sav
;
endelse
;
;
; Print header data in the same format as graphic, if keyword /show is set:
;
if keyword_set(sh) eq 1 then begin
;
print,'File : ',name2,'          ',ctype
print,'         Size    Reference Pixel     Value         Increment'
for i=0,3 do print,h.dim(i),h.convert(*,i)
; blan, extrema, desc, posi, proj, spec.
print,'Blanking value and tolerance ',h.blank 
;
print,'Source name         ',h.source
print,'Map unit            ',h.unit
print,'Axis type           ',h.unit1,' ',h.unit2,' ',h.unit3,' ',h.unit4,' '
print,'Coordinate system   ',h.coord
ra1 = h.ra*12/!dpi
ra2 = (ra1-fix(ra1))*60
ra3 = (ra2-fix(ra2))*60
ra4 = (ra3-fix(ra3))*1000
de1 = h.dec*180/!dpi
de2 = (de1-fix(de1))*60
de3 = (de2-fix(de2))*60
de4 = (de3-fix(de3))*1000
txt_ra = string(ra1,ra2,ra3,ra4,format='(i4,":",i2.2,":",i2.2,".",i3.3)')
txt_de = string(de1,de2,de3,de4,format='(i4,":",i2.2,":",i2.2,".",i3.3)')
print,'Right Ascension   ' + txt_ra + '        Declination       ' + txt_de
print,'Lii ',h.lii*!radeg,'        Bii ',h.bii*!radeg
print,'Epoch ',float(h.epoch,0,1)
;Projection type                         Angle
;Axis       A0                           Axis       D0
print,'Minimum ',h.min, '   at ',transpose(h.where(0,*))
print,'Maximum ',h.max, '   at ',transpose(h.where(1,*))
print,'Axis ',h.faxis, ' Line Name ',h.line, ' Rest Frequency ', h.restfre
print,'Resolution in Velocity ', h.velres, '  in Frequency    ', h.freqres
print,'Offset in Velocity     ', h.veloff, '  Image Frequency ', h.freqoff
;
endif
;
end


