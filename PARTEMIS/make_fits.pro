pro make_fits, map_radec_str, fileout=fileout, dir=dir, xy=xy

COMMON obs1_configb, work_dir, project_name, calibration_tables

if not keyword_set(dir) then dir = work_dir + 'apex_may09/'
toto=findfile(dir + '*.fits', count=nfits)

if not keyword_set(fileout) then fileout = strlowcase(map_radec_str.source+'_'+map_radec_str.name)

if (strmid(fileout,4,5,/reverse_offset) ne '.fits') then begin
	fileout = fileout +'.fits'
endif

toto=findfile(dir+fileout, count=nfile)

;stop

image = map_radec_str.image
weight = map_radec_str.weight

taille = size(image)

cube = fltarr(taille(1),taille(2),2)

cube(*,*,0) = image
cube(*,*,1) = weight 

;
;fxhmake, header_fits, map_radec_str.image
fxhmake, header_fits, cube

;writefits, dir+fileout, map_radec_str.image,  header_fits
writefits, dir+fileout, cube,  header_fits

if not keyword_set(xy) then begin

	fxhmodify, dir+fileout, 'BUNIT', map_radec_str.unit     ;   'Jy/11''-beam'
	fxhmodify, dir+fileout, 'CTYPE1', 'RA---GLS    '
	fxhmodify, dir+fileout, 'CTYPE2', 'DEC--GLS    '
	fxhmodify, dir+fileout, 'CDELT1', map_radec_str.cdelt1
	fxhmodify, dir+fileout, 'CDELT2', map_radec_str.cdelt2
	fxhmodify, dir+fileout, 'CRPIX1', map_radec_str.crpix1+1.
	fxhmodify, dir+fileout, 'CRPIX2', map_radec_str.crpix2+1.
	fxhmodify, dir+fileout, 'OBJECT', map_radec_str.source
	fxhmodify, dir+fileout, 'CROTA1', 0.0
	fxhmodify, dir+fileout, 'CRVAL1', map_radec_str.alpha_ref
	fxhmodify, dir+fileout, 'CRVAL2', map_radec_str.delta_ref
	fxhmodify, dir+fileout, 'RA', map_radec_str.alpha_ref, 'Right Ascension'
	fxhmodify, dir+fileout, 'DEC', map_radec_str.delta_ref, 'Declination'
	fxhmodify, dir+fileout, 'EPOCH', 2000.0
	fxhmodify, dir+fileout, 'EQUINOX', 2000.0
	fxhmodify, dir+fileout, 'DATE-OBS', map_radec_str.date
	fxhmodify, dir+fileout, 'ORIGIN', 'P-ArTeMiS Pipeline', map_radec_str.name

endif else begin	

	elev = map_radec_str.elevatio		; elevation in deg
	delta_az = map_radec_str.daz_scan	; Az, El offsets of (crpix1,crpix2) in arcsec
	delta_el = map_radec_str.del_scan
	rot_dazdel2dxdy, delta_az, delta_el, elev, delta_x, delta_y	; delta_x, delta_y = x,y offsets of (crpix1,crpix2) in arcsec

;	fxhmodify, dir+fileout, 'CTYPE1', 'X--COOR    '
;	fxhmodify, dir+fileout, 'CTYPE2', 'Y--COOR    '
;	fxhmodify, dir+fileout, 'CDELT1', -map_radec_str.cdelt1*!pi/180.
;	fxhmodify, dir+fileout, 'CDELT2', map_radec_str.cdelt2*!pi/180.
	fxhmodify, dir+fileout, 'CTYPE1', 'RA---GLS    '
	fxhmodify, dir+fileout, 'CTYPE2', 'DEC--GLS    '	
	fxhmodify, dir+fileout, 'CDELT1', -map_radec_str.cdelt1
	fxhmodify, dir+fileout, 'CDELT2', map_radec_str.cdelt2
	fxhmodify, dir+fileout, 'CRPIX1', map_radec_str.crpix1+1.
	fxhmodify, dir+fileout, 'CRPIX2', map_radec_str.crpix2+1.
	fxhmodify, dir+fileout, 'OBJECT', map_radec_str.source
	fxhmodify, dir+fileout, 'CROTA1', map_radec_str.crota1
	fxhmodify, dir+fileout, 'CRVAL1', delta_x/3600.			;  crval1, crval2 = x,y offsets of (crpix1,crpix2) in deg
	fxhmodify, dir+fileout, 'CRVAL2', delta_y/3600.
	fxhmodify, dir+fileout, 'RA', map_radec_str.alpha_ref, 'Right Ascension'
	fxhmodify, dir+fileout, 'DEC', map_radec_str.delta_ref, 'Declination'
	fxhmodify, dir+fileout, 'EPOCH', 2000.0
	fxhmodify, dir+fileout, 'EQUINOX', 2000.0
	fxhmodify, dir+fileout, 'ORIGIN', 'P-ArTeMiS Pipeline', map_radec_str.filename

endelse
;

;stop

return
end
