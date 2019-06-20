; List of procedures included in this file

; extract_substring
; extract_calibratelog
; extract_
; extract_maplog
; extract_onlog
; extract_skydiplog
; extract_champ
; get_champ_string
; build_apexobslog

;===============================================================================
function extract_substring, value, param1, param2, off1, off2

pos1 = strpos(value, param1)
pos2 = strpos(value, param2)
substring = strmid(value, pos1+off1, pos2-pos1-off2)

return, substring
end

;===============================================================================
pro extract_calibratelog, value, calibrate_struct

;chaine = "calibrate(mode='COLD',time=10,autolevel='ON')"

pos1 = strpos(value, 'mode=')
pos2 = strpos(value, ',time')
mode = strmid(value, pos1+6, pos2-pos1-7)

pos1 = strpos(value, 'time=')
pos2 = strpos(value, ',autolevel')
time = long(strmid(value, pos1+5, pos2-pos1-5))

pos1 = strpos(value, 'autolevel=')
pos2 = strpos(value, ')')
autolevel = strmid(value, pos1+11, pos2-pos1-12)

calibrate_struct = {mode:mode, time:time, autolevel:autolevel}

return
end

;===============================================================================
pro extract_maplog, value, map_struct

;chaine='otf(xlen=120,xstep=6,ylen=120,ystep=10,time=1.0,direction='x',zigzag=1,unit='arcsec',system='EQ',epoch=2000.0)</command>

pos1 = strpos(value, 'xlen=')
pos2 = strpos(value, ',xstep')
xlen = float(strmid(value, pos1+5, pos2-pos1-5))
;xlen = float(extract_substring('xlen=', ',xstep', 5, 5))

pos1 = strpos(value, 'xstep=')
pos2 = strpos(value, ',ylen')
xstep = float(strmid(value, pos1+6, pos2-pos1-6))

pos1 = strpos(value, 'ylen=')
pos2 = strpos(value, ',ystep')
ylen = float(strmid(value, pos1+5, pos2-pos1-5))

pos1 = strpos(value, 'ystep=')
pos2 = strpos(value, ',time')
ystep = float(strmid(value, pos1+6, pos2-pos1-6))

pos1 = strpos(value, 'time=')
pos2 = strpos(value, ',direction')
time = float(strmid(value, pos1+5, pos2-pos1-5))

pos1 = strpos(value, 'direction=')
pos2 = strpos(value, ',zigzag')
direction = strmid(value, pos1+11, pos2-pos1-12)

pos1 = strpos(value, 'zigzag=')
pos2 = strpos(value, ',unit')
zigzag = long(strmid(value, pos1+7, pos2-pos1-7))

pos1 = strpos(value, 'unit=')
pos2 = strpos(value, ',system')
unit = strmid(value, pos1+6, pos2-pos1-7)

pos1 = strpos(value, 'system=')
pos2 = strpos(value, ',epoch')
system = strmid(value, pos1+8, pos2-pos1-9)

pos1 = strpos(value, 'epoch=')
pos2 = strpos(value, ')')
epoch = float(strmid(value, pos1+6, pos2-pos1-6))

map_struct = {xlen:xlen, xtep:xstep, ylen:ylen, ystep:ystep, time:time, direction:direction, zigzag:zigzag,$
              unit:unit, system:system, epoch:epoch}

return
end

;===============================================================================
pro extract_onlog, value, on_struct

; chaine='on(time=20)'

pos1 = strpos(value, 'time=')
pos2 = strpos(value, ')')
time = long(strmid(value, pos1+5, pos2-pos1-5))

on_struct = {time:time}

return
end

;===============================================================================
pro extract_skydiplog, value, skydip_struct

;skydip(azimuth='current',am_stop=2.0,am_start=1.0005,points=5,time=15.0,mode='RAS')

pos1 = strpos(value, 'azimuth=')
pos2 = strpos(value, ',')
azimuth = strmid(value, pos1+9, pos2-pos1-9-1)

pos1 = strpos(value, 'am_stop=')
pos2 = strpos(value, ',am_start')
am_stop = float(strmid(value, pos1+8, pos2-pos1-8))

pos1 = strpos(value, 'am_start=')
pos2 = strpos(value, ',points')
am_start = float(strmid(value, pos1+9, pos2-pos1-9))

pos1 = strpos(value, 'points=')
pos2 = strpos(value, ',time')
points = fix(strmid(value, pos1+7, pos2-pos1-7))

pos1 = strpos(value, 'time=')
pos2 = strpos(value, ',mode')
time = float(strmid(value, pos1+5, pos2-pos1-5+1))

pos1 = strpos(value, 'mode=')
pos2 = strpos(value, ')')
mode = strmid(value, pos1+6, pos2-pos1-6-1)

skydip_struct = {azimuth:azimuth, am_stop:am_stop, am_start:am_start, points:points, time:time, mode:mode}

return
end

;===============================================================================
pro extract_champ, chaine, champ, value
	
pos1 = strpos(chaine, '<')
pos2 = strpos(chaine, '>')
champ = strmid(chaine, pos1+1, pos2-pos1-1)
subchaine = strmid(chaine, pos2+1, strlen(chaine))
pos1 = strpos(subchaine, '<')
if pos1 eq -1 then pos1=strlen(subchaine)
value = strmid(subchaine, 0, pos1)

;print, champ, value

return
end

;===============================================================================
pro get_champ_string, champ_string

champ_string = [ $
       'command', $
	'comment', $
	'date', $
	'febes', $
	'geom', $
	'lines', $
	'lst', $
	'mode',$
	'observerID', $
	'offsets', $
	'operatorID', $
	'projectID', $
	'reference', $
	'source', $
	'strokeMode', $
	'switchMode', $
	'type', $
	'utc']

return
end

;===============================================================================
pro init_logstructure, struct

on_str = {time : 0l}

map_str = {xlen : 0.,  $
	xstep:0.,  $
	ylen:0., $
	ystep:0., $
	time:0.0, $
	direction:'', $
	zigzag : 0l, $
	unit : '', $
	system : '', $
	epoch : 0. $
	}
	
skydip_str = { $
	azimuth : '', $
	am_stop : 0., $
	am_start : 0., $
	points : 0, $
	time : 0.0, $
	mode : '' $
	}
		
calibrate_str = { $
	mode : '', $
	time : 0l, $
	autolevel : '' $
	}
		
obslog_str = { $
        CA : 0.0, $
        IE : 0.0, $
        azimuth : 0.0, $
        command : '', $
	command_type : '', $
        comment : '', $
        date : '', $
        dewPoint : 0.0, $
        elevation : 0.0, $
        febes : '', $
        geom : '', $
        humidity : 0.0, $
        lines : '', $
        lst : '', $
        mode: '', $
        numSubScans : 0, $
        number : 0L, $
        observerID : '', $
        offsets : '', $
        operatorID : '', $
        pressure : 0.0, $
        projectID : '', $
        pwv : 0.0, $
        reference : '', $
        source : '', $
        strokeMode : '', $
        switchMode : '', $
        temperature : 0.0, $
        totalScanTime : 0L, $
        type : '', $
        utc : '', $
        velocity : 0.0, $
        windDirection : 0.0, $
        windSpeed : 0.0, $
        xFocus : 0.0, $
        yFocus : 0.0, $
        zFocus : 0.0, $
	on : on_str, $
	map : map_str, $
	skydip : skydip_str, $
	calibrate : calibrate_str $
	}

struct = obslog_str

return
end

;===============================================================================
pro build_apexobslog, tab_struct, savefilename=savefilename


if n_params() eq 0 then begin
	print, 'Calling sequence:'
	print, '	BUILD_APEXOBSLOG, log_struct, savefilename=savefilename'
	return
endif


COMMON obs1_configb, work_dir, project_name, calibration_table  



if not keyword_set(savefilename) then $
	savefilename = work_dir + 'apexdata/obslogs/apex_obslog.xdr'

toto=findfile(work_dir + 'apexdata/obslogs/*.obslog', count=nlog)


init_logstructure, strvide
get_champ_string, champ_string

tab_struct = [strvide]

chaine=''
number = ['']
commande = ['']

for i=0, nlog-1 do begin
	print, toto(i)
	get_lun, unit
	openr, unit, toto(i)

	champ = ''	
	while champ ne 'date' do begin
		readf, unit, chaine
		extract_champ, chaine, champ, value
	endwhile
	date = value

	while not eof(unit) do begin
		commande = ''
		struct = strvide
		readf, unit, chaine
		extract_champ, chaine, champ, value
		while champ ne '/scan' or champ ne '/obslog' and not eof(unit) do begin
;			print, champ, value
			if champ ne 'scan' and strmid(champ, 0, 1) ne '/' then begin
				if champ eq 'pwv' and strupcase(value) eq 'SHUTTER CLOSED' then value = -1
				if champ eq 'pwv' and string(value) eq 'Sim. Mode' then value = -1

				if where(champ_string eq champ) ne -1 then begin
					if champ eq 'command' then begin 
						struct.command = value 
						commande = ''
					endif else if champ eq 'comment' then begin 
						struct.comment = value 
						commande = ''
					endif else begin
						commande = "struct." + champ + " = " + "'" + value + "'"
					endelse
				endif else begin
					commande = "struct." + champ + " = " + string(value)
				endelse
			endif
;			print, commande
			status = execute(commande)
			
			if champ eq 'command' then begin
				command_type = strmid(value, 0, strpos(value, '('))
;				struct.command_type = command_type
				if command_type eq 'otf' or command_type eq 'raster' then begin
					extract_maplog, value, map_struct
					struct.map = map_struct
				endif else if command_type eq 'skydip' then begin
					extract_skydiplog, value, skydip_struct
					struct.skydip = skydip_struct
				endif else if command_type eq 'on' then begin
					extract_onlog, value, on_struct
					struct.on = on_struct
				endif else if command_type eq 'calibrate' then begin
					extract_calibratelog, value, calibrate_struct
					struct.calibrate = calibrate_struct
				endif
			endif
;			print, commande	
			readf, unit, chaine
			extract_champ, chaine, champ, value
			if champ eq '/scan' then begin 
				tab_struct = [tab_struct, struct]
				struct = strvide
			endif
		endwhile
	endwhile
	
	close, unit
	free_lun, unit
	
endfor

apexlog_struct = tab_struct(1:*)

;if keyword_set(save) then save, filename=savefilename, apexlog_struct, /xdr
save, filename=savefilename, apexlog_struct, /xdr

return
end
