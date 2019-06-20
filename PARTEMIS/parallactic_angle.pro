;+
; NAME:
;	PARALLACTIC_ANGLE
;
; PURPOSE:
;
;	Calculates parallactic angle.
;
; CALLING SEQUENCE:
; 
;	Parang = PARALLACTIC_ANGLE(Ha, Delta)
;
; INPUTS:
;
;	Ha:	Horar angle.
;	Delta:	Declination.
;
; OPTIONAL INPUTS:
;
;	site_lat:	Latitude.
;			
; EXAMPLE:
;	
;		Parang = PARALLACTIC_ANGLE(Ha, Delta, Site_lat=53)
;
; MODIFICATION HISTORY:
;
;-

function parallactic_angle, ha, delta, site_lat=site_lat

; Renvoie l'angle parallactic en degree pour une source observée depuis le site
; de Kosma (ou d'APEX) de declinaison delta (deg) et d'angle horaire HA (deg)

if n_params() ne 2 then begin
	print, 'Calling sequence :'
	print, '	parang = PARALLACTIC_ANGLE( HA, delta, site_lat=site_lat )'
	return, -1
endif


if not keyword_set(site_lat) then latitude = 45.9834 else latitude=site_lat

deg2rad = !pi/180
rad2deg = 1./deg2rad

coslat = cos(latitude*deg2rad)
sinlat = sin(latitude*deg2rad)
coslatc = cos((90.-latitude)*deg2rad)
sinlatc = sin((90.-latitude)*deg2rad)
;
sindelta = sin(delta*deg2rad)
cosdelta = cos(delta*deg2rad)
sindeltac = sin((90.-delta)*deg2rad)
cosdeltac = cos((90.-delta)*deg2rad)

sinha = sin(ha*deg2rad)
cosha = cos(ha*deg2rad)

cosz = sindelta*sinlat+cosdelta*coslat*cosha		; z = 90-elev
sinz = sqrt(1.-cosz*cosz)

tan_parang = coslat*sinHA/(sinlat*cosdelta-coslat*sindelta*cosHA)

cos_parang = (coslatc-cosz*cosdeltac)/(sinz*sindeltac)

parang = acos(cos_parang)*rad2deg
;if ha le 0. then parang = -parang
ind = where(ha le 0., count)
if count gt 0 then parang(ind) = -parang(ind)
;
;print, "parang (deg) = ", parang

parang2 = atan(tan_parang)*rad2deg
;
;print, "parang2 (deg) = ", parang2

return, parang
end
