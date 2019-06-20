;+
; NAME:
;	ROT_DXDY2DAZDEL
;
; PURPOSE:
;
;	Converts set of coordinates in detector coordinates to azimuth and elevation.
;	
; CALLING SEQUENCE:
;
;	ROT_DXDY2DAZDEL, Delta_x, Delta_y, Elev, Delta_az, Delta_el
;
; INPUTS:
;
;	Elev:		Elevation
;	Delta_x:	Offset in detector coordinates along x.
;	Delta_y:	Offset in detector coordinates along y.		
;
; OUTPUTS:
;
;	Delta_az:	Offet in azimuth.
;	Delta_el:	Offet in elevation.
;
; EXAMPLE:
;
;		ROT_DAZDEL2DXDY, Delta_x, Delta_y, Elev, Delta_az, Delta_el
;
; MODIFICATION HISTORY:
; 	
;-


PRO ROT_dxdy2dazdel, delta_x, delta_y, elev, delta_az, delta_el

; convert set of coordinates in detector coordinates to azimuth and elevation      

rot_angle = elev*!pi/180
delta_az = cos(rot_angle)*delta_x - sin(rot_angle)*delta_y
delta_el = sin(rot_angle)*delta_x + cos(rot_angle)*delta_y


return 
end
