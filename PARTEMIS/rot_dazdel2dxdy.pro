;+
; NAME:
;	ROT_DAZDEL2DXDY
;
; PURPOSE:
;
;	Converts set of coordinates in azimuth and elevation to detector coordinates.	
;
; CALLING SEQUENCE:
;	
;	ROT_DAZDEL2DXDY, Delta_az, Delta_el, Elev, Delta_x, Delta_y
;
; INPUTS:
;
;	Elev:		Elevation
;	Delta_az:	Offet in azimuth.
;	Delta_el:	Offet in elevation.
;
; OUTPUTS:
;
;	Delta_x:	Offset in detector coordinates along x.
;	Delta_y:	Offset in detector coordinates along y.		
;
; EXAMPLE:
;
;		ROT_DAZDEL2DXDY, Delta_az, Delta_el, Elev, Delta_x, Delta_y
;
; MODIFICATION HISTORY:
; 	
;-

PRO ROT_dazdel2dxdy, delta_az, delta_el, elev, delta_x, delta_y

; convert set of coordinates in azimuth and elevation to detector coordinates 

rot_angle = elev*!pi/180

delta_x =  cos(rot_angle)*delta_az + sin(rot_angle)*delta_el
delta_y = -sin(rot_angle)*delta_az + cos(rot_angle)*delta_el

return 
end
