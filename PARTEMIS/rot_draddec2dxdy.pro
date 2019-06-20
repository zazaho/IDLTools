;+
; NAME:
;	ROT_DRADDEC2DXDY
;
; PURPOSE:
;
;	Converts set of coordinates in right ascension and declination to detector coordinates.	
;
; CALLING SEQUENCE:
;
;	ROT_DRADDEC2DXDY, Delta_alpha, Delta_delta, Rot_angle, Delta_x, Delta_y
;
; INPUTS:
;
;	Delta_alpha:	Offet in right ascencion.
;	Delta_delta:	Offet in declination.
;	Rot_angle:	Rotation angle.	
;
; OUTPUTS:
;
;	Delta_x:	Offset in detector coordinates along x.
;	Delta_y:	Offset in detector coordinates along y.	
;
; EXAMPLE:
;
;		ROT_DRADDEC2DXDY, Delta_alpha, Delta_delta, Rot_angle, Delta_x, Delta_y
;
; MODIFICATION HISTORY:
; 	
;-


PRO ROT_draddec2dxdy, delta_alpha, delta_delta, rot_angle, delta_x, delta_y

; convert set of coordinates in right ascension and declination to detector coordinates

;
; rot_angle = crota1 = parang + elev
;
delta_x = -cos(rot_angle)*delta_alpha +sin(rot_angle)*delta_delta
delta_y =  +sin(rot_angle)*delta_alpha + cos(rot_angle)*delta_delta

return 
end
