;+
; NAME:
;	ROT_DXDY2DRADDEC
;
; PURPOSE:
;
;	Converts set of coordinates in detector coordinates to right ascension and declination.
;	
; CALLING SEQUENCE:
;	
;	ROT_DXDY2DRADDEC, Delta_x, Delta_y, Rot_angle, Delta_alpha, Delta_delta
;
; INPUTS:
;
;	Rot_angle:	Rotation angle.
;	Delta_x:	Offset in detector coordinates along x.
;	Delta_y:	Offset in detector coordinates along y.		
;
; OUTPUTS:
;
;	Delta_alpha:	Offet in right ascencion.
;	Delta_delta:	Offet in declination.
;
; EXAMPLE:
;
;		ROT_DXDY2DRADDEC, Delta_x, Delta_y, Rot_angle, Delta_alpha, Delta_delta
;
; MODIFICATION HISTORY:
; 	
;-


PRO ROT_dxdy2draddec, delta_x, delta_y, rot_angle, delta_alpha, delta_delta

; convert set of coordinates in detector coordinates to right ascension and declination

;
; rot_angle = crota1 =  parang + elev
;
delta_alpha = -cos(rot_angle)*delta_x + sin(rot_angle)*delta_y
delta_delta =  sin(rot_angle)*delta_x + cos(rot_angle)*delta_y


return 
end
