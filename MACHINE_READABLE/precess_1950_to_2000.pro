PRO precess_1950_to_2000, ra, dec
;+
; NAME:
;      PRECESS_1950_TO_2000
; PURPOSE:
;      Precess coordinates from 1950 to 2000
; CALLING SEQUENCE:
;      PRECESS_1950_TO_2000, ra, dec
;
; INPUT - OUTPUT:
;      RA - Input right ascension (scalar or vector) in DEGREES
;      DEC - Input declination in DEGREES (scalar or vector)
;              
;      The input RA and DEC are modified by PRECESS_1950_TO_2000 to give the 
;      values after precess_1950_to_2000.
;
; RESTRICTIONS:
;       Accuracy of precess_1950_to_2000 decreases for declination values near 90 
;       degrees.  PRECESS_1950_TO_2000 should not be used more than 2.5 centuries from
;       1950.
;
; EXAMPLES:
;       precess_1950_to_2000 the B1950 coordinates of Eps Ind (RA = 21h 59m,33.053s,
;       DEC = (-56d, 59', 33.053") to equinox 2000
;
;       IDL> ra = ten(21, 59, 33.053)*15
;       IDL> dec = ten(-56, 59, 33.053)
;       IDL> precess_1950_to_2000, ra, dec
;
; PROCEDURE:
;       Algorithm from Computational Spherical Astronomy by Taff (1983), 
;       p. 24. (FK4). FK5 constants from "Astronomical Almanac Explanatory
;       Supplement 1992, page 104 Table 3.211.1.
;
; REVISION HISTORY
;       Written, Wayne Landsman, STI Corporation  August 1986
;       Correct negative output RA values   February 1989
;       Added /PRINT keyword      W. Landsman   November, 1991
;       Provided FK5 (J2000.0)  I. Freedman   January 1994
;       precess_1950_to_2000ion Matrix computation now in PREMAT   W. Landsman June 1994
;       Added /RADIAN keyword                         W. Landsman June 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Correct negative output RA values when /RADIAN used    March 1999 
;       Work for arrays, not just vectors  W. Landsman    September 2003 
;(SH Jan 26 2006) Simplyfied for coord_mr
;-    
  On_error,2                    ;Return to caller

  npar = N_params()
  
  ;; check the input
  if ( npar LT 2 ) then begin 
      doc_library,'precess_1950_to_2000'
      return 
  endif
  
   nra = N_elements(ra)
   ndec = N_elements(dec)
   
   IF (nra EQ 0) OR (ndec EQ 0) OR (nra NE ndec) THEN BEGIN
       doc_library,'precess_1950_to_2000'
       message,'ERROR - Input RA and DEC must be scalars or vectors of the same length'
   ENDIF
   
   deg_to_rad = !DPI/180.0D0
   
   ;; the transformation matrix from 1950 to 2000 using premat.pro
   r=[[0.99992571,0.011178891,0.0048589835], $
      [-0.011178891,0.99993751,-2.7157699e-05], $
      [-0.0048589834,-2.7162367e-05,0.99998819]]
   
   ra_rad = double(reform([ra],/over)*deg_to_rad) ;Convert to radian and double precision
   dec_rad = double(reform([dec],/over)*deg_to_rad)  ;;Making sure they are simple 1D arrays
   
   ;; create the matrix for rotation
   x1950 = transpose([[cos(dec_rad)*cos(ra_rad)], $
                      [cos(dec_rad)*sin(ra_rad)], $
                      [sin(dec_rad)]])

   x2000 = r#x1950              ;rotate to get output direction cosines
   

   ;; and get new coordinates out of the rotated matrix
   ra = reform(atan(x2000[1,*],x2000[0,*])/deg_to_rad,/over)
   ra = ra + (ra LT 0.)*360.D   ;RA between 0 and 360 degrees
   dec = reform(asin(x2[2,*])/deg_to_rad,/over)
   
   return 
END
