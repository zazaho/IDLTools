;+
; NAME: sex2deg
;
; PURPOSE: convert from sexagesimal to decimal degrees
;
; CATEGORY: utilities
;
; CALLING SEQUENCE: coords = sex2dec(input)
;
; INPUTS:
; most usual way of wrting a position in sexagesimal
;     either a single string:
;          '12h23m12.564s -50d30m30.00s'
;     a string(2)) array:
;          ['12:23:12.564','-50:30:30.00']
;     or a six element array:
;          ['12','23','12.564','-50','30','30.00']
;
; OUTPUTS: position [double(2)] of ra and dec in decimal degrees
;
; EXAMPLE:
; position_degrees = sex2dec('12:23:12.564;-50:30:30.00')
;
; MODIFICATION HISTORY:
;(SH Feb  2 2011) Initial version
;-

;; 'valid' input can have many formats, what a nightmare
;; string arrays
;; standard notations:
;; ['12:23:12.564','-50:30:30.00']
;; ['12h23m12.564s','-50d30m30.00s']
;; ['12 23 12.564','-50 30 30.00']
;; ['12 23 12.564','50 30 30.00']
;; ['12 23 12.564','+50 30 30.00']

;; truncated decimals
;; ['12 23 12.564','+50 30.5']
;; ['12 23.31','+50 30.5']

;; single strings
;; standard notations:
;; '12:23:12.564 -50:30:30.00'
;; '12h23m12.564s -50d30m30.00s'
;; '12 23 12.564 -50 30 30.00'
;; '12 23 12.564 50 30 30.00'
;; '12 23 12.564 +50 30 30.00'

;; other separators
;; '12:23:12.564,-50:30:30.00'
;; '12:23:12.564;-50:30:30.00'

;; truncated decimals
;; '12 23 12.564 +50 30.5'
;; '12 23.31 +50 30.5'

;; or the simple case:
;; ['12','23','12.564','-50','30','30.00']

function sex2deg,input,help=help

  if keyword_set(help) or (n_elements(input) eq 0) then begin
     doc_library,'sex2deg'
     return,[!values.d_nan,!values.d_nan]
  endif
  
  ;; first determine whether input is one, two or six parts
  case n_elements(input) of
     1: begin
        ;; first split in 6 possible parts
        parts=stregex(input,'[^0-9+-]*([0-9+-.]*)[^0-9+-.]*([0-9+-.]*)[^0-9+-.]*([0-9+-.]*)[^0-9+-.]*([0-9+-.]*)[^0-9+-.]*([0-9+-.]*)[^0-9+-.]*([0-9+-.]*)',/extract,/subexpr)
        
        ;; check which ones are empty ''
        case total(parts eq '') of
           2: begin
              rastring = parts[1]+' '+parts[2]+' '
              decstring = parts[3]+' '+parts[4]+' '
           end
           1: begin
              ;; try to figure out between
              ;; '12 23 12.564 +50 30.5'
              ;; '12 23.5 +50 30 30.0'
              if stregex(parts[2],'\.',/boolean) then begin
                 rastring = parts[1]+' '+parts[2]+' '
                 decstring = parts[3]+' '+parts[4]+' '+parts[5]
              endif else begin
                 rastring = parts[1]+' '+parts[2]+' '+parts[3]
                 decstring = parts[4]+' '+parts[5]+' '
              endelse
           end
           0: begin
              ;; simple
              rastring = parts[1]+' '+parts[2]+' '+parts[3]
              decstring = parts[4]+' '+parts[5]+' '+parts[6]
           end
           else: begin
              message,/info,'input could not be parsed'
              return,[!values.d_nan,!values.d_nan]
           end
        endcase
     end
     2: begin
        rastring = string(input[0])
        decstring = string(input[1])
     end
     6: begin
        sinput = strcompress(string(input),/remove_all)
        rastring = input[0]+' '+input[1]+' '+input[2]
        decstring = input[3]+' '+input[4]+' '+input[5]
     end
     else: begin
        message,/info,'number of elements in input is not 1,2 or 6, cannot convert'
        return,[!values.d_nan,!values.d_nan]
     end
  endcase
  
  ;; now we should have properly defined strings for ra and dec
  raparts = stregex(rastring,'[^-+0-9]*([+-]*)([0-9]*)[^.0-9]*([.0-9]*)[^.0-9]*([.0-9]*)',/extract,/subexpr)
  rasign = ([1d0,-1d0])[raparts[1] eq '-']
  rah =   double(raparts[2])
  ram =   double(raparts[3])
  ras =   double(raparts[4])
  ra = rasign*(rah+ram/60d0+ras/3600d0)*15d0

  decparts = stregex(decstring,'[^-+0-9]*([+-]*)([0-9]*)[^.0-9]*([.0-9]*)[^.0-9]*([.0-9]*)',/extract,/subexpr)
  decsign = ([1d0,-1d0])[decparts[1] eq '-']
  decd =   double(decparts[2])
  decm =   double(decparts[3])
  decs =   double(decparts[4])
  dec = decsign*(decd+decm/60d0+decs/3600d0)

  return,[ra,dec]

end
