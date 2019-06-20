function atrou_hd, map, nscale
;+
; NAME:
;  ATROU_HD
;
; PURPOSE:
; Compute the "a trou" wavelet transform of an image.
; The result is a cube with each plane has the same dimension
; as the original image.
;
; if im is the input image and if you selected 8 scales, then:
; low = im - total(cube, 3) ; get very low frequencies not directly computed
; new_image = total(cube, 3) + low ; compute new image by adding all frequencies
;  you will see that im and new_image are exaclty the same
;
; CALLING SEQUENCE:
;  result = atrou(map, nscale)
; 
; INPUTS:
;  map: 2D array
;
; OPTIONAL INPUT KEYWORD:
;  nscale: number of scales (default=6)
;
; OUTPUTS:
;  a cube with a scale by plane
;
; SIDE EFFECTS:
;  none
;
; RESTRICTIONS:
;  does not work on complex array
;
; EXAMPLES:
;   cube = atrou_hd(im, 7)
;   cube2 = atrou_hd(im, 4)
;
; MODIFICATION HISTORY:
; 24-Apr-2001 Written Marc-Antoine Miville Deschenes, ENS, Paris
; 20-Nov-2001 add offset and print, Herve Dole University of Arizona (when at IAS)
; 03-Dec-2001 remove offset (useless) and put more comments HD UofA
; 27-Feb-2002 add cosmetics on print HD UofA
;
;-

if not keyword_set(nscale) then nscale = 6
;if not keyword_set(offset) then offset = 0

; Kernel
;-------
kernel_val = [1./16, 1./8., 1./16., 1./8., 1./4., 1./8., 1./16., 1./8, 1./16]

si_map = size(map)
cube = fltarr(si_map(1), si_map(2), nscale)
tempo = map

; Loop Range
;-----------
offset=0
istart = offset+0
iend = offset+nscale-1

; Main Loop on Scales
;--------------------
for i=istart, iend do begin 
    start_time = SYSTIME(1)
    PRINT, '------------------------------------------------------------'
    PRINT, 'i: '+STRING(i, FORMAT='(I2)')+'/'+STRING(iend, FORMAT='(I2)'), FORMAT='(A9,$)'

    n = fix(2*(2^i)+1) 
    kernel = fltarr(n, n) 
    dx = (n-1)/2. 
    lc = (n^2-n)/2. 
    indice = [0, dx, 2*dx, lc, lc+dx, lc+2*dx, 2*lc, 2*lc+dx, 2*(lc+dx)] 
    kernel(indice) = kernel_val 
    tsmooth = convol(tempo, kernel, /edge_truncate)  
    cube(*,*,i) = tempo - tsmooth  
    tempo = tsmooth  

    end_time = SYSTIME(1)
    str_time = STRING(end_time - start_time, FORMAT='(F5.2)')
    PRINT, ' in '+str_time+' s'
endfor

return, cube

end

