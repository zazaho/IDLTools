;+
; NAME: median_absolute_deviation
;
; PURPOSE: calculate a measure of the dispersion which less sensitive
; to outliers
;
; CATEGORY: statistics
;
; CALLING SEQUENCE: mad = median_absolute_deviation(x,[/nan])
;
; INPUTS: x array of numeric values with at least 3 finite elements
;
; KEYWORD PARAMETERS: nan: if set ignore not a number values
; 
; OUTPUTS: median_absolute deviation of the given array
;
; PROCEDURE:
;  calculate median of arr
;  subtract this median from arr to obtain deviations
;  take the absolute value to get the absolute deviations
;  calculate the median to get the median absolute deviation
;  note: for a large normal distribution the median absolute deviation
;  tends to sigma/1.4826
;
; EXAMPLE:
;  mad = median_absolute_deviation(randomn(seed,1d5)
;
; MODIFICATION HISTORY:
;(SH Oct 19 2011) initial version
;-
function median_absolute_deviation,arr_in,nan=nan,_extra=_extra

  if n_elements(arr_in) lt 3 then begin
     doc_library,'median_absolute_deviation'
     return,!values.d_nan
  endif

  arr = arr_in

  if keyword_set(nan) then begin
     idx = where(finite(arr),cnt)
     if cnt lt 3 then begin
        doc_library,'median_absolute_deviation'
        return,!values.d_nan
     endif else begin
        arr = arr[idx]
     endelse
  endif

  return,median(abs(arr-median(arr)))
end
