;; merge the results from two steps into one fit structure
;; use the mask
PRO mergestep,s1,s2,factor=factor
  
  ;; the s2 is not valid, do nothing
  if not isstruct(s2) then begin
     return
  endif

  ;; the s1 is not valid but the second one is, so use the second one
  ;; as the first step and return
  if not isstruct(s1) and isstruct(s2) then begin
     s1 = s2
     
     ;; make sure we apply the same scaling to s2 as below
     for i=0,n_tags(s1.derived_variables)-1 do begin
        s1.derived_variables.(i) = s1.derived_variables.(i)/factor
     endfor
     
     return
  endif

;  s1_org = s1

  h1 = s1.header
  h2 = s2.header
  
  sxaddpar,h1,'naxis',2
  sxaddpar,h2,'naxis',2
  sxdelpar,h1,'naxis3'
  sxdelpar,h2,'naxis3'

  m1 = s1.mask
  m2 = s2.mask

  np1 = n_elements(m1)
  np2 = n_elements(m2)

  ;;p_ means projected
  hastrom,m2,h2,p_m2,htemp,h1

  idxtobereplaced = where((m1 ne 0) and (p_m2 eq 0),cnt)

  if cnt eq 0 then return

  for i=0,n_tags(s1.derived_variables)-1 do begin
     if (n_elements(s1.derived_variables.(i)) eq np1) and (n_elements(s2.derived_variables.(i)) eq np2) then begin
        hastrom,s2.derived_variables.(i),h2,projected,htemp,h1
        s1.derived_variables.(i)[idxtobereplaced] = projected[idxtobereplaced]/factor
     endif
  endfor
  
  for i=0,n_tags(s1.band_ratios)-1 do begin
     if (n_elements(s1.band_ratios.(i)) eq np1) and (n_elements(s2.band_ratios.(i)) eq np2) then begin
        hastrom,s2.band_ratios.(i),h2,projected,htemp,h1
        s1.band_ratios.(i)[idxtobereplaced] = projected[idxtobereplaced]
     endif
  endfor

  s1.mask[idxtobereplaced] = 0

;  stop

end
