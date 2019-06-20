function km,lambda,MODEL=model,PRINT=print

if not keyword_set(model) then begin
;	restore,'kuruczmodel_30000K_3.5g'
	restore,'kuruczmodel_30000K_3.5g_10log'
;	model = kurlin
	model = kurlog
endif

if not keyword_set(print) then begin
	print ='yes'
endif

loglambda = lambda
for i = 0,n_elements(lambda)-1 do begin
   loglambda(i) = alog10(lambda(i))
endfor


;test = spline(model.wave,model.flux,lambda)
test = 10^spline(model.wave,model.flux,loglambda)
if not(print eq 'no') then begin
for i = 0,n_elements(test)-1 do begin
;   print,'<-> Model atm. heeft bij ',lambda(i),' micron',test(i),' Jy flux'
   print,'<-> Model atm. heeft bij ',lambda(i),' micron',test(i),' Jy flux'
endfor
endif
return,test
end