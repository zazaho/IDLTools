function block_itk, spdin, itk=itk, range=range

test=spdin
foo=test.data.itk
foo=(foo-foo(0))/24.

if keyword_set(itk) then begin
  indx=where(foo eq itk,ct)  
  if ct gt 0 then $
    test.data(indx).det.flag=make_flag(flag=test.data(indx).det.flag,/nodata)
endif

if keyword_set(range) then begin
  indx=where((foo ge range(0)) and (foo le range(1)),ct)
  if ct gt 0 then $
      test.data(indx).det.flag=make_flag(flag=test.data(indx).det.flag,/nodata)
endif

return,test
end

