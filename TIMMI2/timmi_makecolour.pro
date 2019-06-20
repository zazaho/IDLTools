function timmi_bbfitfunct,par
COMMON cm_chi2_sh_bbfit,f
ffit = par[1]*3.97296d19/([7.9,11.9,12.9,20.0]^3d0)/ $
  (exp(1.43875d4/([7.9,11.9,12.9,20.0]*par[0]))-1d0)
return,TOTAL((f-ffit)^2d0)
END

function timmi_bbfit,f_in
COMMON cm_chi2_sh_bbfit,f
f = f_in

temp=701d0
bb = 3.97296d19/([7.9,11.9,12.9,20.0]^3d0)/ $
  (exp(1.43875d4/([7.9,11.9,12.9,20.0]*temp))-1d0)
scale = mean(f/bb)

;; So now we know the defaults for the optimisation
par = [temp,scale]
par = amoeba(1d-6,FUNCTION_NAME="timmi_bbfitfunct",P0=par,scale=.5*par)

return, par[0]

END

function timmi_makecolour,i1,i2,i3,i4
out = i1
sn_elements = n2s(n_elements(i1))
for i = 0,n_elements(i1) -1 do begin
    out[i]=timmi_bbfit([i1[i],i2[i],i3[i],i4[i]])
    print,'Pixel:'+n2s(i)+'/'+sn_elements+',BB TEMPERATURE: '+f2s(out[i])
endfor
return,out
end
