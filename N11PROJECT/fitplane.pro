;; shape = a + bx + cy + dx^2 +ey^2 + fxy
FUNCTION plane_funct, p
   COMMON common_fitshape,data,x,y,idx_finite
   case n_elements(p) of
      1: return, p[0]
      2: return, p[0] + p[1]*x
      3: return, p[0] + p[1]*x + p[2]*y
      4: return, p[0] + p[1]*x + p[2]*y + p[3]*x^2 +p[3]*y^2
      5: return, p[0] + p[1]*x + p[2]*y + p[3]*x^2 +p[4]*y^2
      else:  return, p[0] + p[1]*x + p[2]*y + p[3]*x^2 +p[4]*y^2 + p[5]*x*y
   endcase
END

FUNCTION chi2_plane, p
   COMMON common_fitshape,data,x,y,idx_finite
   return, (data-plane_funct(p))[idx_finite]

END

function fitplane_plane,d,params,refpix=refpix

   COMMON common_fitshape,data,x,y,idx_finite

   data=d
   nx = n_elements(data[*,0])
   ny = n_elements(data[0,*])

   if n_elements(refpix) ne 2 then begin
      refpix = [nx/2.,ny/2.]
   endif
   
   x = double(lindgen(nx,ny) mod nx) - refpix[0]
   y = double(lindgen(nx,ny)  / nx) - refpix[1]

   return,plane_funct(params)

END

function fitplane,d,out=out, $
                  initial_params=initial_params, $
                  nparams=nparams, $
                  refpix=refpix

   COMMON common_fitshape,data,x,y,idx_finite

   default,nparams,6

   data=d
   nx = n_elements(data[*,0])
   ny = n_elements(data[0,*])

   if n_elements(refpix) ne 2 then begin
      refpix = [nx/2.,ny/2.]
   endif
   
   x = double(lindgen(nx,ny) mod nx) - refpix[0]
   y = double(lindgen(nx,ny)  / nx) - refpix[1]
   idx_finite=where(finite(data))

   parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0]}, nparams)

   if n_elements(initial_params) eq 0 then initial_params=make_array(nparams,value=0d0)

   p = mpfit('chi2_plane',initial_params,parinfo=parinfo,ftol=1d-12)

   out=plane_funct(p)

   return,p

END

