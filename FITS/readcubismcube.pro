function readcubismcube,cubename

  ;; read the data
  cube = mrdfits(cubename,0,hcube)

  ;; read the wavelength info
  cubewave = mrdfits(cubename,1,hcubewave)

  ;; make sure cubewave is a simple array
  ;; check routines for problems because of this change
  if size(cubewave,/tname) eq 'STRUCT' then cubewave = reform(cubewave.(0)) else cubewave = reform(cubewave)
  
  mask = make_array(n_elements(cube[*,0,0]),n_elements(cube[0,*,0]),value=0)

  return,{cube:cube,header:hcube,wave:cubewave.wavelength,hwave:hcubewave,mask:mask}

end
