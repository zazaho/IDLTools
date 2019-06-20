;; function to create a mbfits file from a directory which contains
;; todo make header, make help check inputs

pro implode_mbfits,dirname,filename,_extra=_extra

  default,filename,'imploded_mbfits.fits'
  writembfits,filename,readmbfits(dirname,_extra=_extra)
  
end
