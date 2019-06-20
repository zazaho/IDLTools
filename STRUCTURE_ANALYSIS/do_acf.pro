pro do_acf,info_file,npixels=npixels,_extra=_extra
  
  dir=file_dirname(info_file)
  prefix=file_basename(info_file,'.info')
  calc_acf,dir,prefix=prefix

end
