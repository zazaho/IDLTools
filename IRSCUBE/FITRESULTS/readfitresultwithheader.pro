;; read a fit result into a handy structure with the header coming from a different fits file
;; hdrfilename provides the header

function readfitresultwithheader,sourcename, $
                       topdir=topdir, $
                       subdir=subdir, $
                       xdrfilename=xdrfilename, $
                       hdrfilename=hdrfilename

  fullhdrfilename=topdir+'/'+sourcename+'/'+subdir+'/'+hdrfilename

  struct=readfitresult(sourcename,topdir=topdir,subdir=subdir,xdrfilename=xdrfilename)
  hdr=headfits(fullhdrfilename)

  nx=sxpar(hdr,'naxis1')
  ny=sxpar(hdr,'naxis2')

  foo = lindgen(nx,ny)

  x = foo mod nx
  y = foo  /  nx

  xyad,hdr,x,y,ra,dec
  return,create_struct(struct,'HEADER',hdr,'X',x,'Y',y,'RA',ra,'DEC',dec)

end
