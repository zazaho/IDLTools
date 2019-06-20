FUNCTION camolp10,file=cispfile,dir=cispdir,num=scd_num, $
                  darkmethod=darkmethod,deglitchmethod=deglitchmethod, $
                  stabilizemethod=stabilizemethod,nosave=nosave

;; Which file
default,cispfile,'cisp*.fits'

;; Which dir
default,cispdir,'../original' 

;; Which SCD do we want?
default,scd_num,0 

default,dark_method,'vilspa'
default,deglitch_method,'mm'
default,stabilize_method,'fs'

default,nosave,0

;; now read the data
spdtoscd,cispfile,sscd,dir=cispdir,/nowrite,method='new_method',/corr

corr_dark,sscd,method=darkmethod
deglitch,sscd,method=deglitchmethod

;; To remove the transient with FS we need a stable image
sscd_c = sscd_clean(sscd)
scds = sscd_elem(sscd_c[scd_num])
data = scd_get('model',scds[0])
n_data = n_elements(data[0,0,*])
stab_img = reduce_cube(data[*,*,0:(n_data<4)],/median)

stabilize,sscd,method='FS',stab=stab_img

sscd_c = sscd_clean(sscd)

raster = get_sscdraster(sscd_c[scd_num],/project)  

sscd_del,sscd
sscd_del,sscd_c

reduce,raster

;; apply the flatfield correction based on the background
corr_flat,raster,method='auto'

deglitch,raster,meth='sky'

raster_scan,raster

conv_flux,raster

IF NOT keyword_set(nosave) THEN BEGIN
    
    save,raster,file='raster_olp10.xdr',/xdr
    
    raster2fits,raster_pds,name='raster_olp10.fits'
ENDIF 

return,raster

END

