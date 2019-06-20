
;+
; NAME:
;	MADPARTEMISWRAP
;
; PURPOSE:
;
;	Wrap P-ArTeMiS data into the format required by MADmap and execute C version of MADmap.
;	If anything goes wrong with the execution of MADmap, use the java version MadMapC included 
;       in HIPE directly from your terminal after having produced your data in big_endian.	
;
; CALLING SEQUENCE:
;
;	MADPARTEMISWRAP, Scan_number=Scan_number
;
; INPUTS:
;
;	Scan_number:	List of observations ID.
;
; OPTIONAL INPUTS:
;
;	Level:	Use MADmap on reduced data (map_otf_xdr)
;
;       Rsamp:	Binning factor on output map, the output grid is made of squared cells with a
;		size of pixel field of view/Rsamp. Default value is Rsamp=3.    
;
; EXAMPLE:
;                   !path = '/mnt/local/home/pandre/partemis/apexpro/apexpro_v4/MADmap_routines/MADmapwrap_idl:'+!path
;		    .r "MADmap_routines/MADmapwrap_idl/madwrap.pro"
;              	    MADPARTEMISWRAP, Scan_number=50010
;
;		    MADPARTEMISWRAP, Scan_number=[50010,50011,50012]
;
;		    MADPARTEMISWRAP, Scan_number=[50010,50011,50012], Rsamp=2		
;
;		    invntt = madmap_read_invntt('/mnt/data2/pandre/partemis/apexdata/MADmap_data/invntt_0.100', /big)
;		    tod = madmap_read_tod('/mnt/data2/pandre/partemis/apexdata/MADmap_data/tod', /big)
;
;[pandre@sappcstarform MADmap_data]$java -Xms256m -Xmx1024m  herschel.ia.toolbox.mapper.MadMapC -t tod -n invntt_0 -i 10000 -m 2000000000 map_0
;
;		    restore, filename='pt.xdr', /verb		;  info on final RA/DEC grid
;		    map_0 =madmap_read_map('/mnt/data2/pandre/partemis/apexdata/MADmap_data/map_0',dim=pt.dim_all,index=pt.obs_all_index,$
;					   redim=pt.npix, start=pt.start,/big_endian)
;
; MODIFICATION HISTORY:
; 	
;-


;
; @obs1_config
;
;
;
; !PATH='/mnt/local/home/gwillman/apexpro_pipeline_MADmap/MADmap-idl/idl:'+ !PATH


pro madpartemiswrap, scan_number=scan_number, rsamp=rsamp


COMMON obs1_configb, work_dir, project_name, calibration_table     ; chargement des common variables depuis obs1_config


if keyword_set(rsamp) EQ 0 then rsamp=3.0d0

nmax=intarr(n_elements(scan_number))

subscan_listes=strarr(n_elements(scan_number),1000)




for k=0,n_elements(scan_number)-1 do begin

init_obs, scan_number=scan_number[k], type='map', init_obs_str

nmax[k]=n_elements(init_obs_str.subscan_liste)

subscan_listes(k,0:nmax[k]-1)=init_obs_str.subscan_liste


endfor



pfov=init_obs_str.pfov


nframe=intarr(total(nmax))

rsamp = double(rsamp)
ncols = 16
nrows = 16
dpix = double(pfov/3600.)
;ncorr=100
goodpix_ima=init_obs_str.goodpix_ima
;goodpix_ima=fltarr(16,16)+1.
dbad=reform(goodpix_ima,256)
;dbad=dblarr(256)+1.
igood=where(dbad EQ 1, ndetector)
ixdetector=igood mod nrows
iydetector=igood / nrows
ixdetector=double(ixdetector)*rsamp
iydetector=double(iydetector)*rsamp
;xoffset = (ixdetector-8.)*pfov/3600. ;*rsamp ; output map coordinate system
;yoffset = (iydetector-8.)*pfov/3600. ;*rsamp ; output map coordinate system
;xoffset = (ixdetector-8)*pfov/3600. ;*rsamp ; output map coordinate system
;yoffset = (iydetector-8)*pfov/3600. ;*rsamp ; output map coordinate system
;xoffset = reform(init_obs_str.dx_rcp/3600.,256)
;yoffset = reform(init_obs_str.dy_rcp/3600.,256)
;xoffset = xoffset(igood)
;yoffset = yoffset(igood)
;scan with data, in this file crota1 in [deg] is stored 
ifirst = 0l
tod_froms = -1l



print, 'RESTORING DATA FILES...'

shift=0

for k=0,n_elements(scan_number)-1 do begin


  for j=0,nmax[k]-1 do begin   ; pour tous les subscans




restore, work_dir +'apexdata/map_otf_xdr/' + project_name + '/otf_subscan_'+ strtrim(string(scan_number[k]),1) + '_' + subscan_listes[k,j] + '.xdr'




taille=size(donnees.cube)


nframe[j+shift]= taille(3)          ; a chaque indice de nframe, on a le nombre d'images acquises lors d'un subscan

  endfor

shift=shift+nmax[k]

endfor

nframes=total(nframe)       ; nombre d'images acquises pour tout le scan

nmaxs=total(nmax)



subscan_index=fltarr(2*nmaxs)   ;  nmax est le nombre de subscans

matrix_index=fltarr(2*nmaxs,nmaxs+1)  ; variable intermediaire servant au calcul de subscan_index

for j=1,nmaxs do begin

matrix_index[(2*j-1):(2*nmaxs-1),j]=nframe[j-1]

endfor

matrix_index[(2*indgen(nmaxs)+1),0]=-1


subscan_index=fltarr(2*nmaxs)

for j=0, 2*nmaxs-1 do begin

subscan_index(j)=total(matrix_index(j,*))

endfor

subscan_index=reform(subscan_index,2,nmaxs)  ; de la forme [2,nmax] contient les indices de debut et 
					    ; de fin pour chaque subscan (nmax subscan au total)
					    ; afin de situer les subscans dans les tables qui 
					    ; contiennent toutes les donnees du scan avec un indice 
					    ; variant de 1 a nframes



ra_cen=dblarr(nframes)
dec_cen=dblarr(nframes)

data_cube=fltarr(nrows,ncols,nframes)

rot_angle=fltarr(nframes)

dx=init_obs_str.dx_rcp/3600.;        les rcp sont a present en degres
dy=init_obs_str.dy_rcp/3600.;



alpha=fltarr(16,16,nframes)
delta=fltarr(16,16,nframes)


x_cen=dblarr(nframes)
y_cen=dblarr(nframes)

rot_angle=fltarr(nmaxs)

shift=0

for k=0,n_elements(scan_number)-1 do begin


  for j=0,nmax[k]-1 do begin   ; pour tous les subscans


; restaure un subscan



restore, work_dir +'apexdata/map_otf_xdr/' + project_name + '/otf_subscan_'+ strtrim(string(scan_number[k]),1) + '_' + subscan_listes[k,j] + '.xdr'



size=n_elements(donnees.datapar(*).baslong) ; returns the size of the array  [16,16,size] 


; remplissage de la table position, subscan apres subscan

ra_cen(subscan_index(0,j+shift):subscan_index(1,j+shift))=donnees.datapar.baslong
dec_cen(subscan_index(0,j+shift):subscan_index(1,j+shift))=donnees.datapar.baslat


; remplissage de la table cube et angle de rotation, subscan apres subscan



data_cube(*,*,subscan_index(0,j+shift):subscan_index(1,j+shift))=donnees_red.cube



rot_angle(j+shift)=carte_scan.crota1*!pi/180.

  endfor

shift=shift+nmax[k]

endfor

taille=size(data_cube)



ra0  = median(ra_cen)                   ; center of output map, de tout le scan
dec0 = median(dec_cen)                  ; center of output map


; methode de projection (x,y) numero 1 :
 
mkhdr,outhdr,dblarr(1, 1)
fxaddpar, outhdr,'BITPIX', -32
fxaddpar, outhdr,'NAXIS', 2;
fxaddpar,outhdr,'CTYPE1', 'RA---TAN'
fxaddpar,outhdr,'CTYPE2', 'DEC--TAN'
fxaddpar,outhdr,'EQUINOX', 2000.0
fxaddpar,outhdr,'CRVAL1', ra0
fxaddpar,outhdr,'CRVAL2', dec0
fxaddpar,outhdr,'CRPIX1', 1
fxaddpar,outhdr,'CRPIX2', 1
fxaddpar,outhdr,'CROTA2', 0
fxaddpar,outhdr,'CDELT1', -pfov/3600.d0/rsamp
fxaddpar,outhdr,'CDELT2', pfov/3600.d0/rsamp



dxoffset=fltarr(nframes,ndetector)				
dyoffset=fltarr(nframes,ndetector)

for j=0, nmaxs-1 do begin

dx=init_obs_str.dx_rcp/3600.;
dy=init_obs_str.dy_rcp/3600.;

dx=reform(dx,256)
dy=reform(dy,256)

dx=dx(igood)
dy=dy(igood)


rot_dxdy2draddec, dx, dy, rot_angle(j), dalpha, ddelta ; rotation des rcp, effectuee pour chaque subscan

dx=dalpha
dy=ddelta


dxoffset[subscan_index(0,j):subscan_index(1,j),*]=(fltarr(subscan_index(1,j)-subscan_index(0,j)+1)+1)#reform(dx,ndetector)
dyoffset[subscan_index(0,j):subscan_index(1,j),*]=(fltarr(subscan_index(1,j)-subscan_index(0,j)+1)+1)#reform(dy,ndetector)

dxoffset[subscan_index(0,j):subscan_index(1,j),*]=dxoffset[subscan_index(0,j):subscan_index(1,j),*]/(cos(dec_cen[subscan_index(0,j):subscan_index(1,j),*]*!pi/180.)#(fltarr(ndetector)+1.))



endfor




; reorganisation des valeurs du cube dans le meme format :

tod_value = madpartemis_format_cube(data_cube, goodpix_ima, rsamp)


nsample = n_elements(tod_value[*,0])
    
udetector = make_array(ndetector, /double, value=1)
usample = make_array(nsample, /double, value=1)


ra=fltarr(nframes,ndetector)
dec=fltarr(nframes,ndetector)

for j=0, nmaxs-1 do begin  ; pour tous les subscans

usample = make_array((subscan_index(1,j)-subscan_index(0,j)+1), /double, value=1)


ra(subscan_index(0,j):subscan_index(1,j),*) = (ra_cen(subscan_index(0,j):subscan_index(1,j)) # udetector + dxoffset(subscan_index(0,j):subscan_index(1,j),*))
dec(subscan_index(0,j):subscan_index(1,j),*) = (dec_cen(subscan_index(0,j):subscan_index(1,j)) # udetector + dyoffset(subscan_index(0,j):subscan_index(1,j),*))


endfor

usample = make_array(nsample, /double, value=1)


adxy, outhdr, ra, dec, x, y
x=round(x)
y=round(y)




nfile=1
ifirst=0l
tod_xs = -1l
tod_ys = -1l
tod_values = 0.

nsample=taille(3)

tod_xs     = [tod_xs, x[*]]
tod_ys     = [tod_ys, y[*]]
tod_values = [tod_values,tod_value[*]]
tod_xs     = tod_xs[1:*]
tod_ys     = tod_ys[1:*]
tod_values = tod_values[1:*]


tod_from = ifirst + lindgen(ndetector)*nframes

tod_froms  = [tod_froms, tod_from]

tod_froms  = tod_froms [1:*]
tod_tos    = [tod_froms[1:*], n_elements(tod_values)]-1

 
 save, tod_xs, tod_ys, tod_values, file='tod_data.xdr' 

; spawn, 'rm /mnt/local/home/gwillman/core'
 spawn, 'rm /mnt/local/home/pandre/core'


madwrap,tod_values, tod_xs, tod_ys, dir = work_dir + 'apexdata/MADmap_data',from=tod_froms,to=tod_tos, fsamp=40, $
calib_invnttfilename='/mnt/local/home/gwillman/apexpro_pipeline_v4_cea/apexpro/in_pow_spec', ncorr=255



madwrap_write_invntt_calib, '/mnt/data2/pandre/partemis/apexdata/MADmap_data/invntt_1', from=tod_froms, to=tod_tos,$
                                   calib='/mnt/local/home/gwillman/apexpro_pipeline_v4_cea/apexpro/in_pow_spec'


;restore, '/mnt/local/home/gwillman/pt.xdr'
;map=madmap_read_map(work_dir + 'apexdata/MADmap_data/map_0',dim=pt.dim_all, index=pt.obs_all_index, redim=pt.npix, start=pt.start,/big_endian)
;window
;tvscl, congrid(map,600,600)



end
