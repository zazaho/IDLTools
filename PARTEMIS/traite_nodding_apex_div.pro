pro traite_nodding_apex_div,scan_list,n_cyc4,image,rms,cube_image,cube_rms,tau=tau,mean_lst=mean_lst,decorrel=decorrel,pwv=pwv, scan_list_group=scan_list_group

; Usage : scan_hd181327_24959 = indgen(48)+24959
;	  traite_nodding_apex_div, scan_hd181327_24959, 4, hd181327_24959, rms, cube_image, cube_rms, tau = 0.49, mean_lst = mean_lst, /pwv
;
;	  traite_nodding_apex_div, scan_hd181327_24959(1:*), 4, hd181327_24959_1, rms, cube_image, cube_rms, tau = 0.49, mean_lst = mean_lst, /pwv
;
;	  traite_nodding_apex_div, scan_saturn_49657, 4, saturn_image_49657, rms, cube_image, cube_rms, tau = 0.87, mean_lst = mean_lst,/pwv
;	  traite_nodding_apex_div, scan_hd_49703, 4, hd_image_49703, rms, cube_image, cube_rms, tau = 0.85, mean_lst = mean_lst, /decorrel,  /pwv
;	  traite_nodding_apex_div, scan_hd100546_50709, 4, hd100546_image_50709, rms, cube_image, cube_rms, tau = 1.3, mean_lst = mean_lst, /decorrel,  /pwv
;
;	  Disques = '/Users/artemis/Documents/apexdata/Disques/'
;	  restore, Disques+'betapic_50175.xdr', /verb
;	  scan_betapic_50175 = scanbeta
;
;	  traite_nodding_apex_div, scan_betapic_50015, 4, betapic_image_50015, rms_50015, cube_image, cube_rms, tau = 0.71, mean_lst = mean_lst, /decorrel,  /pwv
;
;	  traite_nodding_apex_div, scan_betapic_50135, 4, betapic_image_50135, rms_50135, cube_image, cube_rms, tau = 0.64, mean_lst = mean_lst, /decorrel,  /pwv
;
;	  traite_nodding_apex_div, scan_betapic_50175, 4, betapic_image_50175, rms_50175, cube_image, cube_rms, tau = 0.65, mean_lst = mean_lst, /decorrel,  /pwv
;
;         scan_betapic_50135tot = [scan_betapic_50135, scan_betapic_50175]
;	  traite_nodding_apex_div, scan_betapic_50135tot, 4, betapic_image_50135tot, rms_50135tot, cube_image, cube_rms, tau = 0.645, mean_lst = mean_lst, /decorrel,  /pwv
;
;	  scan_betapic_50175_50253 = scan_betapic_50175(0:75)
;traite_nodding_apex_div,scan_betapic_50175_50253, 4,betapic_image_50175_50253_new,rms_new,cube_image_new, cube_rms_new,tau=0.65,mean_lst=mean_lst,/decorrel,/pwv,scan_list_group=scan_list_group
;
;	  traite_nodding_apex_div, scan_betapic, 4, betapic_image_50015tot, rms_50015tot, cube_image, cube_rms, tau = 0.68, mean_lst = mean_lst, /decorrel,  /pwv
;
;.r UTILITIES.PRO

if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	TRAITE_NODDING_APEX_DIV, scan_list,n_cyc4,image,rms,cube_image,cube_rms,tau=tau,
	print, '                        mean_lst=mean_lst,kill50Hz=kill50Hz,decorrel=decorrel,image_nod=image_nod, pwv=pwv'
	return
endif

cycle = 4*n_cyc4 
n_file = n_elements(scan_list)

mean_lst_sub = fltarr(n_file)
pwv_sub = fltarr(n_file)
rot_angle = fltarr(n_file)
phase = intarr(n_file)
par_angle = fltarr(n_file)


for i=0, n_file-1 do begin
    
    donnees = read_apex(scan_list(i), '1', filetype)
    
    phase(i) = fix(sgn(mean(donnees.datapar.longoff)))

    print, "***********************************************"
    print, "Scan # ", scan_list(i)
    print, "PWV (mm)  = ", donnees.obslog.pwv
    pwv_sub(i) = donnees.obslog.pwv
        
    ha = mean(donnees.lst)*15/3600. - donnees.lambda
    parangle = parallactic_angle(ha, donnees.beta, site_lat=donnees.sitelat)
    print, " Hour angle (hr) = ", ha/15.
    print, " Elevation (deg) = ", donnees.elevatio
    print, " Parallactic angle (deg) = ", parangle
    crota1 = parangle + donnees.elevatio
    print, " Rotation angle (deg) = ", crota1
    rot_angle(i) = crota1
    par_angle(i) = parangle
    mean_lst_sub(i) = mean(donnees.datapar.lst)
endfor

;diff_lst = fltarr(n_file-1)
diff_lst = mean_lst_sub(1:*) - mean_lst_sub(0:n_elements(mean_lst_sub)-2)

;ind = where(diff_lst gt 2*38., count)
ind = where(diff_lst gt 2*40., count)

ind_sublist = -1
scan_sublist = -1
if count gt 0 then begin
	scan_sublist = scan_list(ind)
	ind_sublist = ind
	for i = 0, count-1 do begin
	  print, "i = ", ind(i), " mean_lst_sub(i+1)-mean_lst_sub(i) =  ", mean_lst_sub(ind(i)+1)-mean_lst_sub(ind(i))
	endfor 
endif

print, "Scan_sublist : ", scan_sublist

if count gt 0 then begin
	ind_sublist_ext  = [-1,ind_sublist,n_file-1]
        scan_sublist_ext = [scan_list(0)-1,scan_sublist,scan_list(n_file-1)]
	length = ind_sublist_ext(1:*)-ind_sublist_ext(0:n_elements(ind_sublist_ext)-2)
endif else begin
	ind_sublist_ext  = [-1,n_file-1]
        scan_sublist_ext = [scan_list(0)-1,scan_list(n_file-1)]				
	length = n_elements(scan_list)
endelse

	n_cycles = length/cycle
	remain = length mod cycle
        print, " length ", length
        print, " n_cycles ", n_cycles
        print, " remain ", remain
	remain = (remain/2)*2
        print, " even(remain) ", remain

id  = where(remain gt 3, cnt)
n_groups = n_cycles

if cnt gt 0 then begin
   n_groups(id) = n_cycles(id) +1
   print, " n_groups ", n_groups
endif

nmax_groups = max(n_groups)

print, n_file, " scans"
;

scan_list_group = lonarr(cycle,nmax_groups,n_elements(length))

;
for i = 0, n_elements(length)-1 do begin

	for k=0, n_cycles(i)-1 do begin
  		scan_list_group(*,k,i) = scan_list(ind_sublist_ext(i)+1+k*cycle:ind_sublist_ext(i)+1+k*cycle+cycle-1)
		id = where(phase(ind_sublist_ext(i)+1+k*cycle:ind_sublist_ext(i)+1+k*cycle+cycle-1) eq 1, n_pos)
		print, " Paquet = ", i, " Cycle (de 16) = ", k, " Nb de phases +1 = ", n_pos
	endfor

	if remain(i) gt 3 then begin
  		scan_list_group(0:remain(i)-1,n_cycles(i),i) = scan_list(ind_sublist_ext(i)+1+n_cycles(i)*cycle:ind_sublist_ext(i)+1+n_cycles(i)*cycle+remain(i)-1)
		id = where(phase(ind_sublist_ext(i)+1+n_cycles(i)*cycle:ind_sublist_ext(i)+1+n_cycles(i)*cycle+remain(i)-1) eq 1, n_pos)
		print, " Paquet = ", i, " Reste de    ", remain(i), " Nb de phases +1 = ", n_pos
	endif
	print, "                   "
endfor

n_groups_tot = fix(total(n_groups))
print, "n_groups_tot : ", n_groups_tot

echant = 3
npixreb = 24*echant 

image = dblarr(npixreb,npixreb)
image_nod = dblarr(npixreb,npixreb)
rms = dblarr(npixreb,npixreb)

pwv = fltarr(n_groups_tot)
tau_mean = fltarr(n_groups_tot)
mean_lst = fltarr(n_groups_tot)
cube_image = dblarr(npixreb,npixreb,n_groups_tot)
cube_rms = dblarr(npixreb,npixreb,n_groups_tot)
;;;;;scan_list_group = lonarr(cycle,n_groups_tot)

;
if keyword_set(pwv) then begin
   j_group = -1
   for i = 0, n_elements(length)-1 do begin

	for k=0, n_cycles(i)-1 do begin
		j_group = j_group + 1
		mean_lst(j_group) = mean(mean_lst_sub(ind_sublist_ext(i)+1+k*cycle:ind_sublist_ext(i)+1+k*cycle+cycle-1))
		pwv(j_group) = mean(pwv_sub(ind_sublist_ext(i)+1+k*cycle:ind_sublist_ext(i)+1+k*cycle+cycle-1))		
		id = where(phase(ind_sublist_ext(i)+1+k*cycle:ind_sublist_ext(i)+1+k*cycle+cycle-1) eq 1, n_pos)
		print, " Paquet = ", i, " Cycle (de 16) = ", k, " Nb de phases +1 = ", n_pos		
	endfor

	if remain(i) gt 3 then begin
		j_group = j_group + 1
		mean_lst(j_group) = mean(mean_lst_sub(ind_sublist_ext(i)+1+n_cycles(i)*cycle:ind_sublist_ext(i)+1+n_cycles(i)*cycle+remain(i)-1))
		pwv(j_group) = mean(pwv_sub(ind_sublist_ext(i)+1+n_cycles(i)*cycle:ind_sublist_ext(i)+1+n_cycles(i)*cycle+remain(i)-1))					
		id = where(phase(ind_sublist_ext(i)+1+n_cycles(i)*cycle:ind_sublist_ext(i)+1+n_cycles(i)*cycle+remain(i)-1) eq 1, n_pos)
		print, " Paquet = ", i, " Reste de    ", remain(i), " Nb de phases +1 = ", n_pos 		
	endif
	print, "                   "
   endfor

  pwv_glob_mean = mean(pwv_sub)
  print, "Global Mean PWV (mm)  = ", pwv_glob_mean
  print, "Min rotation angle (deg) : ", min(rot_angle)
  print, "Max rotation angle (deg) : ", max(rot_angle)
  print, "Mean rotation angle (deg) : ", mean(rot_angle)
  print, "Mean parallactic angle (deg) : ", mean(par_angle)  


  print, n_file, " scans"
  print, "n_groups_tot : ", j_group+1

endif

if keyword_set(tau) then begin
   j_group = -1
   for i = 0, n_elements(length)-1 do begin
	for k=0, n_cycles(i)-1 do begin
		j_group = j_group + 1
  		tau_mean(j_group) = tau*pwv(j_group)/pwv_glob_mean
	  print, "Group # ", j_group, "  Mean tau : ", tau_mean(j_group), "    Mean PWV (mm)  = ", pwv(j_group)
	  print, "Scans : ",scan_list_group(*,k,i)		
	  print, "Mean LST (sec) ", mean_lst(j_group)
	  print, "****************************"	
;      traite_nodding_apex_derot, scan_list_group(*,k,i), image, rms_nod, cube_nodding, nodding, tau=tau_mean(j_group), /kill50Hz, /decorrel, image_nod=image_nod, /nofilt
      traite_nodding_apex_derot, scan_list_group(*,k,i), image, rms_nod, cube_nodding, nodding, tau=tau_mean(j_group), /decorrel, image_nod=image_nod, /nofilt
	  cube_image(*,*,j_group) = image_nod
	  cube_rms(*,*,j_group) = rms_nod
	endfor
	
      if remain(i) gt 3 then begin
	  j_group = j_group + 1      
  	  tau_mean(j_group) = tau*pwv(j_group)/pwv_glob_mean
	  print, "Group # ", j_group, "  Mean tau : ", tau_mean(j_group), "    Mean PWV (mm)  = ", pwv(j_group)
	  scan_list_last = scan_list_group(0:remain(i)-1,n_cycles(i),i)
	  print, "Scans : ", scan_list_last
	  print, "Mean LST (sec) ", mean_lst(j_group)
	  print, "****************************"
;	traite_nodding_apex_derot, scan_list_last, image, rms_nod, cube_nodding, nodding, tau=tau_mean(j_group), /kill50Hz, /decorrel, image_nod=image_nod, /nofilt	
	traite_nodding_apex_derot, scan_list_last, image, rms_nod, cube_nodding, nodding, tau=tau_mean(j_group), /decorrel, image_nod=image_nod, /nofilt
	  cube_image(*,*,j_group) = image_nod
	  cube_rms(*,*,j_group) = rms_nod
      endif
   endfor      
      
      
;   image = extract_ima_moy(cube_image, rms, cube_rms = cube_rms, /rms_weight)
  image = extract_ima_moy(cube_image, rms, cube_rms = cube_rms, clip = 2.5, /rms_weight)

  array = intarr(16,16)+1
  array_ext = intarr(24,24)
  array_ext(4:19,4:19) = array
  array_res = rebin(array_ext, npixreb, npixreb, /sample)
  array_rot = rot(array_res,-mean(rot_angle),1,11*echant+echant/2,11*echant+echant/2, /interp)

;  image_filtree = image
  filter_image, image, image_filtree
  image = image_filtree*array_rot

  median_rms = median(rms(19:50,19:50))
  ind = where(rms gt 5.*median_rms,count)
  if count gt 0 then rms(ind) = 0.
;
;  rms_filtree = rms
  filter_image, rms, rms_filtree
  rms = rms_filtree*array_rot
  
  ind = where(rms eq 0.,count)
  if count gt 0 then rms(ind) = 1.e-8 & image(ind) = 0.
  
  print, "Final image -  Flux(35,35) = ", image(35,35), "  +- ", rms(35,35)
  print, "Final image -  Flux(34,40) = ", image(34,13*echant+echant/2), "  +- ", rms(34,13*echant+echant/2)
  print, "Median rms in central image = ", median(rms(19:50,19:50))
  print, "rms (standard deviation) estimate in final image = ", stdev(image(33:44,43:51))
  print, "Mean parallactic angle (deg) : ", mean(par_angle)  
  atv, image
  
  print, "n_groups_tot : ", j_group+1
    
endif else begin
  print, "Error: tau value must be provided"
  return
endelse

return
end
