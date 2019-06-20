;+
; NAME:
;	INIT_OBS
;
; PURPOSE:
;
;	Read and fill a structure containing directory path, calibration file
;	and subscan list for a given observation ID.
;
; CALLING SEQUENCE:
;	
;	INIT_OBS, Scan_number=Scan_number, Type=Type, Init_obs_str
;
; INPUTS:
;
;	Scan_number:	Observation ID.
;	Type:		MAP, CHMAP or DECORREL.
;
; OUTPUTS:
;
;	Init_obs_str:	Output structure.
;
; COMMON BLOCKS:
;
;	OBS1_CONFIGB:	Contains working directory, project and calibration table name
;		
; EXAMPLE:
;
;		INIT_OBS, Scan_number=47822, Type='MAP', Init_obs_str
;
; MODIFICATION HISTORY:
;
;-

pro init_obs, scan_number=scan_number, type=type, init_obs_str


if n_params() eq 0 then begin
	print, 'Calling sequence :'
	print, '	INIT_OBS, Scan_number=Scan_number, Type=Type, Init_obs_str
        return
endif	

; remplit une structure qui contient les chemins d'acces, les fichiers de calibration, la liste des subscans pour un scan donne
;
;@obs1_config
;build_apexobslog, toto
;traite_otf_map_main, scan_number= 4792, type = 'map', jupiter_4792_str, tau=0.49, /newreduc, /do_rcp			;   donnees de mars 2007
;
; Check with: init_obs, scan_number= 4792, type = 'map', init_obs_4792_str
;
;noise_fft_skydip, 19038, sky_19038_str, /nopowermap
;noise_fft_skydip, 19038, sky_19038_str
;
;continuous_skydip, 22484
;
;init_obs, scan_number= 19037, type = 'map', init_obs_19037_str
;traite_otf_map_main, scan_number= 19037, type = 'chmap', jupiter_ch_map_19037_str, tau=1.0,/newreduc, /nopowermap, /med_base
;traite_otf_map_main, scan_number= 19037, type = 'map', jupiter_19037_str, tau=1.0,/newreduc, /nopowermap, /do_rcp, /med_base 
;
;traite_otf_map_main, scan_number= 19036, type = 'map', jupiter_spiral_19036_str, tau=1.0,/newreduc, /nopowermap, /med_base
;traite_otf_map_main, scan_number= 19036, type = 'map', jupiter_spiral_19036_str, tau=1.0,/newreduc, /med_base
;
;traite_otf_map_main, scan_number= 19345, type = 'map', jupiter_spiral_19345_str, tau=1.0,/newreduc, /med_base
;traite_otf_map_main, scan_number= 19389, type = 'decorrel', jupiter_spiral_19389_str, tau=1.0,/newreduc, /med_base
;
;traite_otf_map_main, scan_number= 25192, type = 'map', jupiter_spiral_25192_str, tau=0.6, /newreduc, /med_base
;traite_otf_map_main, scan_number= 25192, type = 'chmap', jupiter_spiral_25192_str, tau=0.6, /newreduc, /med_base
;
;traite_otf_map_main, scan_number= 19360, type = 'decorrel', mars_spiral_19360_str, tau=1.0,/newreduc, /med_base
; for i = 0, 1343 do atv, donnees_red.cube(*,*,i)
;
;traite_otf_map_main, scan_number= 19969, type = 'map', saturn_19969_str, tau=1.0,/newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24863, type = 'map', saturn_spiral_24863_str, tau=0.75,/newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24515, type = 'decorrel', mars_24515_str, tau=1.5,/newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24520, type = 'decorrel', mars_24520_str, tau=1.5,/newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24822, type = 'map', mars_24822_str, tau=0.83,/newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24527, type = 'decorrel', neptune_spiral_24527_str, tau=1.5,/newreduc
;traite_otf_map_main, scan_number= 24780, type = 'decorrel', neptune_spiral_24780_str, tau=0.8,/do_rcp
;traite_otf_map_main, scan_number= 24798, type = 'decorrel', neptune_spiral_24798_str, tau=0.6,/do_rcp
;traite_otf_map_main, scan_number= 24924, type = 'decorrel', neptune_spiral_24924_str, tau=0.55,/do_rcp
;traite_otf_map_main, scan_number= 25018, type = 'decorrel', neptune_spiral_25018_str, tau=0.5,/do_rcp
;
;traite_otf_map_main, scan_number= 41234, type = 'decorrel', neptune_41234_str, tau=0.7,/do_rcp
;
;traite_otf_map_main, scan_number= 19392, type = 'decorrel', uranus_spiral_19392_str, tau=0.8,/newreduc, /med_base
;traite_otf_map_main, scan_number= 24949, type = 'decorrel', uranus_spiral_24949_str, tau=0.49,/newreduc
;traite_otf_map_main, scan_number= 24781, type = 'decorrel', uranus_spiral_24781_str, tau=0.8,/do_rcp
;traite_otf_map_main, scan_number= 24803, type = 'decorrel', uranus_spiral_24803_str, tau=0.6,/do_rcp
;traite_otf_map_main, scan_number= 24947, type = 'decorrel', uranus_spiral_24947_str, tau=0.47,/do_rcp
;traite_otf_map_main, scan_number= 24949, type = 'decorrel', uranus_spiral_24949_str, tau=0.47,/do_rcp
;traite_otf_map_main, scan_number= 25020, type = 'decorrel', uranus_spiral_25020_str, tau=0.5,/do_rcp
;
;traite_otf_map_main, scan_number= 19374, type = 'map', mars_19374_str, tau=0.82,/newreduc, /do_rcp, /med_base 
;traite_otf_map_main, scan_number= 19374, type = 'chmap', mars_ch_map_19374_str, tau=0.82,/newreduc, /med_base
;
;traite_otf_map_main, scan_number= 20128, type = 'decorrel', g45_20128_str, tau=1.0,/newreduc, /med_base			; continuous OTF
;traite_otf_map_main, scan_number= 20129, type = 'decorrel', g45_20129_str, tau=1.0,/newreduc, /med_base 			, normal OTF
;
;traite_otf_map_main, scan_number= 19686, type = 'map', venus_19686_str, tau=1.0,/newreduc, /do_rcp, /med_base
;traite_otf_map_main, scan_number= 20532, type = 'map', venus_spiral_20532_str, tau=0.53,/newreduc, /do_rcp, /med_base           ; Chilean account
;traite_otf_map_main, scan_number= 21446, type = 'map', venus_spiral_21446_str, tau=1.0,/newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 20536, type = 'map', mars_spiral_20536_str, tau=0.58,/newreduc, /do_rcp, /med_base            ; Chilean account
;traite_otf_map_main, scan_number= 20539, type = 'map', mars_20539_str, tau=0.65,/newreduc, /do_rcp		                   ; ESO 0994 account
;traite_otf_map_main, scan_number= 25032, type = 'map', mars_spiral_25032_str, tau=0.5,/newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25050, type = 'map', mars_spiral_25050_str, tau=0.5,/newreduc, /do_rcp
;noise_fft_skydip, 20556, sky_20556_str
;
;traite_otf_map_main, scan_number= 25123, type = 'decorrel', b13134_spiral_25123_str, tau=0.99,/do_rcp
;traite_otf_map_main, scan_number= 25369, type = 'decorrel', iras16293_spiral_25369_str, tau=0.7,/do_rcp
;traite_otf_map_main, scan_number= 25378, type = 'decorrel', iras16293_spiral_25378_str, tau=0.8,/do_rcp
;make_fits, iras16293_spiral_25378_str
;
;noise_fft_skydip, 22910, sky_22910_str
;traite_otf_map_main, scan_number= 22909, type = 'map', mars_22909_str, tau=0.79,/newreduc, /do_rcp
; 
;traite_otf_map_main, scan_number= 21194, type = 'decorrel', cwleo_21194_str, tau=1.0,/newreduc, /med_base			; continuous OTF
;
;traite_otf_map_main, scan_number= 23371, type = 'decorrel', haro11_spiral_23371_str, tau=1.2, /newreduc, /do_rcp, /med_base
;traite_otf_map_main, scan_number= 23379, type = 'decorrel', haro11_spiral_23379_str, tau=1.2, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24724, type = 'decorrel', rcw106mms5_spiral_24724_str, tau=1.1, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24725, type = 'decorrel', rcw106mms5_24725_str, tau=1.1, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24728, type = 'decorrel', rcw106mms68_spiral_24728_str, tau=1.08, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24729, type = 'decorrel', rcw106mms68_24729_str, tau=1.08, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24730, type = 'decorrel', rcw106mms39_spiral_24730_str, tau=1.1, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24731, type = 'decorrel', rcw106mms39_24731_str, tau=1.1, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24732, type = 'decorrel', rcw106mms84_spiral_24732_str, tau=1.1, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24733, type = 'decorrel', rcw106mms84_spiral_24733_str, tau=1.1, /newreduc, /do_rcp   ; Ignore ??
;
;traite_otf_map_main, scan_number= 24739, type = 'decorrel', g343_126_spiral_24739_str, tau=1.05, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24740, type = 'decorrel', g343_126_24740_str, tau=1.05, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24744, type = 'decorrel', g326_474_24744_str, tau=1.05, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24756, type = 'decorrel', g335_582_spiral_24756_str, tau=1.0, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24757, type = 'decorrel', g335_582_24757_str, tau=1.0, /newreduc, /do_rcp
;
;traite_otf_map_main,scan_number=24760,type='decorrel',b59mms1_spiral_24760_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.9,/do_rcp
;traite_otf_map_main,scan_number=24761,type='decorrel',b59mms2_24761_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.9,/do_rcp
;traite_otf_map_main,scan_number=24762,type='decorrel',b59mms2_spiral_24762_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.9,/do_rcp
;traite_otf_map_main,scan_number=24763,type='decorrel',b59mms2_spiral_24763_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.9,/do_rcp
;
;traite_otf_map_main,scan_number=24929,type='decorrel',b59mms2_24929_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.55,/do_rcp
;traite_otf_map_main,scan_number=24931,type='decorrel',b59mms2_spiral_24931_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.55,/do_rcp
;
;traite_otf_map_main,scan_number=25381,type='decorrel',b59mms3_spiral_25381_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.78,/do_rcp
;traite_otf_map_main,scan_number=25382,type='decorrel',b59mms1_spiral_25382_str,champ_base='b59_mask_str.xdr',model='b59_model_str.xdr',champ_masque='b59_mask_str.xdr',tau=0.77,/do_rcp
;
;combine_otf_map, [24760,24761,24762,24763,24929,24931,25381,25382], b59_comb_str, /do_rcp
;;;;;combine_otf_map, [24760,24761,24929,24931,25381,25382], b59_part_str, /do_rcp
;save, file='b59_comb_str.xdr', b59_comb_str, /verb
;
;traite_otf_map_main, scan_number= 25124, type = 'decorrel', chamms1_spiral_25124_str, tau=0.95, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 25565, type = 'decorrel', betapic_spiral_25565_str, tau=0.66, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25566, type = 'decorrel', betapic_spiral_25566_str, tau=0.66, /newreduc, /do_rcp         ;    IGNORE ??
;traite_otf_map_main, scan_number= 25571, type = 'decorrel', betapic_spiral_25571_str, tau=0.66, /newreduc, /do_rcp
;combine_otf_map, [25565,25571], betapic_comb_str, /do_rcp
;combine_otf_map, [25565,25566, 25571], betapic_comb_all_str, /do_rcp
;
;make_fits, betapic_comb_all_str, fileout='betapic_comb'
;
;
;traite_otf_map_main, scan_number= 41251, type = 'decorrel', hd181327_spiral_41251_str, tau=0.73, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 41252, type = 'decorrel', hd181327_spiral_41252_str, tau=0.7, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24765, type = 'decorrel', g330_spiral_24765_str, tau=0.85, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24766, type = 'decorrel', g330_24766_str, tau=0.8, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24826, type = 'decorrel', n159_24826_str, tau=0.75, /newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 24868, type = 'decorrel', vela1_spiral_24868_str, tau=0.93, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24869, type = 'decorrel', vela1_spiral_24869_str, tau=0.93, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24870, type = 'decorrel', vela1_24870_str, tau=0.93, /newreduc, /do_rcp
;combine_otf_map, [24868, 24869, 24870], vela1_comb_str, /do_rcp
;
;traite_otf_map_main, scan_number= 24877, type = 'decorrel', vela2_spiral_24877_str, tau=0.93, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24878, type = 'decorrel', vela2_24878_str, tau=0.93, /newreduc, /do_rcp
;combine_otf_map, [24877, 24878], vela2_comb_str, /do_rcp
;
;traite_otf_map_main, scan_number= 25134, type = 'decorrel', vela3_spiral_25134_str, tau=0.8, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25135, type = 'decorrel', vela3_25135_str, tau=0.8, /newreduc, /do_rcp
;combine_otf_map, [25134, 25135], vela3_comb_str, /do_rcp
;
;traite_otf_map_main, scan_number= 24935, type = 'decorrel', rcw106mms84_24935_str, tau=0.55, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25180, type = 'decorrel', rcw106mms84_25180_str, tau=0.5, /newreduc, /do_rcp
;combine_otf_map, [24935, 25180], rcw106mms84_comb_str, /do_rcp
;
;traite_otf_map_main, scan_number= 25186, type = 'decorrel', g345_490_25186_str, tau=0.58, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25188, type = 'decorrel', g345_499_25188_str, tau=0.6, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25661, type = 'decorrel', g345_490_25661_str, tau=0.915, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25663, type = 'decorrel', g345_499_25663_str, tau=0.915, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25664, type = 'decorrel', g345_499_25663_str, tau=0.915, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 25669, type = 'decorrel', g345_490_25669_str, tau=1., /newreduc, /do_rcp
;combine_otf_map, [25186, 25188, 25661, 25663, 25664, 25669], g345_490_comb_str, /do_rcp
;save, file='g345_490_comb_str.xdr', g345_490_comb_str, /verb
;
;traite_otf_map_main, scan_number= 24879, type = 'decorrel', iras08076_spiral_24879_str, tau=0.93, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24880, type = 'decorrel', iras08076_24880_str, tau=0.93, /newreduc, /do_rcp
;combine_otf_map, [24879, 24880], iras08076_comb_str, /do_rcp
;
;traite_otf_map_main, scan_number= 24910, type = 'decorrel', lupus3_spiral_24910_str, tau=0.6, /newreduc, /do_rcp
;traite_otf_map_main, scan_number= 24911, type = 'decorrel', lupus3_24911_str, tau=0.6, /newreduc, /do_rcp
;combine_otf_map, [24910, 24911], lupus3_comb_str, /do_rcp
;
;traite_otf_map_main, scan_number= 48990, type = 'map', hd97048_48990_tp_str, tau=0.71, rmode = 'tp', /do_rcp, /newreduc
;traite_otf_map_main, scan_number= 48990, type = 'decorrel', hd97048_48990_tp_decorrel_str, tau=0.71, rmode = 'tp', /do_rcp
;traite_otf_map_main, scan_number= 48990, type = 'decorrel', hd97048_48990_saa_decorrel_str, tau=0.71, rmode = 'saa', /do_rcp
;traite_otf_map_main, scan_number= 48990, type = 'decorrel', hd97048_48990_wob_decorrel_str, tau=0.71, rmode = 'ekh', /do_rcp
;
;traite_otf_map_main, scan_number= 50010, type = 'map', map_50010_str, tau=0.8,/newreduc, /do_rcp
;traite_otf_map_main, scan_number= 50010, type = 'chmap', mars_ch_map_50010_str, tau=0.8,/newreduc 
;
;traite_otf_map_main, scan_number= 49020, type = 'map', map_49020_str, tau=1.1,/newreduc, /do_rcp
;
;traite_otf_map_main, scan_number= 49020, type = 'decorrel', map_decorrel_49020_str, tau=1.1
;
;traite_otf_map_main, scan_number= 49020, type = 'decorrel', map_decorrel_mod_49020_str, champ_base='g34_mask_str.xdr', model='g34_model_str.xdr',champ_masque='g34_mask_str.xdr', tau=1.1, /do_rcp
;
;traite_otf_map_main, scan_number= 47543, type = 'decorrel', map_decorrel_47543_str, champ_base='bhr71_mask_str.xdr', model='bhr71_model_str.xdr', champ_masque='bhr71_mask_str.xdr', tau=0.6
;
;traite_otf_map_main, scan_number= 48973, type = 'decorrel', s255_48973_decorrel_str, champ_base='s255_mask_base_str.xdr', model='s255_model_str.xdr', champ_masque='s255_mask_str.xdr',tau=0.825, /do_rcp
;
;traite_otf_map_main, scan_number= 49170, type = 'decorrel', s255_49170_decorrel_str, champ_base='s255_mask_base_str.xdr', model='s255_model_str.xdr', champ_masque='s255_mask_str.xdr', tau=0.9,/do_rcp
;
;combine_otf_map, [49170,48973], s255_comb_decorrel_str
;
;traite_otf_map_main, scan_number= 48814, type = 'decorrel', ngc2264c_48814_decorrel_str, champ_base='ngc2264c_mask_base_str.xdr', model='ngc2264c_model2_str.xdr', champ_masque='ngc2264c_mask2_str.xdr', tau=1.04,/do_rcp 
;
;traite_otf_map_main, scan_number= 48971, type = 'decorrel', ngc2264c_48971_decorrel_str, champ_base='ngc2264c_mask_base_str.xdr', model='ngc2264c_model2_str.xdr', champ_masque='ngc2264c_mask2_str.xdr', tau=0.91,/do_rcp 
;
;combine_otf_map, [48814], ngc2264c_48814_comb
;.r remove_uncordrifts
;combine_otf_map, [48814], ngc2264c_48814_rdrift, /removedrift, /do_rcp
;combine_otf_map, [48814,48971], ngc2264c_48814_48971_rdrift, /removedrift, /do_rcp
;
;traite_otf_map_main,scan_number=4825, type='decorrel', n3576_4825_dec_mod2_str, champ_base='n3576_4825_mask_base.xdr', model='n3576_4825_model2_str.xdr', champ_masque='n3576_4825_mask.xdr', tau=0.65, /do_rcp
;traite_otf_map_main,scan_number=4828, type='decorrel', n3576_4828_dec_mod2_str, champ_base='n3576_4828_mask_base.xdr', model='n3576_4828_model2_str.xdr', champ_masque='n3576_4828_mask.xdr', tau=0.8, /do_rcp
;combine_otf_map, [4825, 4828], n3576_4825_4828
;combine_otf_map, [4825, 4828], n3576_4825_4828_rdrift, /removedrift, /do_rcp
;
;traite_otf_map_main,scan_number=47527, type='decorrel', n3576_47527_dec_mod2_str, champ_base='n3576_47527_mask_base.xdr', model='n3576_47527_model2_str.xdr', champ_masque='n3576_47527_mask.xdr', tau=0.79, /do_rcp
;
;traite_otf_map_main,scan_number=47536, type='decorrel', n3576_47536_dec_mod2_str, champ_base='n3576_47536_mask_base.xdr', model='n3576_47536_model2_str.xdr', champ_masque='n3576_47536_mask.xdr', tau=0.73, /do_rcp
;
;combine_otf_map, [47527, 47536], n3576_47527_47536
;
;combine_otf_map, [47527, 47536], n3576_47527_47536_rdrift, /removedrift, /do_rcp
;
;combine_otf_map, [4825, 4828, 47527, 47536], n3576_4825_4828_47527_47536

COMMON obs1_configb, work_dir, project_name, calibration_table    ; chargement des common variables depuis obs1_config

scan_number=scan_number

obsname = 'APEX-' + strtrim(string(scan_number),1)		;  Nov 2007

apexdata = work_dir+'apexdata/'					    ; contient les donnees


Calib_partemis = work_dir+'Calib_partemis/'                         ; contient les fichiers de calibration
;Calib_partemis ='/mnt/local/home/pandre/partemis/Calib_partemis/'  ; SAPPCSTARFORM



dirdat = apexdata + 'rawdata/'                                      ; contient les donnees brutes en format MBFITS

dirdef = apexdata + 'basic_xdr/'                                    ; contient les donnees brutes(V) ou etalonnees(pW) en format XDR (IDL structure)

; recherche de la date




u=findfile(dirdef+'*'+strtrim(string(scan_number),1)+'*', count=nfiles)

if u[0] EQ '' then begin

u=find_all_dir(dirdat) ; tous les repertoires / sous repertoires contenant les rawdata 
       
       if u[0] EQ '' then begin
       
       date=''
       
       endif else begin
       
       subscan_liste=u[where(strmatch(u,'*'+strtrim(string(scan_number),1)+'*',/FOLD_CASE) EQ 1)] ; selection des repertoires / sous repertoires lies au scan_number
       subscan_liste=subscan_liste[0]
       pos1=strpos(subscan_liste[0],'APEX')
       ;date=strmid(subscan_liste, date, 30)
       date=strmid(subscan_liste, pos1, 30)
       date=strsplit(date, /EXTRACT, '-')
       ;date=var1[2:4]
       date=date[2:4]
       endelse

endif else begin

restore, u[0]
date=strsplit(datastr.date_obs, /EXTRACT, 'T')
date=date[0]
date=strsplit(datastr.date_obs, /EXTRACT, '-')

endelse

if  (date[0] NE '') then begin
if(date[0] LE 2007) AND ((date[1] LT 11)) then begin

if scan_number lt 5000 then begin
   project_name_save = project_name        
   project_name = 'T-78.F-0006-2006'
;;   help, project_name   
endif   

endif

endif

;basic_name = '/mnt/local/home/pandre/partemis/apexdata/basic_xdr/' + project_name +'_' + strtrim(string(scan_number),1) ;SAPPCSTARFORM

;basic_name = basic_xdr + project_name +'_' + strtrim(string(scan_number),1) 

;basic_name = dirdef + project_name +'_' + strtrim(string(scan_number),1)  

basic_name = dirdef + '*' +'_' + strtrim(string(scan_number),1)  


if  (date[0] NE '') then begin

if (date[0] LE 2007) AND (date[1] LT 11) then begin

if scan_number lt 5000 then begin
   project_name = project_name_save     
;   help, project_name  
endif   

endif

endif

subscan_liste = findfile(basic_name + '_*', count=nfiles)

if n_elements(subscan_liste) gt 0  and subscan_liste[0] NE '' then begin   ; les fichiers basic_xdr files existent, construction de la subscan liste
									   ; a partir des basic_xdr
for i = 0, n_elements(subscan_liste)-1 do print, subscan_liste(i)          



;sc_liste_pos=strlen(basic_name)+1;           ; ou extraire le subscan

sc_liste_pos=min(strlen(subscan_liste))-1-4					    ; BUG ! (if one wants to select only subscans > 9)
;sc_liste_pos=min(strlen(subscan_liste))-2-4					   ; To select subscans > 17 of scan 22909


;subscan_liste = strmid(subscan_liste,sc_liste_pos,3)                              ;  for all home directories
subscan_liste = strmid(subscan_liste,sc_liste_pos,6) 					

for i = 0, n_elements(subscan_liste)-1 do print, subscan_liste(i)

for i = 0, n_elements(subscan_liste)-1 do begin

subscan_liste(i) = (strsplit(subscan_liste(i),'.',/extract))(0)

endfor

subscan_liste = subscan_liste(sort(long(subscan_liste)))

endif else begin   ; les fichiers basic_xdr n'existent pas, on contruit la subscan_liste a partir des rawdata


u=find_all_dir(dirdat) ; tous les repertoires / sous repertoires contenant les rawdata 



subscan_liste=u[where(strmatch(u,'*-'+strtrim(string(scan_number),1)+'-*',/FOLD_CASE) EQ 1)] ; selection des repertoires / sous repertoires lies au scan_number

sc_liste_pos=min(strlen(subscan_liste))+1

subscan_liste = strmid(subscan_liste,sc_liste_pos,3)     ; extraction des subscans

subscan_liste = subscan_liste(sort(long(subscan_liste)))

   if subscan_liste(0) EQ '' then begin
 
   subscan_liste=subscan_liste(1:n_elements(subscan_liste)-1)
 
   endif

endelse


for i = 0, n_elements(subscan_liste)-1 do print, subscan_liste(i)




subscan_name = project_name +'/' + strtrim(string(scan_number), 2) + '_' + subscan_liste



;calibration_table2=work_dir+'apexpro/'+calibration_table   ; le fichier calibration_table est place dans apexpro

calibration_table2=Calib_partemis+calibration_table   ; le fichier calibration_table est place dans Calib_partemis

openr, lun, calibration_table2 ,/GET_LUN                    ; ouverture du fichier
nlines=File_lines(calibration_table2)                       ; lecture du nombre de lignes
table=strarr(nlines)                                       ; generation d'une table de caracteres de longueur nlines
readf, lun, table                                          ; on stocke la lecture du fichier calibration_table dans table


compteur=0 
    
number=['0','1','2','3','4','5','6','7','8','9']
    
for i=0, nlines-1  do begin 


    v=strmatch(number,strmid(strcompress(table(i)),0,1))   ; reperage si les lignes de calibration_table commence par un chiffre ou non
    
    if (total(v) eq 0) then begin                          ; si ca ne commence pas par un chiffre par ex. un blanc ou du texte, on incremente
    compteur=compteur+1                                    ; la ligne compteur est la ligne a partir de laquelle on peut extraire l'information necessaire
    endif else begin 
   
    endelse 

endfor


table2=strarr(nlines-compteur,7);                         ; deuxieme table qui va ne contenir que les lignes utiles, et 5 colonnes pour scan_number, Conv, goodpix_ima 
                                                           ;flatfield, calibration_camera et rcp_file 


for i=0, nlines-compteur-1  do begin

table(compteur+i)=strjoin(strsplit(table(compteur+i),/EXTRACT),'-')   ; pour une ligne utile, les blancs sont elimines et remplaces par un '-'
table2(i,*)=strsplit(table(compteur+i),/EXTRACT,'-')                  ; extraction des sous chaines de caracteres (5 par ligne)


endfor



                                          ; conversion string en entier du scan_number
;scan_table=intarr(nlines-compteur)                                     
;scan_table=table2(*,0) 
scan_table=long(table2(*,0))
                                           ; contient la liste des scan_number dont nous connaissons quelques informations

min_index=min(where(scan_number le scan_table))
max_index=max(where(scan_number ge scan_table))

if (where(scan_number eq scan_table) ne -1) then begin

index_c=where(scan_number eq scan_table);                              ; si le scan_number se trouve dans la scan_table 

endif else begin
                                                                       ; si le scan_number ne se trouve pas dans la scan_table (cas le plus frequent)
;index_c=min(min_index,max_index)                                       ; on prend la ligne correspondant au numero de scan le plus proche inferieurement. Chaque ligne
index_c=min([min_index,max_index])
                                                                       ; contient des fichiers de calibration differents 
                                                                       ; de calibration, prendre la valeur superieure ne changerait rien d'ailleurs
endelse

;stop

print, "Restoring ", Calib_partemis+table2(index_c,2)
restore, Calib_partemis+table2(index_c,2)                       ; restoration de goodpix_ima, flat, et affectation de Conv
;
print, "Restoring ", Calib_partemis+table2(index_c,3)
restore, Calib_partemis+table2(index_c,3)
;
print, "Restoring ", Calib_partemis+table2(index_c,5)
restore, Calib_partemis+table2(index_c,5)                       ; restoration du fichier rcp

;if (scan_number le 5000) then begin
;  dx_mars_best = dx_jupiter_best
;  dy_mars_best = dy_jupiter_best
;endif

;dx_rcp = dx_mars_best
;dy_rcp = dy_mars_best

calibration_camera=Calib_partemis+table2(index_c,4);

;if scan_number lt 5000 then begin
;   goodpix_ima = goodpix_ima_4792
;   flat_field = flat_jupiter_4792_gauss
;endif


;if scan_number gt 5000 and scan_number lt 47913 then begin
;   goodpix_ima=goodpix_ima_mars
;   flat_field=flat_mars
;endif

;if scan_number ge 47913 then begin
;goodpix_ima=goodpix_ima_50010
;flat_field=flat_mars_50010
;endif

pfov=float(table2(index_c,6))               

pfov=pfov[0]                                ; extraction de la taille du pixel                             

; remplissage de la structure

if type ne ('chmap') then begin             ; type CHMAP

Conv=float(table2(index_c,1))*0.0001
Conv=Conv[0]

init_obs_str = {scan_number:scan_number, type:type, work_dir:work_dir, project_name:project_name, apexdata:apexdata, dirdat:dirdat, subscan_liste:subscan_liste, subscan_name:subscan_name,$
goodpix_ima:goodpix_ima $
, flat_field:flat_field, Conv:Conv, dx_rcp:dx_rcp, dy_rcp:dy_rcp, calibration_camera:calibration_camera, obsname:obsname, pfov:pfov, date:date}

endif else begin                            ; type MAP ou DECORREL

Conv=float(table2(index_c,1))*0.0001
Conv=Conv[0] 

init_obs_str = {scan_number:scan_number, type:type, work_dir:work_dir, project_name:project_name, apexdata:apexdata, dirdat:dirdat, subscan_liste:subscan_liste, subscan_name:subscan_name,$
goodpix_ima:goodpix_ima $
, flat_field:flat_field, Conv:Conv, calibration_camera:calibration_camera, obsname:obsname, pfov:pfov, date:date}

endelse

;help, project_name

free_lun, lun

end
