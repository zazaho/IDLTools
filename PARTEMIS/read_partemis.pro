pro read_partemis,filein,header,data

;lecture des donnees p-artemis au format mbfits APEX
;un fichier par acquisition
fileout=strmid(filein,0,strlen(filein)-5)+'.xdr'
raw_data=mrdfits(filein,1,header)
images=reform(raw_data.data)
s=size(images)
cube=reform(images,16,16,s(2))

;creation variable IDL a la BOLC
;creation de la structure HK
;il n'y a qu'un seul MD alimente par la Keithley
hk={HK_CAMERA,$ 
VH:-1.,$
VL:-1.,$
VRL:-1.,$
VINJ:-1.,$
HEATER:-1.,$
VDL:-1.,$
VSS:-1.,$
VGL:-1.,$
CKRLH:-1.,$
CKRLL:-1.,$
VDECXH:-1.,$
VDECXL:-1.,$
VSMSH:-1.,$
VSMSL:-1.,$
VDDPROT_CL:-1.,$
GND_BU:-1.,$
VDD:-1.,$
VGG:-1.,$
VSS_BU:-1.,$
VDL_BU:-1.,$
VGL_BU:-1.,$
VDDPROT_BU:-1.,$
I_HEATER:-1.,$
I_VSS:-1.,$
I_VSS_BU:-1.,$
VH_BLIND:-1.,$
CKTRIL_REF:-1.,$
PWR_ANA_P:-1.,$
PWR_ANA_N:-1.,$
PWR_DIG:-1.,$
__1:-1.,$
__2:-1.,$
;ajout de HK specifiques Keithley
;FREQ:-1,$
;NBMOY:-1,$
;HACHEUR:-1,$
;DIFF:-1,$
OFFSET:-1.}
;infos telescope
apex={HK_TELESCOPE,EL:-1.}

;creation de la structure frame
;only 2 blue arrays
bmask=bytarr(16,16,2)
frame={APEX_DATA,$
blue:make_array(16,16,2,/float),$
hk:hk,gain:-1,unit:'raw',data_type:fix(-1),fn:fix(-1),$
bmask:ptr_new(bmask),rmask:ptr_new(rmask),apex:apex}
;creation de la structure data
; nframes=sxpar(header,'NBIMG')
nframes=sxpar(header,'NAXIS2')
data=replicate(frame,nframes)
;remplissage des donnees pixels
data.blue(*,*,0)=cube
;remplissage du gain
data.gain=fltarr(nframes)+sxpar(header,'gain')
;remplissage des donnees HK
;il n'y a pas de HK avec la Keithley, sauf les mesures courants, on remplit donc avec les consignes (gasp...)
data.hk.VH=fltarr(nframes)+sxpar(header,'VH')
data.hk.VL=fltarr(nframes)+sxpar(header,'VL')
data.hk.VRL=fltarr(nframes)+sxpar(header,'VRL')
data.hk.VINJ=fltarr(nframes)+sxpar(header,'VINJ')
;data.hk.HEATER=fltarr(nframes)+sxpar(header,'VCH')
data.hk.VDL=fltarr(nframes)+sxpar(header,'VDL')
data.hk.VSS=fltarr(nframes)+sxpar(header,'VSS')
data.hk.VGL=fltarr(nframes)+sxpar(header,'VGL')
data.hk.CKRLH=fltarr(nframes)+sxpar(header,'CKRLH')
data.hk.CKRLL=fltarr(nframes)+sxpar(header,'CKRLL')
data.hk.VDECXH=fltarr(nframes)+sxpar(header,'VDECXH')
data.hk.VDECXL=fltarr(nframes)+sxpar(header,'VDECXL')
data.hk.VSMSH=fltarr(nframes)+sxpar(header,'VSMSH')
data.hk.VSMSL=fltarr(nframes)+sxpar(header,'VSMSL')
data.hk.VDDPROT_CL=fltarr(nframes)+sxpar(header,'HIERARCH VDDPROTCL')
data.hk.GND_BU=fltarr(nframes)+sxpar(header,'GNDBU')
data.hk.VDD=fltarr(nframes)+sxpar(header,'VDD')
data.hk.VGG=fltarr(nframes)+sxpar(header,'VGG')
data.hk.VSS_BU=fltarr(nframes)+sxpar(header,'VSSBU')
data.hk.VDL_BU=fltarr(nframes)+sxpar(header,'VDLBU')
data.hk.VGL_BU=fltarr(nframes)+sxpar(header,'VGLBU')
data.hk.VDDPROT_BU=fltarr(nframes)+sxpar(header,'HIERARCH VDDPROTBU')
;data.hk.I_HEATER=reform(mes(2,*))
data.hk.I_VSS=fltarr(nframes)+sxpar(header,'IVSS')
data.hk.I_VSS_BU=fltarr(nframes)+sxpar(header,'IVSSBU')
data.hk.VH_BLIND=fltarr(nframes)+sxpar(header,'VHBLIND')
;data.hk.CKTRIL_REF=fltarr(nframes)+0.
;data.hk.FREQ=fltarr(nframes)+sxpar(header,'FREQ')
;data.hk.NBMOY=fltarr(nframes)+sxpar(header,'NBMOY')
;data.hk.HACHEUR=fltarr(nframes)+sxpar(header,'HACHEUR')
;data.hk.DIFF=fltarr(nframes)+sxpar(header,'DIFF')
data.hk.OFFSET=fltarr(nframes)+sxpar(header,'OFFSET')
;if ctel gt 0 then data.apex.EL=fltarr(nframes)+sxpar(header_tel,'ELEVATIO')
save,data,file=fileout

return
end
