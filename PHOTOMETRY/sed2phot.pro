;+
; NAME:
;  sed2phot
;
;
; PURPOSE:
;  convolve a sed (wavelength,flux) table to synthetic photometry
;  using filter profile tables
;
; CATEGORY:
; Data Reduction/Analysis
;
; CALLING SEQUENCE:
;  phot=sed2phot(sed) or
;  phot=sed2phot(wave,flux)
;
; INPUTS:
;  sed: data structure which contains .data.wave and .data.flux columns
;  wave,flux: vectors which describe the input sed
;
; KEYWORD PARAMETERS:
;  filterdata=filterdata a structure describing the filter system
;   if should contain the following tags:
;   name, filename, referencewavelength
;   wavelengthunit [angstrom (1e-10 m) if not specified]
;   calibrationconvention [-1 if not specified] (meaning F_nu ~ nu^(-1))
;   detectortype [energycounter if not set] (energycounter or photoncounter)
;
;  /plot: if set make a plot showing the results
;  /quiet: if set do not print results to the terminal
;
;  relaxed=relaxed: if set, be less strict when applying the wavelength
;     criterion which determines if the data cover the filter.
;     It allows you specify a limit (in fraction of the peak
;     responsivity) below which data is not essential to calculate the
;     filter photometry. If /relaxed is given this fraction is set to
;     0.01, other wise the value is the value of relaxed
;
;  /galex: if set calculate GALEX photometry
;  /denis: if set calculate DENIS photometry
;  /twomass: if set calculate 2MASS photometry
;  /irac: if set calculate IRAC photometry
;  /msx: if set calculate MSX photometry
;  /wise: if set calculate WISE photometry
;  /iras: if set calculate IRAS photometry
;  /mips: if set calculate MIPS photometry
;  /pacs: if set calculate PACS photometry
;  /spire: if set calculate SPIRE photometry
;  /akari: if set calculate AKARI photometry
;  /herschel: if set calculate HERSCHEL photometry (PACS and SPIRE)
;  /spitzer: if set calculate SPITZER photometry (IRAC and MIPS)
;  /all: if set calculate all predefined filters
;
;  profilesdir=profilesdir (default '~/IA_FILES/FILTER_PROFILES')
;
; OUTPUTS:
;  two column matrix containing the synthetic photometry
;
; OPTIONAL OUTPUTS:
;  colorcorrections=colorcorrections
;
; SIDE EFFECTS:
;  may produce a plot
;  may print to the terminal
;
; MODIFICATION HISTORY:
; (SH Apr 22 2013): Initial version
; (SH Apr 23 2013): corrected input of two parameters (wave,flux)
;                   linear resampling is more suitable now that we
;                   calculate the integral only within one given filter
; (SH May  2 2013): small bug fix incalculation on npoints
; (SH May  6 2013): bug fix for WISE calibration convention
;                   bug fix for 2MASS calibration convention
;                   add detector type to instrument data. Note that
;                   some experiments have include the factor lambda in
;                   the Response curve even for photon detectors
; (SH Mar 24 2014)  Corrected refence wavelength to those given at
; http://irsa.ipac.caltech.edu/data/SPITZER/docs/mips/mipsinstrumenthandbook/51
; Warning: reference wavelength may vary between catalogs
;-


;; simple trapesium integration in X_nu * d_nu
FUNCTION sed2phot_integrate,w,f
  
  x=1d0/w
  y=f
  
  idx = sort(x)
  x = x[idx]
  y = y[idx] 

  npoints=n_elements(x)
  intval=fltarr(npoints)
    
  intval[1:npoints-2]=0.5*(x[2:npoints-1]-x[0:npoints-3])
  intval[0]=0.25*(x[1]-x[0])
  intval[npoints-1]=0.25*(x[npoints-1]-x[npoints-2])

  return,total(y*intval)

END

function sed2phot,p1,p2, $
                  filterdata=filterdata, $
                  plot=plot, $
                  quiet=quiet, $
                  iras=iras, $
                  akari=akari, $
                  twomass=twomass, $
                  wise=wise, $
                  galex=galex, $
                  denis=denis, $
                  irac=irac, $
                  mips=mips, $
                  spitzer=spitzer, $
                  pacs=pacs, $
                  spire=spire, $
                  herschel=herschel, $
                  msx=msx, $
                  all=all, $
                  colorcorrections=colorcorrections, $
                  relaxed=relaxed, $
                  profilesdir=profilesdir, $
                  help=help, $
                  _extra=_extra


  oldquiet = !QUIET
  if keyword_set(quiet) then !QUIET=1
  
  c = 2.99792458d8                        ;; Speed of light [SI]

  ;; XXXXXX ADAPT to local installation XXXXXXXX
  if n_elements(profilesdir) ne 1 then profilesdir=shell_expand('~/IA_FILES/FILTER_PROFILES/')
  
  colors=[!d.table_size*(.1+0.06*[8,13,5,1,11,6,2,10,3,14,9,4,12,7])]
  ncolors=n_elements(colors)
  
  needhelp=0
  helptext=''

  if keyword_set(help) then needhelp=1

  ;; check input
  case n_params() of
     1: begin
        if not tag_exist(p1,'DATA') then begin
           helptext='The input does not contain a .DATA tag'
           needhelp=1
           break
        endif
        if (not tag_exist(p1.data,'WAVE')) or (not tag_exist(p1.data,'FLUX')) then begin
           helptext='The input does not contain .DATA.WAVE and .DATA.FLUX tags'
           needhelp=1
           break
        endif
        wave = reform(p1.data.wave)
        f_nu_actual = reform(p1.data.flux)
     end
     2: begin
        if n_elements(p1) ne n_elements(p2) then begin
           helptext='The input vectors are not of the same size'
           needhelp=1
           break
        endif
        wave = reform(p1)
        f_nu_actual = reform(p2)
     end        
     else: begin
        helptext='No input given'
        needhelp=1
     end
  endcase

  if n_elements(relaxed) ne 0 then begin
     if relaxed eq 1 then relaxed=0.01d0
  endif else begin
     relaxed=0.0d0
  endelse

  ;; now build the list of filters to treat.
  ;; start with an empty list

  filters = {name:'',filename:'', referencewavelength:0d0, wavelengthunit:0d0 ,calibrationconvention:0d0, detectortype:""}

  if n_elements(filterdata) ne 0 then begin
     if size(filterdata,/tname) eq 'STRUCT' then begin
        if tag_exist(filterdata,'NAME') then begin
           tmpname = filterdata.name
        endif else begin
           message,/info,'Filterdata does not contain a name, using "FilterX"'
           tmpname='FilterX'
        endelse

        if tag_exist(filterdata,'FILENAME') then begin
           tmpfilename = filterdata.filename
        endif else begin
           message,/info,'Filterdata does not contain a filename'
           GOTO,RETURN_NAN
        endelse

        if tag_exist(filterdata,'REFERENCEWAVELENGTH') then begin
           tmpreferencewavelength = filterdata.referencewavelength
        endif else begin
           message,/info,'Filterdata does not contain a referencewavelength'
           GOTO,RETURN_NAN
        endelse

        if tag_exist(filterdata,'WAVELENGTHUNIT') then begin
           tmpwavelengthunit = filterdata.wavelengthunit
        endif else begin
           message,/info,'Filterdata does not contain a wavelengthunit, assuming 1e-6 (micrometer)'
           tmpwavelengthunit=1d-6
        endelse

        if tag_exist(filterdata,'CALIBRATIONCONVENTION') then begin
           tmpcalibrationconvention = filterdata.calibrationconvention
        endif else begin
           message,/info,'Filterdata does not contain a calibrationconvention, assuming -1: F_nu = nu^(-1)'
           tmpcalibrationconvention = -1.0
        endelse

        if tag_exist(filterdata,'DETECTORTYPE') then begin
           tmpcalibrationconvention = filterdata.detectortype
        endif else begin
           message,/info,'Filterdata does not contain a detectortype, assuming energycounter'
           tmpdetectortype = "ENERGYCOUNTER"
        endelse

        if not file_test(tmpfilename,/read) then begin
           if file_test(profilesdir+'/'+tmpfilename,/read) then begin
              tmpfilename=profilesdir+'/'+tmpfilename
           endif else begin
              message,/info,'File pointed to by Filterdata.filename cannot be read'
              GOTO,RETURN_NAN
           endelse
        endif

        tmpfilter = {name:tmpname,filename:tmpfilename, referencewavelength:tmpreferencewavelength, wavelengthunit:tmpwavelengthunit,calibrationconvention:tmpcalibrationconvention,detectortype:tmpdetectortype}
        filters = [filters,tmpfilters]

     endif
  endif
  
  ;; predefined filters (take from http://svo2.cab.inta-csic.es/theory/fps)
  IRAS_12 = {name:'IRAS_12',   filename:profilesdir+'/'+'IRAS_IRAS.12mu.dat' , referencewavelength:c/25d12, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  IRAS_25 = {name:'IRAS_25',   filename:profilesdir+'/'+'IRAS_IRAS.25mu.dat' , referencewavelength:c/12d12, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  IRAS_60 = {name:'IRAS_60',   filename:profilesdir+'/'+'IRAS_IRAS.60mu.dat' , referencewavelength:c/5d12 , wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  IRAS_100 = {name:'IRAS_100', filename:profilesdir+'/'+'IRAS_IRAS.100mu.dat', referencewavelength:c/3d12 , wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}

  ;; http://iopscience.iop.org/1538-3881/140/6/1868
  ;; note that it is really a photon counter but the RSR if build as a
  ;; energy counter
  WISE_1 = {name:'WISE_1', filename:profilesdir+'/'+'WISE_WISE.W1.dat', referencewavelength:3.3526d-6, wavelengthunit:1d-10,calibrationconvention:-2d0, detectortype: "ENERGYCOUNTER"}
  WISE_2 = {name:'WISE_2', filename:profilesdir+'/'+'WISE_WISE.W2.dat', referencewavelength:4.6028d-6, wavelengthunit:1d-10,calibrationconvention:-2d0, detectortype: "ENERGYCOUNTER"}
  WISE_3 = {name:'WISE_3', filename:profilesdir+'/'+'WISE_WISE.W3.dat', referencewavelength:11.5608d-6, wavelengthunit:1d-10,calibrationconvention:-2d0, detectortype: "ENERGYCOUNTER"}
  WISE_4 = {name:'WISE_4', filename:profilesdir+'/'+'WISE_WISE.W4.dat', referencewavelength:22.0883d-6, wavelengthunit:1d-10,calibrationconvention:-2d0, detectortype: "ENERGYCOUNTER"}

  ;; http://iopscience.iop.org/1538-3881/126/2/1090/pdf/203020.web.pdf
  TWOMASS_J = {name:'2MASS_J', filename:profilesdir+'/'+'2MASS_2MASS.J.dat', referencewavelength:1.2350d-6, wavelengthunit:1d-10,calibrationconvention:1.55d0, detectortype: "ENERGYCOUNTER"}
  TWOMASS_H = {name:'2MASS_H', filename:profilesdir+'/'+'2MASS_2MASS.H.dat', referencewavelength:1.6620d-6, wavelengthunit:1d-10,calibrationconvention:1.55d0, detectortype: "ENERGYCOUNTER"}
  TWOMASS_Ks = {name:'2MASS_Ks', filename:profilesdir+'/'+'2MASS_2MASS.Ks.dat', referencewavelength:2.1590d-6, wavelengthunit:1d-10,calibrationconvention:1.55d0, detectortype: "ENERGYCOUNTER"}

  ;; verified with
  ;; http://www.ir.isas.jaxa.jp/AKARI/Observation/PSC/Public/RN/AKARI-FIS_BSC_V1_RN.pdf and
  ;; http://www.sciops.esa.int/SA/ASTROF/docs/IRC_IDUM_1.3.pdf
  AKARI_N2    = {name:'AKARI_N2', filename:profilesdir+'/'+'AKARI_IRC.N2.dat'  , referencewavelength:2.4d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_N3    = {name:'AKARI_N3', filename:profilesdir+'/'+'AKARI_IRC.N3.dat'  , referencewavelength:3.2d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_N4    = {name:'AKARI_N4', filename:profilesdir+'/'+'AKARI_IRC.N4.dat'  , referencewavelength:4.1d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_S7    = {name:'AKARI_S7', filename:profilesdir+'/'+'AKARI_IRC.S7.dat'  , referencewavelength:7.0d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_S9W   = {name:'AKARI_S9W', filename:profilesdir+'/'+'AKARI_IRC.S9W.dat', referencewavelength:9d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_S11   = {name:'AKARI_S11', filename:profilesdir+'/'+'AKARI_IRC.S11.dat', referencewavelength:11d-6 , wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_L15   = {name:'AKARI_L15', filename:profilesdir+'/'+'AKARI_IRC.L15.dat', referencewavelength:15d-6 , wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_L18W  = {name:'AKARI_L18W', filename:profilesdir+'/'+'AKARI_IRC.L18W.dat', referencewavelength:18d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_L24   = {name:'AKARI_L24', filename:profilesdir+'/'+'AKARI_IRC.L24.dat', referencewavelength:24d-6 , wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_N60   = {name:'AKARI_N60', filename:profilesdir+'/'+'AKARI_FIS_N60.dat', referencewavelength:65d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_N160  = {name:'AKARI_N160', filename:profilesdir+'/'+'AKARI_FIS_N160.dat', referencewavelength:160d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_WIDES = {name:'AKARI_WIDES', filename:profilesdir+'/'+'AKARI_FIS.WIDE-S.dat', referencewavelength:90d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  AKARI_WIDEL = {name:'AKARI_WISEL', filename:profilesdir+'/'+'AKARI_FIS.WIDE-L.dat', referencewavelength:140d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}

  ;; http://irsa.ipac.caltech.edu/data/MSX/docs/MSX_psc_es.pdf
  MSX_A   = {name:'MSX_A', filename:profilesdir+'/'+'MSX_MSX.A.dat', referencewavelength: 8.28d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  MSX_C   = {name:'MSX_C', filename:profilesdir+'/'+'MSX_MSX.C.dat', referencewavelength:12.13d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  MSX_D   = {name:'MSX_D', filename:profilesdir+'/'+'MSX_MSX.D.dat', referencewavelength:14.65d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  MSX_E   = {name:'MSX_E', filename:profilesdir+'/'+'MSX_MSX.E.dat', referencewavelength:21.34d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
 
;  ;; see also http://irsa.ipac.caltech.edu/data/SPITZER/docs/dataanalysistools/cookbook/14
;  these seem to be reference wavelengths that are no longer used. In
;  SHA I think they use 3.6, 4.5, 5.8 and 8.0 http://irsa.ipac.caltech.edu/data/SPITZER/Enhanced/Imaging/docs/Spitzer_EIP_expsup.pdf
;  IRAC_1  = {name:'IRAC_1', filename:profilesdir+'/'+'Spitzer_IRAC.I1.dat', referencewavelength:3.5443331d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}
;  IRAC_2  = {name:'IRAC_2', filename:profilesdir+'/'+'Spitzer_IRAC.I2.dat', referencewavelength:4.4870185d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}
;  IRAC_3  = {name:'IRAC_3', filename:profilesdir+'/'+'Spitzer_IRAC.I3.dat', referencewavelength:5.7103728d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}
;  IRAC_4  = {name:'IRAC_4', filename:profilesdir+'/'+'Spitzer_IRAC.I4.dat', referencewavelength:7.8413115d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}

  ;; see also http://irsa.ipac.caltech.edu/data/SPITZER/docs/dataanalysistools/cookbook/14
  IRAC_1  = {name:'IRAC_1', filename:profilesdir+'/'+'Spitzer_IRAC.I1.dat', referencewavelength:3.6d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}
  IRAC_2  = {name:'IRAC_2', filename:profilesdir+'/'+'Spitzer_IRAC.I2.dat', referencewavelength:4.5d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}
  IRAC_3  = {name:'IRAC_3', filename:profilesdir+'/'+'Spitzer_IRAC.I3.dat', referencewavelength:5.8d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}
  IRAC_4  = {name:'IRAC_4', filename:profilesdir+'/'+'Spitzer_IRAC.I4.dat', referencewavelength:8.0d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "PHOTONCOUNTER"}

  ;; MIPS uses 10K bb as a calibration spectrum (F_nu ~ nu^2)
;  MIPS_24 = {name:'MIPS_24.dat' , filename:profilesdir+'/'+'Spitzer_MIPS.24mu.dat' , referencewavelength:24d-6, wavelengthunit:1d-10,calibrationconvention:2d0, detectortype: "ENERGYCOUNTER"}
;  MIPS_70 = {name:'MIPS_70.dat' , filename:profilesdir+'/'+'Spitzer_MIPS.70mu.dat' , referencewavelength:70d-6, wavelengthunit:1d-10,calibrationconvention:2d0, detectortype: "ENERGYCOUNTER"}
;  MIPS_160= {name:'MIPS_160.dat', filename:profilesdir+'/'+'Spitzer_MIPS.160mu.dat', referencewavelength:160d-6, wavelengthunit:1d-10,calibrationconvention:2d0, detectortype: "ENERGYCOUNTER"}

  MIPS_24 = {name:'MIPS_24.dat' , filename:profilesdir+'/'+'Spitzer_MIPS.24mu.dat' , referencewavelength:23.68d-6, wavelengthunit:1d-10,calibrationconvention:2d0, detectortype: "ENERGYCOUNTER"}
  MIPS_70 = {name:'MIPS_70.dat' , filename:profilesdir+'/'+'Spitzer_MIPS.70mu.dat' , referencewavelength:71.42d-6, wavelengthunit:1d-10,calibrationconvention:2d0, detectortype: "ENERGYCOUNTER"}
  MIPS_160= {name:'MIPS_160.dat', filename:profilesdir+'/'+'Spitzer_MIPS.160mu.dat', referencewavelength:155.9d-6, wavelengthunit:1d-10,calibrationconvention:2d0, detectortype: "ENERGYCOUNTER"}

  DENIS_I    = {name:'DENIS_I' , filename:profilesdir+'/'+'DENIS_DENIS.I.dat' , referencewavelength:0.793d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  DENIS_J    = {name:'DENIS_J' , filename:profilesdir+'/'+'DENIS_DENIS.J.dat' , referencewavelength:1.235d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  DENIS_Ks   = {name:'DENIS_Ks', filename:profilesdir+'/'+'DENIS_DENIS.Ks.dat', referencewavelength:2.160d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}

  GALEX_FUV   = {name:'GALEX_FUV', filename:profilesdir+'/'+'GALEX_GALEX.FUV.dat', referencewavelength:1528d-10, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  GALEX_NUV   = {name:'GALEX_NUV', filename:profilesdir+'/'+'GALEX_GALEX.NUV.dat', referencewavelength:2271d-10, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}

  PACS_blue    = {name:'PACS_blue' , filename:profilesdir+'/'+'Herschel_Pacs.blue.dat' , referencewavelength: 70d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  PACS_green   = {name:'PACS_green', filename:profilesdir+'/'+'Herschel_Pacs.green.dat', referencewavelength:100d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  PACS_red     = {name:'PACS_red'  , filename:profilesdir+'/'+'Herschel_Pacs.red.dat'  , referencewavelength:160d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}

  SPIRE_PSW   = {name:'SPIRE_PSW', filename:profilesdir+'/'+'Herschel_SPIRE.PSW.dat', referencewavelength:250d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  SPIRE_PMW   = {name:'SPIRE_PMW', filename:profilesdir+'/'+'Herschel_SPIRE.PMW.dat', referencewavelength:350d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}
  SPIRE_PLW   = {name:'SPIRE_PLW', filename:profilesdir+'/'+'Herschel_SPIRE.PLW.dat', referencewavelength:500d-6, wavelengthunit:1d-10,calibrationconvention:-1d0, detectortype: "ENERGYCOUNTER"}

  if keyword_set(all) then begin
     akari=1
     denis=1
     galex=1
     irac=1
     iras=1
     mips=1
     msx=1
     pacs=1
     spire=1
     twomass=1
     wise=1
  endif

  if keyword_set(spitzer) then begin
     irac=1
     mips=1
  endif

  if keyword_set(herschel) then begin
     pacs=1
     spire=1
  endif

  if keyword_set(galex) then filters = [filters,galex_fuv,galex_nuv]
  if keyword_set(denis) then filters = [filters,denis_i,denis_j,denis_ks]
  if keyword_set(twomass) then filters = [filters,twomass_j,twomass_h,twomass_ks]
  if keyword_set(irac) then filters = [filters,irac_1,irac_2,irac_3,irac_4]
  if keyword_set(msx) then filters = [filters,msx_a,msx_c,msx_d,msx_e]
  if keyword_set(wise) then filters = [filters,wise_1,wise_2,wise_3,wise_4]
  if keyword_set(iras) then filters = [filters,iras_12,iras_25,iras_60,iras_100]
  if keyword_set(mips) then filters = [filters,mips_24,mips_70,mips_160]
  if keyword_set(pacs) then filters = [filters,pacs_blue,pacs_green,pacs_red]
  if keyword_set(spire) then filters = [filters,spire_psw,spire_pmw,spire_plw]
  if keyword_set(akari) then filters = [filters,akari_n2,akari_n3,akari_n4,akari_s7,akari_s9w,akari_s11,akari_l15,akari_l18w,akari_l24,akari_n60,akari_n160,akari_wides,akari_widel]

  if n_elements(filters) eq 1 then begin
     message,/info,'no filters have been defined'
     
     tmpname = ''
     print,'Enter the NAME of the filter to be calculated'
     read,tmpname
     
     tmpreferencewavelength = 0d0
     print,'Enter the REFERENCE WAVELENGTH in meters'
     read,tmpreferencewavelength
     
     READFILENAME: tmpfilename = ''
     print,'Enter the Filename which contains the filter throughput data'
     print,'Absolute path or I will look in the current dir and in '+profilesdir
     print,'q will exit'
     read,tmpfilename
     if strupcase(tmpfilename) eq 'Q' then GOTO,RETURN_NAN
     if not file_test(tmpfilename,/read) then begin
        if file_test(profilesdir+'/'+tmpfilename,/read) then begin
           tmpfilename=profilesdir+'/'+tmpfilename
        endif else begin
           message,/info,'File pointed to by Filterdata.filename cannot be read'
           GOTO, READFILENAME
        endelse
     endif

     tmpwavelengthunit = ''
     print,'Enter the UNIT of the wavelengths given in the filter profile table (default 1d-10)'
     read,tmpwavelengthunit
     if tmpwavelengthunit eq '' then tmpwavelengtunit=1d-10 else tmpwavelengthunit=double(tmpwavelengthunit)
     
     tmpcalibrationconvention = ''
     print,'Enter the CALIBRATION CONVENTION (default -1, ie F_nu ~ nu^-1)'
     read,tmpcalibrationconvention
     if tmpcalibrationconvention eq '' then tmpcalibrationconvention=-1d0 else tmpcalibrationconvention=double(tmpcalibrationconvention)

     tmpdetectortype = ''
     print,'Enter the DECTECTOR TYPE  (E(nergy)counter or Photo(counter) (default E)'
     read,tmpdetectortype
     if tmpdetectortype eq '' then tmpdetectortype="ENERGYCOUNTER"
     case strupcase(strmid(tmpdetectortype,0,1)) of
        'P': tmpdetectortype="PHOTONCOUNTER"
        else: tmpdetectortype="ENERGYCOUNTER"
     endcase

     tmpfilter = {name:tmpname,filename:tmpfilename, referencewavelength:tmpreferencewavelength, wavelengthunit:tmpwavelengthunit,calibrationconvention:tmpcalibrationconvention,detectortype:tmpdetectortype}
     filters = [filters,tmpfilter]
     
  endif else begin
     filters=filters[1:*]
  endelse
  
  if needhelp then begin
     print,'********************************************'
     print,helptext
     print,'********************************************'
     doc_library,'sed2phot'
     GOTO,RETURN_NAN
  endif

  if keyword_set(plot) then begin
     plot,wave,f_nu_actual,xtit='wavelength',ytit='Flux',/xstyle,/ystyle,_extra=_extra
  endif
  
  ;; now start the real work
  ;; to hold the outputs
  nfilters = n_elements(filters)

  isvalid = make_array(nfilters,value=0)
  outname = make_array(nfilters,value='')
  outwave = make_array(nfilters,value=0d0)
  outflux = make_array(nfilters,value=0d0)
  outcolc = make_array(nfilters,value=0d0)

  ;; sort along the wavelengths
  idx = sort(wave)
  wave = wave[idx]
  min_wave = min(wave,max=max_wave)
  f_nu_actual = f_nu_actual[idx]

  for ff = 0,nfilters-1 do begin
     filter=filters[ff]
     
     if file_test(filter.filename,/read) then begin
        respdata  = (read_ascii(filter.filename,comment='#')).(0)
        Rwave = reform(respdata[0,*]) * filter.wavelengthunit / 1d-6 ;; um
        R = reform(respdata[1,*])
     endif else begin
        message,/info,'Could not read '+filter.filename
        continue
     endelse
     
     ;; number of photons is proportial to the flux-density * wavelength
     if filter.detectortype eq "PHOTONCOUNTER" then begin
        R = R * (Rwave/(filter.referencewavelength * filter.wavelengthunit / 1d-6))
        R = R / max(R)
     endif

     nRwave = n_elements(Rwave)
     min_Rwave = min(Rwave,max=max_Rwave)
     
     min_required_Rwave = min_Rwave
     max_required_Rwave = max_Rwave

;; allow to be less stringent on the wavelength coverage (ie AKARI N60)
     idx = where(R gt max(R)*relaxed,cnt)
     if cnt gt 0 then begin
        min_required_Rwave = min(Rwave[idx],max=max_required_Rwave)
     endif
           
     if (min_wave gt min_required_Rwave) or (max_wave lt max_required_Rwave) then begin
        message,/info,'The input data does not cover the filter: '+filter.name
        message,/info,'Skipping '+filter.name
        continue
     endif

     ;; resample on the fine grid (linear)
     npoints = max([201,nRwave])
; log scale:
; fine_wave = min_Rwave*10d0^(alog10(max_Rwave/min_Rwave)*dindgen(npoints)/(npoints -1L))
     fine_wave  = min_Rwave+(max_Rwave-min_Rwave)*dindgen(npoints)/(npoints -1L)
     fine_R = interpol(R,Rwave,fine_wave)
     fine_f_nu_actual = interpol(f_nu_actual,wave,fine_wave)

     ;; we follow very closely the notation at
     ;; http://lambda.gsfc.nasa.gov/product/iras/docs/exp.sup/ch6/C3.html
     lambda_0  = filter.referencewavelength/1d-6 ;; um

     f_nu_0_actual  = interpol(fine_f_nu_actual,fine_wave,lambda_0 )
     ;; this is the calibration spectrum shape normalised to
     ;; the referencewavelength (nu^p)
     fine_f_nu_quoted  = (lambda_0/fine_wave)^(filter.calibrationconvention)
     
     f_nu_0_quoted  = sed2phot_integrate(fine_wave,fine_f_nu_actual *fine_R )  / sed2phot_integrate(fine_wave,fine_f_nu_quoted *fine_R )

     K  = f_nu_0_quoted /f_nu_0_actual
     
     isvalid[ff] = 1
     outname[ff] = filter.name
     outwave[ff] = lambda_0
     outflux[ff] = f_nu_0_quoted
     outcolc[ff] = K

     if keyword_set(plot) then begin
        thiscolor = colors[ff mod ncolors]
        oplot,[min_Rwave,max_Rwave],[f_nu_0_quoted,f_nu_0_quoted],_extra=_extra,color=thiscolor
        oplot,[lambda_0],[f_nu_0_quoted],ps=4,thick=3,_extra=_extra,color=thiscolor
        oplot,Rwave,R*f_nu_0_quoted,ps=0,linestyle=2,_extra=_extra,color=thiscolor
     endif

  endfor

  idx = where(isvalid,cnt)
  if cnt ne 0 then begin
     outname = outname[idx]
     outwave = outwave[idx]
     outflux = outflux[idx]
     outcolc = outcolc[idx]
  endif else begin
     message,/info,'No valid data photometry points have been calculated'
     GOTO,RETURN_NAN
  endelse

  colorcorrections={filter:outname,colorcorrection:outcolc}

  if not keyword_set(quiet) then begin
     print,'Color correction values:'
     print,transpose([[outname],[string(outcolc,format='(F7.3)')]])
  endif

  !QUIET = oldquiet
  return,transpose([[outwave],[outflux]])

  RETURN_NAN:
  !QUIET = oldquiet
  return,[[!values.d_nan],[!values.d_nan]]

END
