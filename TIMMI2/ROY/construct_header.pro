function construct_header,head0,head2

  ;;trick from Ralph Siebenmorgen to be able to read fits header
  ;;values with non standard (i.e. > 8 characters) keywords.
  ;;output values are strings.

   keyname= 'HIERARCH ESO TEL AIRM START  '
  res     = strpos(head2,keyname)
  dummy   = strnumber(strn(strmid(head2(where(res eq 0)), 34,9)),airmass)
   keyname= 'HIERARCH ESO PRO REDU XPECT MINOR'
  res     = strpos(head0,keyname)
  dummy   = strnumber(strn(strmid(head0(where(res eq 0)), 35,5)),N_nod_ex)
   keyname= 'HIERARCH ESO OBS MAJOR'
  res     = strpos(head0,keyname)
  dummy   = strnumber(strn(strmid(head0(where(res eq 0)), 25,8)),N_major)
   keyname= 'HIERARCH ESO INS OPTI2 NAME'
  res     = strpos(head2,keyname)
  slitname= strn(strmid(head2(where(res eq 0)), 31,20))
   keyname= 'HIERARCH ESO DET DIT'
  res     = strpos(head2,keyname)
  dummy   = strnumber(strn(strmid(head2(where(res eq 0)), 24,11)),def_time)
   keyname= 'HIERARCH ESO PRO REDU XPECT VIDEO'
  res     = strpos(head2,keyname)
  dummy   = strnumber(strn(strmid(head2(where(res eq 0)), 40,11)),N_video)
 

  ;;keywords that are more easy to read since they are allready in correct format
  object=sxpar(head2,'OBJECT')
  RA=sxpar(head2,'RA')
  dec=sxpar(head2,'DEC')
  equinox=sxpar(head2,'EQUINOX')
  radecsys=sxpar(head2,'RADECSYS')
  date_obs=sxpar(head2,'DATE-OBS')
  mjd_ob=sxpar(head2,'MJD-OBS')
  telescop=sxpar(head2,'TELESCOP')
  instrume=sxpar(head2,'INSTRUME')
  utc=sxpar(head2,'UTC')
  lst=sxpar(head2,'LST')
  observer=sxpar(head2,'OBSERVER')
  pi_coi=sxpar(head2,'PI-COI')

  ;;make the actual header
  dummy=fltarr(1,1)        ;;initialisation
  sxhmake,dummy,1,headout  ;;of header

  fxaddpar,headout,'OBJECT  ',object,  'name of the target'
  fxaddpar,headout,'RA      ',RA,      'Right Ascension of target'
  fxaddpar,headout,'DEC     ',dec,     'declination of target'
  fxaddpar,headout,'EQUINOX ',equinox, 'equinox'
  fxaddpar,headout,'RADECSYS',radecsys,'coordinate system'
  fxaddpar,headout,'DATE-OBS',date_obs,'date of observation'
  fxaddpar,headout,'MJD-OBS ',mjd_ob,  'julian date of observation',format='(f13.7)'
  fxaddpar,headout,'UTC     ',utc,     'Universal Time of observation'
  fxaddpar,headout,'LST     ',lst,     'local siderial time at start of observation',format='(f13.7)'
  fxaddpar,headout,'INT-TIME',def_time,'user defined integration time'
  fxaddpar,headout,'AIRMASS ',airmass, 'airmass from 3.6 TCS'
  fxaddpar,headout,'N-MAJOR ',N_major, 'sequence number of exposure',format='(I0)'
  fxaddpar,headout,'N_NOD_EX',N_nod_ex,'expected number of nod positions',format='(I0)'
  fxaddpar,headout,'N_VID_EX',N_video, 'expected number of video frames per nod position',format='(I0)'
  fxaddpar,headout,'TELESCOP',telescop,'telescope'
  fxaddpar,headout,'INSTRUME',instrume,'instrument'
  fxaddpar,headout,'OBSERVER',observer,'observer'
  fxaddpar,headout,'PI-COI  ',pi_coi,  'principal investiagtor / co investigator'

  return,headout
end
