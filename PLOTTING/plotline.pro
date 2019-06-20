;From peeters@poseidon.sron.rug.nlTue Jun  9 13:51:11 1998
;Date: Tue, 9 Jun 1998 11:53:40 +0200 (MET DST)
;From: Els Peeters <peeters@poseidon.sron.rug.nl>
;To: hony@astro.rug.nl
;Subject: plotline.pro

pro plotline, aar_in, line_in, wl=wl, fl=fl, sig=sig

; plotline.pro: plots all 12 detectors for a specific line of an AAR. For
;	AOT1's the up and down scans are separately plotted (despite that
;	they have same line number). Positive numbers are down scans,
;	negative numbers up scans.
;
;
; CALLING:	plotline, aar	     AAR	: INPUT AAR
;			, [..,..,]   FLT(N)	: line number(s) to be plotted
;			OPTIONAL INPUT:
;			, wl=[..,..] FLT(2)	: wavelength range to plot
;						 [default: full range]
;			, fl=[..,..] FLT(2)	: flux range to to plot
;						 [default: full range]
;			, /sig	     KWD	: do sigma clip before
;					determining sigma in each detector
;
;
;
; v0.0 24/12/97 A.C.A. Boogert: created for band 2
; v0.1 26/01/98 A.C.A. Boogert: applicable for all bands
; v0.2 17/05/98 A.C.A. Boogert: *plot up and down separately for AOT1's,
;			since by default they have same line number
;				*give stop sign after each line is plotted, and
;			don't stop after last line is plotted
;				*gave input keyword /sig to allow sigma 
;			clipping before determining sigma in each detector
;			Default is to not do sigma clipping, because costs
;			too much time
;

!x.style=1
!y.style=1
!x.margin=2
!x.omargin=10
!y.margin=2.3
!y.omargin=5
!p.charsize=2
!x.ticks=3
IF keyword_set(wl) THEN !x.range=wl
IF keyword_set(fl) THEN !y.range=fl

!p.multi=[0,4,3]

a=aar_in

object=STRMID(a.header(0,0),1050,17)

w=a.data.wave
f=a.data.flux
d=a.data.det
l=a.data.line
s=a.data.sdir

IF STRMID(a.header(0,0),1531,3) EQ 'S01' THEN BEGIN
 ; don't modify line number if input aar is just up or just
 ; down scan
 IF n_elements(uniq((s)(sort(s)))) EQ 2 THEN BEGIN
  l=l*s
  line_in=[line_in, -line_in]
  a.data.line=l
 ENDIF ELSE BEGIN
  print, '**input AAR just up or down scan'
 ENDELSE
ENDIF

FOR z=0,(n_elements(line_in)-1) DO BEGIN
 index=where(l EQ line_in(z))
 wl1=w(index)
 fl=f(index)
 dl=d(index)
 wlmed=median(wl1)
 IF wlmed LT 4.1 THEN BEGIN
  band=1
  det0=1
 ENDIF
 IF (wlmed GT 4.1 AND wlmed LE 12.) THEN BEGIN
  band=2
  det0=13
 ENDIF
 IF (wlmed GT 12. AND wlmed LE 29.) THEN BEGIN
  band=3
  det0=25
 ENDIF
 IF (wlmed GT 29. AND wlmed LE 45.) THEN BEGIN
  band=4
  det0=37
 ENDIF
; n=sws_detnoise(a,band,line=line_in(z),sig=sig,wl=wl)
 n=[1,1,1,1,1,1,1,1,1,1,1,1]
 FOR i=1,12 DO BEGIN
  nt=STRMID(STRING(n(i-1)),6,3)
  plot, wl1(where(dl EQ (i+det0-1))), fl(where(dl EQ (i+det0-1))), $
	title='DET='+STRMID(STRING(i+det0-1),6,2)+' !7r!5='+nt
 ENDFOR
 xyouts, 0.12, 0.94, '!5line'+STRING(line_in(z))+' of '+ object,/normal, charsize=3
 IF z NE (n_elements(line_in)-1) THEN BEGIN
  print, '***Type .c to plot next line'
;  stop
 ENDIF
ENDFOR

end
