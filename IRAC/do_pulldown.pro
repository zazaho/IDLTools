FUNCTION lmode, data
   
; Naive script to determine the mode, only take the first 
; in case of double-valued result...
   
   hist = histogram(data) 
   ifin = n_elements(hist)
   xvals = findgen(ifin)+min(data)+0.5
   xvals = xvals(where(hist eq max(hist)))
   return, xvals(0)
   
END 

PRO literstat, data, out, nsigrej=nsigrej,$
               maxiter=maxiter, lower=lower, upper=upper, silent=silent
   
; Program based on dimsum's iterstat:
; Created lam98jan18ucb
; 
; Converted to idl, and made output into a structure.
; 
; procedure iterstat(image)
; 
; # Script to find image statistics excluding deviant pixels
; # 4 August 1992 by John Ward
; # Minor modifications 4 August 1992 MD
; # Various subsequent variations.
; # Latest revision:  18 Aug 1993 MD
;
   
   IF n_params() LE 0 THEN BEGIN 
      print,' '
      print,'literstat, data, out, [nsigrej=], [maxiter=],'
      print,'           [lower=], [upper=], [silent]'
      print,' '
      print,'  N.b. -- The output array ''out'' is a structure:'
      print,'    out.NPIX .MEAN .MEDIAN .SIGMA .MODE .MIN .MAX'
      print,' '
      return 
   ENDIF 
   
   out =  create_struct('Npix',0.,$
                        'Mean',0.,$
                        'Median',0.,$
                        'Sigma',0.,$
                        'Mode',0.,$
                        'min',0.,$
                        'max',0.)
   
   IF keyword_set(nsigrej) EQ 0. THEN nsigrej=5. ; N * sigma for limits
   IF keyword_set(maxiter) EQ 0  THEN maxiter=10 ; Max # of iterations
   IF NOT keyword_set(lower) THEN lower=min(data[where(finite(data) EQ 1)])
   IF NOT keyword_set(upper) THEN upper=max(data[where(finite(data) EQ 1)])
   
   data = float(data)           ; Verify that data are floated
   sz = size(data)
   npx = sz(n_elements(sz)-1)   ; Number of relevant pixels
;   lind = lindgen(npx)
   lind = where(data GE lower AND data LE upper)
;;;   data = data(lind)
;    IF n_elements(where(finite(data) EQ 1)) NE n_elements(data) THEN BEGIN 
;       print,'There are NaNs in the array'
;       lind = where(finite(data) EQ 1)
;    ENDIF 
   mn  =  mean  (data(lind))    ; Mean
   sig =  stddev (data(lind))    ; Standard Deviation
   med =  median(data(lind))    ; Median
   mde =  lmode (data(lind))    ; Mode, cf 'lmode' below
   IF NOT keyword_set(silent) THEN print, ' '
   IF NOT keyword_set(silent) THEN print,'    Iter       Npix        Mean         Sigma        Median       Mode'

   Tpx =  0
   m = 1
   WHILE (m LE maxiter AND Tpx NE npx) DO BEGIN 
      npx = Tpx
      ll =  mn - (nsigrej*sig)
      ul =  mn + (nsigrej*sig)
      IF (ll NE min(data(lind)) AND ll LE lower) THEN ll=lower
      IF (ul NE max(data(lind)) AND ul GE upper) THEN ul=upper
      lind = where(data GE ll AND data LE ul)
      IF total(lind) EQ -1 THEN BEGIN 
         print,'Error -- all the pixels masked...'
         return
      ENDIF 
      Tpx =  n_elements(lind)   ; Number of relevant pixels
      mn  =  mean  (data(lind)) ; Mean
      sig =  stddev (data(lind)) ; Standard Deviation
      med =  median(data(lind)) ; Median
      mde =  lmode (data(lind)) ; Mode, cf 'lmode' below
      IF NOT keyword_set(silent) THEN print,m,tpx,mn,sig,med,mde
      m = m+1
   ENDWHILE 
   
   IF NOT keyword_set(silent) THEN print, ' '
   
   out.npix = npx
   out.mean = mn
   out.median = med
   out.sigma = sig
   out.mode = mde
   out.min = ll
   out.max = ul
   
   IF NOT keyword_set(silent) THEN help,/structure,out
   
END 

;+
; NAME:
;       FPULLDOWN
;
; PURPOSE:
;       Fixes the 'pulldown' effect in IRAC images
;
; INPUTS:
;       A 2D image array
;
; KEYWORD PARAMETERS:
;
;       TITLE      - prefix to output fits name; default='fp_'
;       OBJCLIP    - Nsigma threshold for masking objects
;       BOXCARSIZE - Boxcar size for smoothing in creating mask
;       HSIG       - high sigma threshold over projected median
;       LSIG       - low sigma threshold over projected median
;       NOFOWLER   - ignore fowler-based tweak, apply single value 
;       DISPLAY    - switch; display resulting image
;       WRITE      - switch; write out resulting fits image+header
;       HELP       - switch; returns quick keyword summary
;       VERBOSE    - switch; give extra information
;
; OUTPUTS:
;       Returns pulldown-corrected 2D arrays, optionally writing out
;       corrected fits image, and/or displaying to window.
;
; COMMON BLOCKS:
;       None.
;
; RESTRICTIONS:
;       Does not work for Channel 4 (yet), returns -1 in that case. 
;
; PROCEDURE:
;       fim = fpulldown(image.fits, TITLE = title, $
;             OBJCLIP = objclip, BOXCARSIZE = boxcarsize,$
;             HSIG = hsig, LSIG = lsig, $
;             /NOFOWLER, /DISPLAY, /WRITE, /HELP, /VERBOSE)
;
; COMMENTS:
;
;       An outline of the algorithm is:
;
;   1. Create an unsharp mask of the image, by boxcar-smoothing the
;   original image by a small kernel
;
;   2. Create a copy of the original image, and replace all 'object'
;   pixels (determined in step 1) by a NAN value
;
;   3. For each column, calculate the object-masked median value,
;   generating a projected median-value row.
;
;   4. Subtract the projected median-averaged row by its own median
;   value, and calculate its statistics.
;
;   5. Using the specified HSIG and LSIG thresholds, identify the
;   deviant columns, and add in the differential correction, possibly
;   with Fowler-based weighting.
;
;   6. Return the corrected image, possibly writing out a new fits
;   image, and/or displaying to screen.
;
; PROCEDURES USED:
;       LITERSTAT
; 
; MODIFICATION HISTORY:
;       LAM '03sep22 - written, with input from DS & MED
;       LAM '03sep23 - modified input method, added Fowler-based
;                      correction of DS
;       LAM '03sep25 - corrected header name identifying Channel
;   
;       MDL '04jan02 - fixed bug which left NaNs in output images
;       LAM '04feb28 - changed input method to image+header
;-
function fpulldown, image, header, $
                    OBJCLIP = objclip,       $
                    BOXCARSIZE = boxcarsize, $
                    HSIG = hsig,             $
                    LSIG = lsig,             $
                    NOFOWLER = nofowler,     $
                    DISPLAY = display,       $
                    WRITE = write,           $
                    TITLE = title,           $
                    HELP = help,             $
                    VERBOSE = verbose

     if n_elements(help) ne 0 then begin
         print,' '
         print,'fim = fpulldown(image, header, TITLE = title, $'
         print,'      OBJCLIP = objclip, BOXCARSIZE = boxcarsize,$'
         print,'      HSIG = hsig, LSIG = lsig, $'
         print,'      /NOFOWLER, /DISPLAY, /WRITE, /HELP, /VERBOSE)'
         print,' '
         return,-1
     endif

     if n_elements(image) eq 0 then begin
         message,'no image provided'
         return,-1
     endif
     
     if fix(sxpar(header,'ACHANID')) eq 4 then begin
         message,'skipping channel 4 data'
         return,image
     endif
     
     if n_elements(BOXCARSIZE) eq 0 then BOXCARSIZE = 2.5; 1.5; 5.0
     if n_elements(OBJCLIP)    eq 0 then OBJCLIP = 7.0
     if n_elements(HSIG)       eq 0 then HSIG = 5. ;2.5
     if n_elements(LSIG)       eq 0 then LSIG = 3. ;1.7

;   1. Create an unsharp mask of the image, by boxcar-smoothing the
;   original image by a small kernel

     sz=size(image,/dimen)

     maskim = bytarr(sz[0],sz[1])

     sim=smooth(image,boxcarsize,/nan)
     literstat,sim,simst,/silent
     id=where(sim gt simst.median+OBJCLIP*simst.sigma)

;   2. Create a copy of the original image, and replace all 'object'
;   pixels (determined in step 1) by a NAN value

     tim=image
; fixed WTR
     if id(0) ne -1 then tim[id]=!values.f_nan
     
;   3. For each column, calculate the object-masked median value,
;   generating a projected median-value row.

     projval=fltarr(sz[0])
     for i=0,sz[0]-1 do projval[i] = median(tim[i,*])

;   4. Subtract the projected median-averaged row by its own median
;   value, and calculate its statistics.

     medpval = median(projval)
     projval = projval - medpval
     literstat,projval,pst,/silent

;   5. Using the specified HSIG and LSIG thresholds, identify the
;   deviant columns, and calculate the Fowler-based correction.

     hthresh = 0.0 + HSIG*pst.sigma
     lthresh = 0.0 - LSIG*pst.sigma

; default: apply the fowler-weighted correction
     if n_elements(nofowler) eq 0 then begin 
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; n pixels above, m pixels below, fowler sampling FN
; pn = pulldown amount above, pm = pulldown amount below
;
; code calculates what correction pmp is, assuming correction is
; constant over entire column
;
; n*(FN-1)/FN*pm + m*pm = (n+m)*pmp
;
; pm*(n*(FN-1)/FN+m) = (n+m)*pmp
;
;-------------------------------------
;|  pm = (m+n) / (m+(FN-1)/FN*n) * pmp|
;|                                    |
;|  pn = (FN-1)/FN * pm               |
;-------------------------------------
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

         FN = float(sxpar(header,'AFOWLNUM')) ; Fowler number
         if n_elements(verbose) ne 0 then $
            print,'FPULLDOWN: Fowler number '+strcompress(fix(FN))
         fpix = where(projval lt lthresh or projval gt hthresh)
         if total(fpix) ne -1 then begin
             timmask=tim
             tim=image
             for i=0,n_elements(fpix)-1 do begin
; the value pmp
                 pmp = projval[fpix[i]]
; the location of the brightest (unmasked) pixel in the original image 
                 brpix = (where(image[fpix[i],*] eq max(image[fpix[i],*])))[0]
; the relevant numbers of pixels above & below brpix from the masked image
                 m = n_elements(where(finite(timmask[fpix[i],0:(brpix-1>0)]) eq 1)) ; below
                 n = n_elements(where(finite(timmask[fpix[i],brpix:(sz[1]-1)]) eq 1)) ; above
                 pm = ( (m+n) / (m+(FN-1)/FN*n) ) * pmp
                 pn = (FN-1)/FN * pm
                 
                 if n_elements(verbose) ne 0 then $
                   print,string(fpix[i] ,format='(i3.3)')+' '+$
                         string(pmp     ,format='(f8.2)')+' '+$
                         string(brpix   ,format='(i3.3)')+' '+$
                         string(m       ,format='(i3.3)')+' '+$
                         string(n       ,format='(i3.3)')+' '+$
                         string(pm      ,format='(f8.2)')+' '+$
                         string(pn      ,format='(f8.2)')+' '+$
                         string( (m*pm+n*pn)/(m+n) ,format='(f8.2)')
                 
                 tim[fpix[i],0:(brpix-1>0)]   = tim[fpix[i],0:(brpix-1>0)]   - pm
                 tim[fpix[i],brpix:(sz[1]-1)] = tim[fpix[i],brpix:(sz[1]-1)] - pn
                 maskim[fpix[i],*] = 1b
             endfor
; if there are no columns to correct restore the original image
         endif else begin
         tim = image
        endelse
     endif else begin ; case where not worrying about fowler-based correction
         if n_elements(verbose) ne 0 then print,'FPULLDOWN: Single-value subtraction'
         fpix = where(projval lt lthresh or projval gt hthresh)
         if total(fpix) ne -1 then begin 
             tim = image
             for i=0,n_elements(fpix)-1 do begin
                 tim[fpix[i],*]=tim[fpix[i],*]-projval[fpix[i]]
                 maskim[fpix[i],*] = 1b
             endfor
         endif
     endelse
         
;   6. Possibly output and return the corrected image.

     if n_elements(display) ne 0 then begin
         literstat,tim,timst,/silent
         loadct,0
         tvlct,r,g,b,/get
         tvlct,reverse(r),reverse(g),reverse(b)
         plotimage,bytscl(tim,$
                          min=timst.median-   timst.sigma,$
                          max=timst.median+5.*timst.sigma),$
                   /preserve
     endif

     sxaddhist,'pulldown effect corrected',header

     return,{fpc: tim, head: header, mask: maskim}
 end


pro do_pulldown,filelist

; takes a file list and runs the pulldown correction

filelist = strcompress(filelist,/remove_all)

readcol,filelist,file,format='a'

nfiles = n_elements(file)

for i=0,nfiles-1 do begin

m=readfits(file[i],h,/silent)
outstr = fpulldown(m,h,verbose=1,hsig=5,lsig=3,objclip=7.0)

p=strpos(file[i],'bcd_fp')
filenew=strmid(file[i],0,p)+'bcd_fpc.fits'
filenewmask=strmid(file[i],0,p)+'bcd_fpm.fits'
writefits,filenew,outstr.fpc,outstr.head
writefits,filenewmask,byte(outstr.mask)
endfor

end
