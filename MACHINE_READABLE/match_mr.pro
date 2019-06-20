;+
; NAME:
;  match_mr.pro
;
; PURPOSE:
;  Given two machine readable tables it will find positional (ra,dec)
;  coincidences and join the two tables where these coincidences occur
;  thus allowing to merge two tables with data from different sources 
;
; CATEGORY:
;  STRUCTURE MANIPULATION
;
; CALLING SEQUENCE:
;  matched = match_mr(tab1,tab2 
;     [,col1=col1,col2=col2,
;      multiple=multiple,keep1=keep1,keep2=keep2,keepall=keepall,
;      precess1=precess1,precess2=precess2,max_dist=max_dist,
;      shift=shift,sigma=sigma,distances=distances,/add_distance])
;
; INPUTS:
;  tab1,tab2: Machine readable tables that contain positions.
;
; KEYWORD PARAMETERS:
;  /help: if set show the help
;  col1,col2: array(s) of columns from the tables that should be used
;    as positions eq ['ra','dec'] or [[1,2,3],[12,13,14]] where the
;    latter indicates that [1,2,3] form the RA. Some special forms are
;    allowed: 
;    missing = ['RA','DEC'] (default)
;    'radec' = ['RA','DEC']
;    'deg' = ['RAdeg','DEdeg']
;    'sex' = [['RAh','RAm','RAs',''],['DE-','DEd','DEm','DEs']]
;    Note that [['RAh','RAm','RAs'],['DEdeg']] is not allowed by IDL.
;    Use [['RAh','RAm','RAs'],['DEdeg','','']] or
;    Use [[1,2,3],[25,-1,-1]] instead
;  precess1=precess1:
;   if set and equal to 1: the coordinates1 are transformed from 1950
;    to 2000 before matching.
;   if set and a single value: the coordinates1 are transformed from value
;    to 2000 before matching.
;   if set and a pair of values: the coordinates1 are transformed from value[0]
;    to value[1] before matching.
;  precess2=precess2:
;   if set and equal to 1: the coordinates2 are transformed from 1950
;    to 2000 before matching.
;   if set and a single value: the coordinates2 are transformed from value
;    to 2000 before matching.
;   if set and a pair of values: the coordinates2 are transformed from value[0]
;    to value[1] before matching.
;  max_dist = maximum distance (arcsec) to be considered a match (1 default)
;  /multiple: if set return all matches not only the closest one
;  /keep1: if set keep the records from table 1 even if they are not matched
;  /keep2: if set keep the records from table 2 even if they are not matched
;  /keepall: if set keep the records from table 1 and 2 even if they are not matched
;  /remove: if set remove the matching record in table 2 in order to clean 
;  /shift: if set determine the median shift (position angle and
;    distance) between the sources from table 2 towards table 1
;    and use this shift prior to doing the final matching.
;    sigma = sigma. The number of sigmas to from each other to be
;    considered a match
;  distances=optional array to return the distance for each match in
;  arcsec -1 means no match
;  /add_distance: if set add a column with the match distance
;
; EXAMPLE:
;  matched = match_mr(tab1,tabl)
;      or
;  matched = match_mr(tab1,tabl,col1='deg',col2=['ra','decli'])
;   
; MODIFICATION HISTORY:
;  Version 1:
;  (SH Jul 22 2005)
;  (SH Mar  4 2007) add remove option
;  (SH Apr  2 2008) added option to return the distances or add them
;  to the table
;  (SH Mar  7 2013) use external routin add_column_mr
;  (SH Mar  7 2013) generalise the precess option to be able to
;  precess the coordinates from A to B. A is 1950 by default and B is 2000.
;-

FUNCTION match_mr,tab1,tab2,col1=col1,col2=col2, $
                  max_dist=mdist,multiple=multi,keep1=keep1,keep2=keep2, $
                  keepall=keepall,shift=shift,sigma=sigma,help=help, $
                  precess1=precess1,precess2=precess2,remove=remove, $
                  distances=distances,add_distance=add_distance
  
;; Only print the usage info and return if asked for help
  IF keyword_set(help) THEN BEGIN
      doc_library,'match_mr'
      return,0
  ENDIF
  
;; some reasonable defaults for the keyword parameters  
  default,col1,['RA','DEC']
  default,col2,['RA','DEC']
  default,precess1,0
  default,precess2,0
  default,mdist,1d0 ;; arcsec
  default,multi,0
  default,add_distance,0
  
  default,keep1,0
  default,keep2,0
  IF keyword_set(keepall) THEN BEGIN
      keep1 = 1
      keep2 = 1
  endif

  default,shift,0
  default,sigma,0

  default,remove,0 ;; if set it will remove the matching sources from table2
  ;; this is made to clean a table of some given bad sources.

  ;; grab the coordinates from the table
  coord1 = coord_mr(tab1,col1,precess=precess1)
  ra1 = reform(coord1[0,*])
  de1 = reform(coord1[1,*])

  coord2 = coord_mr(tab2,col2,precess=precess2)
  ra2 = reform(coord2[0,*])
  de2 = reform(coord2[1,*])
  
  DO_MATCHING:
;; make a dummy array to hold the pair of matching indices
  
  iarr      = [-1,-1]
  ;; also allow to return the distances
  distances   = [-1]
  
  ;; The minimum of the cos(dec) occurs where dec is maximum. Use that
  ;; to estimate which sources may possibly match, instead of every
  ;; time calculating cos(de1[i])
  mincos = cos(!dpi/180d0*max(abs(de1)))
  mdistdeg = mdist/3600d0

  FOR i = 0L,n_elements(ra1)-1L DO BEGIN
      
      ;; Do a preselection in order to speed up the matching with big
      ;; tables. Just select the sources in a box around the sources
      ;; with sides of 2*max_dist
      subset = where( (abs(ra2-ra1[i])*mincos LE mdistdeg) AND $
                      (abs(de2-de1[i]) LE mdistdeg) ,count)

      ;; So there are some sources in this box ?
      IF count NE 0 THEN BEGIN
         ;; calculate the distance to each source in tab2 in arcsecs
         dist_subset = sqrt( ((ra2[subset]-ra1[i])*cos(!dpi/180d0*(de2[subset]+de1[i])/2d0))^2d0+ $
                             (de2[subset]-de1[i])^2d0 )
         ;; find the matching source
         matches_subset = where(dist_subset LE mdistdeg,count)
      endif
      
      ;; either no count because the box is empty or because the
      ;; sources in the box are too far away
      CASE count OF
         0: BEGIN
            IF (keep1 NE 0) THEN BEGIN
               iarr = [[iarr],[i,-1]]
               distances = [distances,-1]
            ENDIF
         END
         ; one match
         1: BEGIN
            iarr = [[iarr],[i,subset[matches_subset]]]
            distances = [distances,dist_subset[matches_subset]]
         END
         ELSE: BEGIN
            ;; sort by distance such that the index of the closest
            ;; match is listed first etc
            idx_sort_dist_subset = matches_subset[sort(dist_subset[matches_subset])]
            IF (multi EQ 0) THEN BEGIN
               ;; take only the smallest distance one.
               iarr = [[iarr],[i,subset[idx_sort_dist_subset[0]]]]
               distances = [distances,dist_subset[idx_sort_dist_subset[0]]]
            ENDIF ELSE BEGIN
               FOR j = 0L,count-1L DO BEGIN
                  iarr = [[iarr],[i,subset[idx_sort_dist_subset[j]]]]
                  distances = [distances,dist_subset[idx_sort_dist_subset[j]]]
               ENDFOR
            ENDELSE
         END
      ENDCASE
   ENDFOR

;; now make sure that the missing elements from mr2 are also include
;; incase of keep2 = 1
  IF (keep2  NE 0) THEN BEGIN
     all = make_array(n_elements(ra2),val=0)
     matched = iarr[1,*]
     all[matched] = 1
     unmatched = where(all EQ 0)
      
     FOR i = 0L,n_elements(unmatched)-1L DO BEGIN
        iarr = [[iarr],[-1,unmatched[i]]]
        distances = [distances,-1]
     ENDFOR
     
  ENDIF
  
  ;; strip the first bogus element
  IF n_elements(iarr[0,*]) GT 1 THEN BEGIN
     iarr = iarr[*,1:*]
     distances = distances[1:*]
     ;; distances in arcsec
     distances[where(distances ne -1)] = 3600d0*distances[where(distances ne -1)]
  ENDIF ELSE BEGIN
     message,'Not a single match was found',/inform
     return,0
  ENDELSE

  ;; Do we want to optimise the matching by shifting the positions
  ;; in tab2?
  IF (shift NE 0) THEN BEGIN
      
      ;; matching source array:
      marr = iarr[*,where((iarr[0,*] NE -1) AND (iarr[1,*] NE -1))]
      deltax = (ra2[marr[1,*]]-ra1[marr[0,*]])* $
               cos(!dpi/180d0*(de2[marr[1,*]]+de1[marr[0,*]])/2d0)
      deltay = de2[marr[1,*]]-de1[marr[0,*]]
      
      ;; The median value of the displacements
      shiftx = median(deltax)
      shifty = median(deltay)
      
      message,'applying a shift in ra,de of: '+ $
              string(shiftx*3600)+string(shifty*3600)+' in arcsec',/inform
      ;; Apply this shift to the coordinates of tab2
      ra2 = ra2 - shiftx/cos(!dpi/180d0*de2)
      de2 = de2 - shifty
      
      shift = 0
      GOTO,DO_MATCHING

  ENDIF

  IF keyword_set(sigma) THEN BEGIN
      
      ;; matching source array:
      marr = iarr[*,where((iarr[0,*] NE -1) AND (iarr[1,*] NE -1))]
      deltax = ((ra2[marr[1,*]]-ra1[marr[0,*]])* $
                cos(!dpi/180d0*(de2[marr[1,*]]+de1[marr[0,*]])/2d0))
      deltay = (de2[marr[1,*]]-de1[marr[0,*]])
      
      ;; fit a gaussian to these delta's to get an idea of
      ;; the width of the distribution
      
      ;; first we determine the range to take into account
      minmax = 0.5*(abs(min(deltax)) < abs(max(deltax)))
      ;; the number of bins, on average 20 points per bin but not
      ;; less than 20 bins
      foo = where(abs(deltax) LE minmax,cnt)
      nbins = ((cnt/20.d) > 10.0)
      binsize = (2.*minmax)/nbins
      histx = histogram(deltax,min=-1d0*minmax,binsize=binsize,nbins=nbins,locat=locat)
      fit =  gaussfit(locat,histx,param,nterms=3)
      stdevx = param[2]
      
      ;; first we determine the range to take into account
      minmax = 0.5*(abs(min(deltay)) < abs(max(deltay)))
      ;; the number of bins, on average 20 points per bin but not
      ;; less than 20 bins
      foo = where(abs(deltay) LE minmax,cnt)
      nbins = ((cnt/20.d) > 10.0)
      binsize = (2.*minmax)/nbins
      histy = histogram(deltay,min=-1d0*minmax,binsize=binsize,nbins=nbins,locat=locat)
      fit =  gaussfit(locat,histy,param,nterms=3)
      stdevy = param[2]
      
      stdev = sqrt(stdevx^2d0+stdevy^2d0)
      
      mdist = sigma*stdev*3600d0
      
      message,'the max distance is set to: '+string(mdist)+' arcsec',/inform
      message,'which equals '+string(sigma)+' x stdev of '+string(stdev*3600d0)+' arcsec', $
                  /inform
      
      sigma = 0
      GOTO,DO_MATCHING
  ENDIF
  
  IF remove NE 0 THEN BEGIN
     ;; iarr[1,*] holds the matching sources in tab2
     out = select_mr(tab2,/remove,where=reform(iarr[1,*]))
  ENDIF ELSE BEGIN
     out = join_mr(tab1,tab2,index_array=iarr)
  ENDELSE

  ;; we want an extra column in the output which has the found distance
  if add_distance then begin
     out = add_column_mr(out,dist,name='Match_Dist',unit='arcsec',descr='Distance between the matching sources')
  endif

  ;; Do some headwork here
  return,out
END 
