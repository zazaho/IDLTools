PRO CleanPlot,print=print	;Set System Plot Variables to Default Values
;+
; NAME:
;	CLEANPLOT
; PURPOSE:
;	Reset all system variables (!P,!X,!Y,!Z) set by the user which 
;	affect plotting to their default values.
;
; CALLING SEQUENCE:
;	Cleanplot
;
; INPUTS:	
;	None
;
; OUTPUTS:	
;	Optionally prints changed values
;
; SIDE EFFECTS:	
;	The system variables that concern plotting are reset
;	to their default values.  A message is output for each
;	variable changed.   The CRANGE, S, WINDOW, and REGION fields of the
;	!X, !Y, and !Z system variables are not checked since these are 
;	set by the graphics device and not by the user.    
;
; PROCEDURE:
;	This does NOT reset the plotting device.
;	This does not change any system variables that don't control plotting.
;
; RESTRICTIONS:
;	If user default values for !P, !X, !Y and !Z are different from
;	the defaults adopted below, user should change P_old etc accordingly
;
; MODIFICATION HISTORY:
;	Written IDL Version 2.3.0  W. Landsman & K. Venkatakrishna May '92
;	Handle new system variables in V3.0.0     W. Landsman   Dec 92
;	Assume user has at least V3.0.0           W. Landsman   August 95
;	(SH Mar 16 2000) Added print keyword to suppress output unless set 
;       (SH Feb  1 2001) Updated to work with version 5.4 of IDL
;                        Safer implementation by not assuming X_old
;                        and !X to hold the same structure.
;                        More elegant implementation usin execute
;-
 On_error,2
 
 print = keyword_set(print)
 
 ;; Default values for !p,!x,!y,!z
 def_P ={BACKGROUND: 0L,CHARSIZE:0.0, CHARTHICK:0.0,  $
         CLIP:[0L,0,639,511,0,0], $ ;Not used
         COLOR : !D.N_COLORS-1, FONT: -1L, LINESTYLE: 0L, MULTI:lonarr(5),$
         NOCLIP: 0L, NOERASE: 0L, NSUM: 0L, POSITION: fltarr(4),$
         PSYM: 0L, REGION: fltarr(4), SUBTITLE:'', SYMSIZE:0.0, T:fltarr(4,4),$
         T3D:0L, THICK: 0.0, TITLE:'', TICKLEN:0.02, CHANNEL:0L }
 IF !D.NAME EQ 'PS' THEN def_P.COLOR = 0
 
 def_X ={TITLE: '', TYPE: 0L, STYLE:0L, TICKS:0L, TICKLEN:0.0, $
         THICK: 0.0, RANGE:fltarr(2), CRANGE:fltarr(2), S:fltarr(2), $
         MARGIN: [10.0 ,3.0], OMARGIN: fltarr(2), WINDOW: fltarr(2), $
         REGION: fltarr(2), CHARSIZE:0.0, MINOR: 0L, TICKV:fltarr(30), $
         TICKNAME: strarr(30), GRIDSTYLE: 0L, TICKFORMAT: '' } 
 
 def_Y = def_X
 def_Y.MARGIN = [4.0, 2.0]

 def_Z = def_X
 def_Z.MARGIN = [0.0, 0.0]
 
 ;; Loop over P,X,Y,Z
 postfices = ['P','X','Y','Z']
 
 FOR j=0,n_elements(postfices)-1 DO BEGIN
   postfix = postfices[j]
   ;; The tags belonging to this postfix.
   ;; The "." is already included here since we always need it below
   foo = execute('tags = "."+tag_names(def_'+postfix+')')
   ;; Now loop over the tags
   FOR i=0, n_elements(tags)-1 DO BEGIN
     
     ;; Determine if the value is changed
     foo = execute('changed=total(!'+postfix+tags[i]+ $
                   ' ne def_'+postfix+tags[i]+') ne 0')
     
     IF changed THEN BEGIN
       ;; Do we want printed comfirmation of what is changed?
       IF (print) THEN  BEGIN
         foo=execute('print,"Clearing !"+postfix+tags[i]+",old value=",!'+ $
                     postfix+tags[i])
       ENDIF
       ;; Now changed the value back to the default value
       foo = execute('!'+postfix+tags[i]+'= def_'+postfix+tags[i])
     ENDIF
   ENDFOR
 ENDFOR
END
