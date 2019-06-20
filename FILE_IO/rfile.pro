pro rfile, path=path, file, edum=edum, out, str, arrstr=arrstr

; rfile.pro: general procedure to read data from ascii files
;
; calling sequence: rfile, IN	path  : STR [path for data file]
;					default: './' i.e. current directory
;			   IN	file  : STR [filename]
;			   IN   edum=.. : INT [optional: #string rows before
;							data starts]
;			   IN   /arrstr the output array is a string
;					instead of a double
;			   OUT	out   : DBLARR(n,m)  [array with data points, 
;							n=#col m=#rows]
;					STRARR(n,m) if /arrstr set
;			   OUT	str   : STR(1,#string rows) [number of string
;					rows before data starts]
; The input data file MUST have format:
;		#rows(3) #cols(3) #rows(3) before data starts
;		some text
;		some more text
;		etc..etc
;		1.2	1.3	3.4 
;		 .	 .	 .
;		 .	 .	 .
;
; v0.0 10 Dec 1996 ACAB	: created from scratch 
; v0.1 18 Feb 1997 ACAB (at Ames): added empty header ('dum') handling 
;
;

if keyword_set(path) then path=path else path='./'
close, 1
print, '***', path+file, ' opened***'
openr, 1, path+ file

rowcol = INTARR(3,1)
                                           readf, 1, rowcol
IF keyword_set(edum) THEN rowcol(2)=edum
IF rowcol(2) NE 0 THEN dum = STRARR(1,rowcol(2)) 
IF keyword_set(arrstr) EQ 0 THEN all = DBLARR(rowcol(1),rowcol(0)) ELSE $
	all = STRARR(rowcol(1),rowcol(0)) 
IF rowcol(2) NE 0 THEN readf, 1, dum ELSE dum=' '
readf, 1, all
close, 1

out=all
str=dum

end
