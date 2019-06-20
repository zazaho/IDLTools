;From peeters@poseidon.sron.rug.nlTue Jun  9 13:52:31 1998
;Date: Tue, 9 Jun 1998 11:53:18 +0200 (MET DST)
;From: Els Peeters <peeters@poseidon.sron.rug.nl>
;To: hony@astro.rug.nl
;Subject: showlines.pro

pro showlines, a

; showlines.pro: prints line numbers and number of datapoints per line
; 
; 
; Author: A.C.A. Boogert 
; Date: June 12 1997: version 0.0, created 
; 
;

a_in=a

line=nlines(a_in)

FOR nl=0,(n_elements(line)-1) DO BEGIN
 ind=where(a_in.data.line EQ line(nl))
 g=n_elements(ind)
 w_min=min(a_in.data(ind).wave)
 w_max=max(a_in.data(ind).wave)
 print, '***SCAN', line(nl), ' HAS ', g, ' DATAPOINTS AT', w_min, w_max
ENDFOR

end


