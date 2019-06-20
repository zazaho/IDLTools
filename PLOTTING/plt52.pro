PRO plt52, aarf, title, title2
; is used to plot a spectrum into two pages
; 98/06/04 I.Yamamura
; 98/06/06 as an individual routine

; aarf : filename of a AAR/ALN file
; title : title for whole spectrum
; title2: title for plots of each bands

if IS_AAR(aarf) then aar = aarf else aar=read_faar(aarf)

subdiv, 1,1
plt, aar, xra=[2, 50], /xlog, ti=title
subdiv, 2,2
plt, aar, band=1, ti=title2
plt, aar, band=2, ti=title2
plt, aar, band=3, ti=title2
plt, aar, band=4, ti=title2
subdiv, 1,1

return
end

