pro corr_ext, donnees, donnees_cor, tau=tau

print, tau


donnees_cor=donnees

amass = 1./sin(donnees.elevatio*!pi/180.)
        donnees_cor.cube = donnees.cube*0.
	donnees_cor.cube = donnees.cube*exp(tau*amass)		; Calibration (extinction correction + pW --> Jy conversion)
	donnees_cor.unit = 'pW ext. corr.'

print, stdev(donnees_cor.cube-donnees.cube)
print, exp(tau*amass)
;wait, 1
;donnees_Jy=donnees




return
end
