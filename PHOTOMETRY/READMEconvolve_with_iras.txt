To use this routine:

put the *.pro files in your idl path
put the *.dat files in some place and edit the profilesdir
on line 31 of convolve_with_iras.pro to correspond to the directory
where you put the data files

start idl and read your spectrum in two variables
wave
flux

SyntheticIrasPhotometry = convolve_with_iras(wave,flux)

you can get the color correction factors out by doing

SyntheticIrasPhotometry = convolve_with_iras(wave,flux,K_12=K_12)

etc.

PS
To get reliable results the spectrum should of course cover the
wavelength range observed by iras otherwise extrapolation will take
place and strange/wrong  results will be obtained

The routine is well commented and follows very closely the notation
form the IRAS supplement:
http://lambda.gsfc.nasa.gov/product/iras/docs/exp.sup/ch6/C3.html


