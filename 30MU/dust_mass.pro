FUNCTION dust_mass,lnk,temp=temp,flux=flux,distance=distance,rho=rho,xrange=xrange
  
  IF n_elements(lnk) EQ 0 THEN lnk = (read_ascii('~/d1/OPTICALCONSTANTS/LNK/MgS.lnk')).field1

  default,temp,200 ;; K
  default,flux,1d-12 ;; Integrated flux in the feature
  default,distance,1d3 ;; pc
  default,rho,3d3 ;; kg/m^3
  default,xrange,[15,50]
  
  pc = 3.09d16 ;;m
  Msun = 2d30 ;; kg

  intqab = int_qab(temp,lnk,xrange=xrange) ;; watt/m^2 /m^3
  
  mass = 4d0*rho/3d0*flux*(distance*pc)^2d0/intqab
  print,string(format='("From: d=",F7.2," pc, T=",F7.2," K, flux=",E10.2," W/m^2")',distance,temp,flux)
  print,string(format='("We find: M=",E10.2," kg, M=",E10.2," M_sun")',mass,mass/msun)
  
  return,mass/msun
END 

