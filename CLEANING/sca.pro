;; Function sca.pro
;; (Sacha's clean aar)
;; Remove points from aar interactively
;; Usage: cleaned_a1a = sca(a1a)
;; This will give a plot of the detectors and one screen with one
;; detector in which one can select bad points. Instructions are in
;; the command window
;; Usefull options:
;; ca1a = sca(a1a,yrange=[0,100],/updown,/xmode)
;; yrange determines the yrange for the detecors plots
;; /updown: make det-plots of up down on top of each other
;; /xmode: select bad points in between x1 and x2 and not in box.

;; (SH Feb 29 2000) Fixed bugs with windows not being correctly set
;; back to original window
FUNCTION sca,a,_extra=_extra
;;Make sure we are in normal plot mode
  device,set_graphics=3 
;; remember the window number or if no window: 0
  w = !d.window > 0
  
  sh_plotdet,a,/auto,_extra=_extra
  window,!d.window+2
  out = sh_cleanaar(a,_extra=_extra)
;; Set the window back to original  
  wset,w
  return,out
END
