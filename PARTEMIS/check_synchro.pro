wset, 0
artbe = mrdfits('/Users/artemis/Desktop/SCAN21192/ACQ_090512_205309.fits',1)
;
apex  = mrdfits('/Users/artemis/Documents/apexdata/rawdata/APEX-21192-2009-05-12-E-083.B-0995A-2009/33/ARTEMIS450-ARTBE-ARRAYDATA-1.fits',1)
;
;plot,(artbe.mjd-artbe(0).mjd)*3600.*24., (apex.mjd-apex(0).mjd)*3600.*24.-(artbe.mjd-artbe(0).mjd)*3600.*24.
;
ref_apex_mjd  = apex(0).mjd
ref_artbe_mjd = artbe(0).mjd
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.
print, max((apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.)
; 0.011005020
;
wset, 2
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-artbe.mjd-0.5d0)*3600.d0*24.d0

;
wset, 1
artbe = mrdfits('/Users/artemis/Desktop/SCAN21192/ACQ_090512_205324.fits',1)
;
apex  = mrdfits('/Users/artemis/Documents/apexdata/rawdata/APEX-21192-2009-05-12-E-083.B-0995A-2009/34/ARTEMIS450-ARTBE-ARRAYDATA-1.fits',1)
;
;plot,(artbe.mjd-artbe(0).mjd)*3600.*24., (apex.mjd-apex(0).mjd)*3600.*24.-(artbe.mjd-artbe(0).mjd)*3600.*24.
;
ref_apex_mjd  = apex(0).mjd
ref_artbe_mjd = artbe(0).mjd
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24., $
xtitle="Time in sec from start of subscan", ytitle="(MJD(MBFITS_APEX)- MJD(PC_ArTeMiS) -0.5)*86400 (sec)", $
title="Comparison between MJD(MBFITS_APEX) and MJD(PC_ArTeMiS) during a typical subscan"
;
set_plot,'ps'
device,file="/Users/artemis/Documents/apexdata/partemis_desynchro.ps"
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24., $
xtitle="Time in sec from start of subscan", ytitle="(MJD(MBFITS_APEX)- MJD(PC_ArTeMiS) -0.5)*86400 sec", $
title="Comparison between MJD(MBFITS_APEX) and MJD(PC_ArTeMiS) during a subscan"
;
device,/close
set_plot,'x'
;
print, max((apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.)
; 0.010974845

;print, 46*11.
;506.

plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-artbe.mjd-0.5d0)*3600.d0*24.d0

;
wset, 1
artbe = mrdfits('/Users/artemis/Desktop/SCAN21192/ACQ_090512_204154.fits',1)
;
apex  = mrdfits('/Users/artemis/Documents/apexdata/rawdata/APEX-21192-2009-05-12-E-083.B-0995A-2009/1/ARTEMIS450-ARTBE-ARRAYDATA-1.fits',1)
;
;plot,(artbe.mjd-artbe(0).mjd)*3600.*24., (apex.mjd-apex(0).mjd)*3600.*24.-(artbe.mjd-artbe(0).mjd)*3600.*24.
;
ref_apex_mjd  = apex(0).mjd
ref_artbe_mjd = artbe(0).mjd
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.
print, max((apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.)
; 0.010913867
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., -(apex.mjd-artbe.mjd-0.5d0)*3600.d0*24.d0

;
;
;
;
wset, 0
artbe = mrdfits('/Users/artemis/Desktop/SCAN21194/ACQ_090512_210224.fits',1)
;
apex  = mrdfits('/Users/artemis/Documents/apexdata/rawdata/APEX-21194-2009-05-12-T-083.F-0100-2009/1/ARTEMIS450-ARTBE-ARRAYDATA-1.fits',1)
;
;plot,(artbe.mjd-artbe(0).mjd)*3600.*24., (apex.mjd-apex(0).mjd)*3600.*24.-(artbe.mjd-artbe(0).mjd)*3600.*24.
;
ref_apex_mjd  = apex(0).mjd
ref_artbe_mjd = artbe(0).mjd
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.
print, max((apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.)
; 0.61401925
;
wset, 1
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., -(apex.mjd-artbe.mjd-0.5d0)*3600.d0*24.d0

;
;
wset, 2
artbe = mrdfits('/Users/artemis/Desktop/SCAN20539/ACQ_090510_165907.fits',1)
;
apex  = mrdfits('/Users/artemis/Documents/apexdata/rawdata/APEX-20539-2009-05-10-E-083.C-0994A-2009/1/ARTEMIS450-ARTBE-ARRAYDATA-1.fits',1)
;
;plot,(artbe.mjd-artbe(0).mjd)*3600.*24., (apex.mjd-apex(0).mjd)*3600.*24.-(artbe.mjd-artbe(0).mjd)*3600.*24.
;
ref_apex_mjd  = apex(0).mjd
ref_artbe_mjd = artbe(0).mjd
;
plot, (artbe.mjd-ref_artbe_mjd)*3600.*24., (apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.
print, max((apex.mjd-ref_apex_mjd)*3600.*24.-(artbe.mjd-ref_artbe_mjd)*3600.*24.)
; 0.010731560
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

scan = 21450
scan = 21449
nsub = 23

scan = 20539
scan = 19969
scan = 19037
scan = 22241
scan = 22909
scan = 23824
nsub = 46
mjd_array_start   = dblarr(nsub)
mjd_array_end     = dblarr(nsub)
mjd_monitor_start = dblarr(nsub)
mjd_monitor_end   = dblarr(nsub)
;
@obs1_config
;
for i = 0, nsub-1 do begin & $
   print, "i = ", i, " Reading subscan ", i+1 & $
   READ_MBFITS, scan, i+1, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel   & $
   mjd_array_start(i) = arraydata(0).mjd & mjd_array_end(i) = arraydata(n_elements(arraydata.mjd)-1).mjd   & $
   mjd_monitor_start(i) = monitor(0).mjd & mjd_monitor_end(i) = monitor(n_elements(monitor.mjd)-1).mjd   & $ 
   print, "       " & $
endfor
;
help, mjd_array_start, mjd_array_end, mjd_monitor_start, mjd_monitor_end
;
wset, 0
plot, (mjd_array_start-mjd_monitor_start)*3600.d0*24.d0, $
xtitle="Subscan number -1", ytitle="Time delay between first P-ArTeMiS image and first monitor point in sec", $
title="Delay as a function of subscan number for scan"+string(scan)
;
set_plot,'ps'
device,file="/Users/artemis/Documents/apexdata/partemis_delay_for_"+strtrim(strcompress(string(scan)),2)+".ps"
;
plot, (mjd_array_start-mjd_monitor_start)*3600.d0*24.d0, $
xtitle="Subscan number-1", ytitle="Time delay between first image and first monitor point (sec)", $
title="Delay as a function of subscan number for scan"+string(scan)
;
device,/close
set_plot,'x'
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
scan = [21443, 21444, 21445, 21446, 21447, 21448]
scan = [23368, 23369, 23371, 23373, 23374, 23375, 23378, 23379, 23380, 23381, 23387, 23388, 23389, 23390]
scan_mars_22912 = [22912L, 22913L, 22914L, 22915L, 22916L, 22917L, 22918L, 22919L,22920L, 22921L, 22922L, 22923L, 22924L, 22926L, 22927L]
scan = scan_mars_22912
scan_mars_20540 = [20540L, 20541L, 20542L, 20543L, 20544L, 20545L, 20546L, 20547L, 20548L, 20549L, 20550L, 20551L]
scan = scan_mars_20540
nscan = n_elements(scan)
;
mjd_array_start   = dblarr(nscan)
mjd_array_end     = dblarr(nscan)
mjd_monitor_start = dblarr(nscan)
mjd_monitor_end   = dblarr(nscan)
;
;
for i = 0, nscan-1 do begin & $
   print, " i = ", i," Reading spiral scan ", scan(i) & $
   READ_MBFITS, scan(i), 1, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel, rows=[0,1]   & $
   mjd_array_start(i) = arraydata(0).mjd & mjd_array_end(i) = arraydata(n_elements(arraydata.mjd)-1).mjd   & $
   mjd_monitor_start(i) = monitor(0).mjd & mjd_monitor_end(i) = monitor(n_elements(monitor.mjd)-1).mjd   & $ 
   print, "       " & $
endfor
;
help, mjd_array_start, mjd_array_end, mjd_monitor_start, mjd_monitor_end
;
wset, 0
plot, scan, (mjd_array_start-mjd_monitor_start)*3600.d0*24.d0, $
xtitle="Spiral scan number", ytitle="Time delay between first P-ArTeMiS image and first monitor point in sec", $
title="Delay as a function of nodding scan number - 18 May 2009"
;title="Delay as a function of spiral scan number - 17 May 2009"
;
;
set_plot,'ps'
;device,file="/Users/artemis/Documents/apexdata/partemis_delay_for_spirals.ps"
device,file="/Users/artemis/Documents/apexdata/partemis_delay_for_nodding.ps"
;
plot, scan, (mjd_array_start-mjd_monitor_start)*3600.d0*24.d0, $
xtitle="Spiral scan number", ytitle="Time delay between first image and first monitor point (sec)", $
title="Delay as a function of nodding scan number - 18 May 2009"
;title="Delay as a function of spiral scan number - 13 May 2009"
;
device,/close
set_plot,'x'
;
;

scan = 22243
scan = 22894
scan = 22905
scan = 22908
scan = 23390
scan = 23379
READ_MBFITS, scan, 1, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel, rows=[0,1]
READ_MBFITS, scan, 1, arraydata, header_arraydata, datapar, header_datapar, monitor, header_monitor, datatel, header_tel, rows=lindgen(10000)
;
mjd_array_start   = arraydata(0).mjd
mjd_monitor_start = monitor(0).mjd
;
help, mjd_array_start, mjd_monitor_start
;
print, "(mjd_array_start-mjd_monitor_start) in sec: ", (mjd_array_start-mjd_monitor_start)*3600.d0*24.d0
;
mjd_datapar1 = datapar(1:*).mjd
mjd_datapar0 = datapar(0:33050).mjd
print, median((mjd_datapar1-mjd_datapar0)*3600.d0*24.d0)
;
mjd_array1 = arraydata(1:*).mjd
mjd_array0 = arraydata(0:64985).mjd
print, median((mjd_array1-mjd_array0)*3600.d0*24.d0)
;
mjd_datapar =datapar(0:33051).mjd
mjd_array = arraydata(0:33051).mjd
print, median((mjd_array-mjd_datapar)*3600.d0*24.d0)
; 0.0000000
;
print, min((mjd_array-mjd_datapar)*3600.d0*24.d0,id)
; 33041
; 
print, mjd_array(27236)*3600.d0*24.d0-mjd_array(27235)*3600.d0*24.d0
; 0.024999619
print, mjd_datapar(27236)*3600.d0*24.d0-mjd_datapar(27235)*3600.d0*24.d0
; 3.7499990
