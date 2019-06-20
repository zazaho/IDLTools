function sh_aper, instruct, band
qi=!pi/180.d0

ra=dblarr(4)
dec=dblarr(4)

cinstra = 0
cinstdec = 0
cinstroll = 0

st_ra  =read_fits_key(instruct.header,'CINSTRA',cinstra,comment)
st_dec =read_fits_key(instruct.header,'CINSTDEC',cinstdec,comment)
st_roll=read_fits_key(instruct.header,'CINSTROLL',cinstroll,comment)

if ((st_ra+st_dec+st_roll) eq 0) and $
   ((cinstra ne 0) or (cinstdec ne 0) or (cinstroll ne 0)) then begin
  instra   = cinstra
  instdec  = cinstdec
  instroll = cinstroll
endif else begin
  st_ra  =read_fits_key(instruct.header,'INSTRA',instra,comment)
  st_dec =read_fits_key(instruct.header,'INSTDEC',instdec,comment)
  st_roll=read_fits_key(instruct.header,'INSTROLL',instroll,comment)
endelse

st=read_fits_key(instruct.header,'OBSERVER',obsid,comment)
st=read_fits_key(instruct.header,'EOHAPLID',propid,comment)
st=read_fits_key(instruct.header,'EOHAOSN',ion,comment)

band = strupcase(band)
dy = 0

if ( band eq '1A' or $
     band eq '1B' or $
     band eq '1D' or $
     band eq '1E' or $
     band eq '2A' or $
     band eq '2B' or $
     band eq '2C'      ) then begin

       dy = 20./2.
       dz = 14./2.

endif

if ( band eq '3A' or $
     band eq '3C' or $
     band eq '3D'      ) then begin

       dy = 27./2.
       dz = 14./2.

endif

if ( band eq '3E'     ) then begin

       dy = 27./2.
       dz = 20./2.

endif

if ( band eq '4A' or $
     band eq '4C' or $
     band eq '4D'      ) then begin

       dy = 33./2.
       dz = 14./2.

endif

if ( band eq '4'      ) then begin

       dy = 33./2.
       dz = 20./2.

endif

if ( band eq '5A' or $
     band eq '5B' or $
     band eq '5C' or $
     band eq '5D'      ) then begin

       dy = 39./2.
       dz = 10./2.

endif

if  ( band eq '6'      ) then begin

       dy = 40./2.
       dz = 17./2.

endif


if ( dy eq 0) then begin
print, 'Wrong band  - try again'
return,0
endif


yz2radec,instra,instdec,instroll,-dy,-dz,ra0,dec0
ra(0)=float(ra0)
dec(0)=float(dec0)
yz2radec,instra,instdec,instroll, dy,-dz,ra0,dec0
ra(1)=float(ra0)
dec(1)=float(dec0)
yz2radec,instra,instdec,instroll, dy, dz,ra0,dec0
ra(2)=float(ra0)
dec(2)=float(dec0)
yz2radec,instra,instdec,instroll,-dy, dz,ra0,dec0
ra(3)=float(ra0)
dec(3)=float(dec0)

radecs = [[ra(0), dec(0)],[ra(1), dec(1)],[ra(2), dec(2)],[ra(3), dec(3)]]
return,radecs
end
