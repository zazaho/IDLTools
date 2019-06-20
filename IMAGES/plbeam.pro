;; routine to put a beam (defined
pro plbeam

  ;; show the beam size on the top right
  xcirc = beamsize/2./3600.*cos(2*!dpi*dindgen(101)/100.) ;; beam coordinates in degree
  ycirc = beamsize/2./3600.*sin(2*!dpi*dindgen(101)/100.)
  
  ;; calculate the device pixels per degree
  tmp=convert_coord([0.,box_width],[0.,0.],/normal,/to_device)
  device_pixels_per_degree = abs((tmp[0,1]-tmp[0,0])/(ra_range[1]-ra_range[0]))
  
  xcirc_device = xcirc*device_pixels_per_degree
  ycirc_device = ycirc*device_pixels_per_degree
  
  ;; determine the center of where we want the beam in device coordinates
  beam_center_device = convert_coord( $
                       position[0]+box_width*0.9, $
                       position[1]+box_height*0.9, $
                       /normal,/to_device)
  
  plots,/device, $
        xcirc_device+beam_center_device[0], $
        ycirc_device+beam_center_device[1],color=kleur(textcolor)
  
  polyfill,/device, $
           xcirc_device+beam_center_device[0], $
           ycirc_device+beam_center_device[1], $
           /line_fill, $
           color=kleur(textcolor), $
           spacing=0.1, $
           orient=40, $
           _extra=_extra
end
