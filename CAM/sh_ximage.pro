;+
; NAME:
;	SH_XIMAGE
;
; WARNING: Don't use widget_olh and sh_ximage at the same time !!!
;          It will crash your idl session.
;
; PURPOSE:
;	SH_XIMAGE is a widget program for image display.
;       The input is one image with or without astrometry, in various format:
;        2-D IDL array,2-D IDL array+fits header, IDL structure raster,
;        name of a  file : fits, xdr, bdf (Midas)
;
;        This timage is displayed, you can get the intensity of a pixel 
;        (with its astrometry), zoom, 
;
;      - log and histogram equalization scale
;      - you can run several SH_XIMAGE at the same time
;      - saving the display in fits file 
;        (even if the input is not a fits file)
;      - saoimage-like zoom (=> right button mouse for pixel values)
;      - automatic and user title for the plots
;      - handling of a user path and user path format 
;      
; See also XDISP
;
; CATEGORY:	III-2, USER.
;
; CALLING SEQUENCE:
;       SH_XIMAGE
;       SH_XIMAGE, filename
;	SH_XIMAGE, Image
;       SH_XIMAGE, Raster
;	SH_XIMAGE, Image, FitsHeader 
;       SH_XIMAGE, Image, astr_struc, [equinox = equinox]
;
; INPUTS:
;       filename   -- string : fits or midas file name
;	Image      -- two dimensional array: image to analyse
;       Raster     -- cia structure : raster
;
; OPTIONAL INPUTS:
;	FitsHeader -- string array : Fits header
;       astr_struc -- structure : structure containing astrometry info
;	
; KEYED INPUTS:
;       equinox    -- float : equinox for astrometry (used for saving into fits)
;       group      -- long  : parent widget ID
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; KEYED INPUTS:
;
; EXAMPLE:
;       Image = readfits('data.fits', FitsHead)
;       SH_XIMAGE, Image, FitsHead
; or
;       raster=xslice(xcisp)
;       reduce, raster
;       raster_scan, raster
;       sh_ximage, raster
; or
;       sh_ximage, image, raster.astr
;
; COMMON BLOCKS: none !
;
; SIDE EFFECTS: if a bug occurs the heap stack is getting encumbered
;
; RESTRICTIONS: 
;   Don't use sh_ximage and widget_olh at the same time, It could crash your
;    session.
;
; ALGORITHM: 
;
; DEPENDENCIES: none
;
; SEE ALSO: XDISP, XVISU
;
; CALLED PROCEDURES AND FUNCTIONS:
;        save_window, restore_window, hist_equalize
;        tviso, plot_profil, plothist, xcontour, x3dim, power_spectrum, dft
;        la_undef, la_min
;        
; MODIFICATION HISTORY:
;       20-sep-1999 : P Chanial fecit
;       24-sep-1999 : PC added possibility to read xdr files
;       28-sep-1999 : PC wrote interface for cube viewing (arrows...)
;       05-Nov-1999 : RG for idl version 5.0
;       22-Feb-2000 : PC for idl version 5.3
;       15-May 2001 SO mrdfits and readfits are called with silent = cia_debug()
;                     V5.4
;       08-oct 2001 SO proper window title
;       10-october-2001 : RG get the palette and keep it v5.5
; COPYRIGHT: 
;
; Pierre Chanial (pchanial@cea.fr)
; Service d'Astrophysique, Bat 709
; CE Saclay - Orme des merisiers
; F-91191 Gif / Yvette
;
; The use and distribution to authorised sites are free, as long as this header
; text is not removed, or changed. 
; 
; No warranties for installation/ support/ maintenance are given.
;-


;----------------------------------------------------------------------
;           global variable handling
;----------------------------------------------------------------------

pro sh_ximage_get_global, event, global
 child = widget_info(event.top, /child)
 base_global = widget_info(child, /sibling)
 widget_control, base_global, get_uvalue=global, /no_copy
end

;----------------------------------------------------------------------

pro sh_ximage_put_global, event, global
 child = widget_info(event.top, /child)
 base_global = widget_info(child, /sibling)
 widget_control, base_global, set_uvalue=global, /no_copy
end

;----------------------------------------------------------------------

pro sh_ximage_error, global
 if n_params() ne 1 then begin
    print, 'Calling sequence: sh_ximage, global'
    return
 endif
 if n_elements(global) eq 0 then begin
    print, 'The bug has not occured inside an sh_ximage routine !'
    return
 endif
 child = widget_info(global.base_sh_ximage, /child)
 base_global = widget_info(child, /sibling)
 widget_control, base_global, set_uvalue=global, /no_copy
 retall
end

;----------------------------------------------------------------------

pro sh_ximage_subroutine_get_global, event, global
 child = widget_info(event.handler, /child)
 widget_control, child, get_uvalue=base_global
 widget_control, base_global, get_uvalue=global, /no_copy
end

;----------------------------------------------------------------------

pro sh_ximage_subroutine_put_global, event, global
 child = widget_info(event.handler, /child)
 widget_control, child, get_uvalue=base_global
 widget_control, base_global, set_uvalue=global, /no_copy
end

;----------------------------------------------------------------------

pro sh_ximage_cleanup, id
  common colors
  ;print,'sh_ximage_cleanup'
  widget_control, id, get_uvalue=global, /no_copy
  sh_ximage_free_pointers, global
  ;if xregistered('sh_ximage') eq 0 then loadct, global.color_table, /silent
  if xregistered('sh_ximage') eq 0 then $
      tvlct, global.palette.r, global.palette.g, global.palette.b  ; 10/10/2001
; !p.color= 0
; !p.background = !d.table_size-1 ;
  
end

;----------------------------------------------------------------------

pro sh_ximage_free_pointers, global
  ptr_free, global.image, global.astr, global.fitsheader, $
            global.raster, global.save_cross, global.cube_window, $
            global.scaled_image
end

;----------------------------------------------------------------------
 
pro sh_ximage_init_global, global

 tvlct, r, g, b, /get
 palette = {r:r, g:g, b:b}  ; 10/10/2001
 
 defsysv, '!cir', exist=i
 if not(i) then loadct, 15 ; stern
 if (i) then package = 'ISOCAM CIR' else package = 'ISOCAM CIA'
 cd, current = path
 
 global = {                   $
    base_sh_ximage : 0l,         $
    base_thumbnail : 0l,      $
    base_text : 0l,           $
    base_raster : 0l,         $
    display_min_id : 0l,      $
    display_max_id : 0l,      $
    image : ptr_new(0),       $
    image_size : [0l, 0l],    $
    image_range : [0., 0.],   $
    image_draw_id : 0l,       $
    image_draw_size : 512,    $
    image_draw_margin: [0, 0],$
    x_ratio : 0.,             $
    y_ratio : 0.,             $
    zoom : 1.,                $
    first_zoom : 1.,          $
    undefined_value : 0.,     $
    fitsheader : ptr_new(''), $
    query_astr   : 0,         $
    astr : ptr_new(0),        $
    equinox : 2000.,          $
    window  : -1l,            $
    path : path,              $
    no_data : 1,              $
    mousemode_id : 0l,        $
    mousemode_selector_id:0l, $
    
    scaled_image:ptr_new(0.), $
    display_autozoom_on : 0,  $
    display_autozoom_id : 0l, $
    colormode_id : 0l,        $
    display_min : 0.,         $ ; label min of image
    display_max : 0.,         $ ; label max of image
    threshold_min : -32768.,  $ ; min value of the image to be displayed
    threshold_max : -32768.,  $ ; max value of the image to be displayed
    scale : 'Scale.Linear',   $ ; color scaling linear, log, hist. eq.
    colorbar : bytarr(512),   $ ; colorbar
    colorbar_height : 10,     $ ; height of colorbar
    colorbar_window : 0l,     $ ; id of the colorbar window
    colorbar_id : 0l,         $ ; id of the colorbar widget draw
    rcolors : 8,              $ ; reserved colors
    ncolors : !d.table_size-8,$ ; image colors (!d.table_size - 8)
    brightness : 500l,        $ ; initial brightness setting
    contrast  : 500l,         $ ; initial contrast setting
    maxbright : 1000l,        $ ; max for bright and contrast (min=1)
    r_vector  : bytarr(256),  $ ; red   table for image display
    g_vector  : bytarr(256),  $ ; green table for image display
    b_vector  : bytarr(256),  $ ; blue  table for image display
    color_table : 15,         $ ; current color table (stern special)
    palette   : palette ,     $ ; old palette  10/10/2001
      
    xy_label : 0l,            $
    xcorner : 0,              $
    ycorner : 0,              $
    x : 0,                    $
    y : 0,                    $
    x_old : 0,                $
    y_old : 0,                $
    xcross  : 0,              $
    ycross  : 0,              $
    stretch_on : 0,           $
    xdevice :0,               $
    ydevice :0,               $
    xdevice_old  : 0,         $
    ydevice_old  : 0,         $
    save_cross : ptr_new(0),  $
    px : intarr(5),           $
    py : intarr(5),           $
    thumb_xoffset : 0,        $
    thumb_yoffset : 0,        $
    thumb_scroll_on : 0,      $ ; 1 if the user clicked on the thumbnail
    thumb_moved_on  : 0,      $ ; 1 if the thumbnail square is moved
    title : '',               $
    package : package,        $
    
    datamode_id : 0l,         $
    raster : ptr_new(),       $
    cube_button_id  : 0l,         $
    cube_on      : 0,         $
    base_cube    : 0l,        $
    cube_draw_id   : 0l,      $
    cube_draw_size:[512, 400],$
    cube_draw_margin :[0, 0], $
    cube_window : ptr_new(0), $
    single_on    : 0,         $  ; rg to be put to 1
    multi_on     : 0,         $
    together_on  : 1,         $
    cube_mask_on : 1,         $
    cube_flat_on : 0,         $  ; 1 if the cube displayed with flat correction
    scdlim_on    : 0,         $
    rasterscale_on : 0,       $
      
; overplot mode
    sky_views_on : 0,         $ ; 1 if the sky views are displayed
    skyviews_id  : 0l,        $ ; id of sky views button
;    compass_on   : 0,         $ ; 1 if the compass is displayed 
;    compass_id   : 0l,        $ ; id of compass button
;    scalebar_on  : 0,         $ ; 1 if the scalebar is displayed 
;    scalebar_id  : 0l,        $ ; id of scalebar button
    
; stat window
    stats_on : 0,             $
    base_stats : 0l,          $
    base_statbox : 0l,        $
    base_showstatzoom : 0l,   $
    stats_button_id : 0l,        $
    statboxsize : 11,         $
    statzoom_widget_id : 0l,  $
    statzoom_window_id : 0l,  $
    statxcenter_id : 0l,      $
    statycenter_id : 0l,      $
    stat_npix_id : 0l,        $
    statbox_min_id : 0l,      $
    statbox_max_id : 0l,      $
    statbox_mean_id : 0l,     $
    statbox_median_id : 0l,   $
    statbox_stdev_id : 0l,    $
    statzoom_size: 300,       $ ; size of statzoom window
      
    thumbnail:fltarr(128,128),$
    base_loupe : 0l,          $
    loupe_zoom : 16,          $
    base_loupe_npix : 0,      $
    base_loupe_window : 0l,   $
    thumbnail_size_max : 128, $
    thumbnail_size : [0, 0],  $
      
    base_phot : 0l,           $
    phot_on : 0,              $
    phot_button : 0l,         $
    centerpos: fltarr(2),     $ ; centered x,y for photometry
    cursorpos_id: 0l,         $ ; id of cursorpos widget
    centerpos_id: 0l,         $ ; id of centerpos widget
    centerbox_id: 0l,         $ ; id of centeringboxsize widget
    radius_id: 0l,            $ ; id of radius widget
    innersky_id: 0l,          $ ; id of inner sky widget
    outersky_id: 0l,          $ ; id of outer sky widget
    skyresult_id: 0l,         $ ; id of sky widget
    photresult_id: 0l,        $ ; id of photometry result widget
    fwhm_id: 0l,              $ ; id of fwhm widget
    radplot_widget_id: 0l,    $ ; id of radial profile widget
    radplot_window_id: 0l,    $ ; id of radial profile window
    photzoom_window_id: 0l,   $ ; id of photometry zoom window
    photzoom_size: 200l,      $ ; size in pixels of photzoom window
    showradplot_id: 0l,       $ ; id of button to show/hide radplot
    photwarning_id: 0l,       $ ; id of photometry warning widget
    photwarning: ' ',         $ ; photometry warning text
    centerboxsize: 7l,        $ ; centering box size
    r: 5l,                    $ ; aperture photometry radius
    innersky: 10l,            $ ; inner sky radius
      
    overplotmode_id : 0l      $
 }
  
end

;----------------------------------------------------------------------
;           dialog to warn the user
;----------------------------------------------------------------------

pro sh_ximage_dialog, global, str, _extra = extra
 
 ; if the widget is not not realized and an error occurs
 ; then prints an error message, elsewise uses a dialog box

 if global.base_sh_ximage eq 0 then begin
    print, str
 endif else begin
    ok = dialog_message(str, dialog_parent = global.base_sh_ximage, $
                        _extra = extra)
 endelse
end

;----------------------------------------------------------------------
;       routines for handling the color maps
;----------------------------------------------------------------------
;----------------------------------------------------------------------

function sh_ximage_icolor, color

; routine to reserve the bottom 8 colors of the color table
; for plot overlays and line plots.

   if (n_elements(color) eq 0) then return, 1

   ncolor = n_elements(color)

   ; if color is a string or array of strings, then convert color names
   ; to integer values
   ;if (size(color,/tname) eq 'STRING') then begin ; test if color is a string
   if (datatype(color) eq 'STR') then begin ; test if color is a string ; idl v5.0

      ; detemine the default color for the current device
      if (!d.name eq 'X') then defcolor = 7 $ ; white for x-windows
       else defcolor = 0 ; black otherwise

      icolor = 0 * (color eq 'black') $
             + 1 * (color eq 'red') $
             + 2 * (color eq 'green') $
             + 3 * (color eq 'blue') $
             + 4 * (color eq 'cyan') $
             + 5 * (color eq 'magenta') $
             + 6 * (color eq 'yellow') $
             + 7 * (color eq 'white') $
             + defcolor * (color eq 'default')

   endif else begin
      icolor = long(color)
   endelse

   return, icolor
end 

;*****************************

pro sh_ximage_set_color_table, event

 common colors
 
 sh_ximage_get_global, event, global
 widget_control, event.id, get_uvalue = values
 table = values[event.index]
 global.color_table = table
 sh_ximage_getct, global
 sh_ximage_put_global, event, global
 
end

;----------------------------------------------------------------------

pro sh_ximage_reset_color_table, event

 ;print, 'sh_ximage_reset_color_table', global.color_table
 sh_ximage_get_global, event, global
 widget_control, event.id, get_uvalue = values
 table = global.color_table
 loadct, table, bottom = global.rcolors, ncolors = global.ncolors
 sh_ximage_put_global, event, global
 
end

;----------------------------------------------------------------------

pro sh_ximage_stretch, global

; routine to change color stretch for given values of 
; brightness and contrast.
; brightness and contrast range from 1 up to global.maxbright
; a rather brute-force algorithm, but it works well enough.

common colors

;print, 'sh_ximage_stretcht', global.color_table

contrast = 0 > (global.ydevice / float(512) * $
                global.maxbright) < (global.maxbright-1)
contrast = long(abs(global.maxbright - contrast))

brightness = 0 > $
  (global.xdevice / float(512) * global.maxbright) < $
  (global.maxbright - 1)
brightness = long(abs(brightness-global.maxbright))

d = global.ncolors

maxdp = 600
mindp = 4

if (contrast lt (global.maxbright / 2)) then begin
    dp = ((d - maxdp) / float((global.maxbright / 2) - 1)) * contrast + $
      ((maxdp * global.maxbright / 2) - d) / float(global.maxbright / 2)
endif else begin
    dp = ((mindp - d) / float(global.maxbright / 2)) * contrast + $
      ((d * global.maxbright) - (mindp * global.maxbright / 2)) / $
      float(global.maxbright / 2)
endelse

dp =  fix(dp)

r = replicate(global.r_vector(d-1), 2*d + dp)
g = replicate(global.g_vector(d-1), 2*d + dp)
b = replicate(global.b_vector(d-1), 2*d + dp)

r[0:d-1] = global.r_vector(0)
g[0:d-1] = global.g_vector(0)
b[0:d-1] = global.b_vector(0)

a = findgen(d)

r[d] = congrid(global.r_vector(0:global.ncolors-1), dp)
g[d] = congrid(global.g_vector(0:global.ncolors-1), dp)
b[d] = congrid(global.b_vector(0:global.ncolors-1), dp)

bshift = round(brightness * (d+dp) / float(global.maxbright))

rr = r[a + bshift] 
gg = g[a + bshift]
bb = b[a + bshift]

tvlct, rr, gg, bb, global.rcolors
;print,  n_elements(rr), n_elements(r_curr)
;print,  max(rr-r_curr)
tvlct, rr, gg, bb, /get
r_curr=rr
g_curr=gg
b_curr=bb

global.brightness = brightness
global.contrast = contrast

end

;--------------------------------------------------------------------

pro sh_ximage_getct, global

; read in a pre-defined color table, and invert if necessary.

; load a simple color table with the basic global.rcolors colors in the lowest global.rcolors entries
; of the color table
rtiny   = [0, 1, 0, 0, 0, 1, 1, 1]
gtiny = [0, 0, 1, 0, 1, 0, 1, 1]
btiny  = [0, 0, 0, 1, 1, 1, 0, 1]
tvlct, 255*rtiny, 255*gtiny, 255*btiny
common colors
loadct, global.color_table, /silent,  bottom=global.rcolors
tvlct, r, g, b, global.rcolors, /get

;if (state.invert_colormap eq 1) then begin
;r = reverse(r)
;g = reverse(g)
;b = reverse(b)
;endif
global.r_vector(0:global.ncolors-1) = r
global.g_vector(0:global.ncolors-1) = g
global.b_vector(0:global.ncolors-1) = b
global.xdevice = 255
global.ydevice = 255

sh_ximage_stretch, global

end

;----------------------------------------------------------------------
;            initialization
;----------------------------------------------------------------------

pro sh_ximage_init_variable, global

 global.no_data = 0
 ;if size(*global.image, /n_dim) eq 3 then begin
 if (size(*global.image))(0) eq 3 then begin  ; idl v5.0
    sh_ximage_dialog, global, 'I take the first image of the cube !', $
      /info
    *global.image = reform((*global.image)(*,*,0))
 endif
 
 ;dim = size(*global.image, /dim)
 dim = (size(*global.image))(1:2)  ; idl v5.0
 global.image_size = dim
 *global.scaled_image = bytarr(dim[0], dim[1])
 global.xcorner = 0
 global.ycorner = 0
 global.zoom = 1.
 global.first_zoom = float(global.image_draw_size) / max(dim)
 global.x_ratio = dim(0)/float(max(dim))
 global.y_ratio = dim(1)/float(max(dim))
 global.thumbnail_size[0] = dim(0)/float(max(dim)) * global.thumbnail_size_max
 global.thumbnail_size[1] = dim[1]/float(max(dim)) * global.thumbnail_size_max
; stat, *global.image, good_index = good_index, bad_index = bad_index
 bad_index = where(*global.image eq -32768. or finite(*global.image) eq 0, count)
 if count gt 0 then (*global.image)[bad_index] = 0
 good_index = where(*global.image ne 0, count)

 ;if size(good_index, /n_dim) eq 0 then begin
 if (size(good_index))(0) eq 0 then begin  ; idl v5.0
    dok = dialog_message('All values are undefined !', dialog_parent = global.base_sh_ximage, /info)
    global.image_range[0] = 0.
    global.image_range[1] = 0.
;    (*global.image)(bad_index) = global.undefined_value
 endif else begin ;if size(bad_index, /n_dim) ne 0 then begin
    global.image_range[0] = min((*global.image)(good_index), max = maxi)
    global.image_range[1] = maxi
;    (*global.image)(bad_index) = global.undefined_value
 endelse
 global.threshold_min = global.image_range[0]
 global.threshold_max = global.image_range[1]
 global.thumbnail = congrid(*global.image, global.thumbnail_size[0], global.thumbnail_size[1])
 global.xcross = dim[0] / 2
 global.ycross = dim[1] / 2
 global.x = global.xcross
 global.y = global.ycross
end

;----------------------------------------------------------------------

pro sh_ximage_init_widget, global

 if ptr_valid(global.raster) then begin
    widget_control, global.cube_button_id, /sensitive
    widget_control, global.skyviews_id   , /sensitive
 endif else begin
    widget_control, global.cube_button_id, sensitive = 0
    widget_control, global.skyviews_id   , sensitive = 0
 endelse
 if global.query_astr then begin
;    widget_control, global.compass_id , /sensitive
;    widget_control, global.scalebar_id, /sensitive
 endif else begin
;    widget_control, global.compass_id , sensitive = 0
;    widget_control, global.scalebar_id, sensitive = 0
 endelse
 widget_control, global.base_text, set_value = global.title
 widget_control, global.base_thumbnail, draw_xsize = global.thumbnail_size[0]
 widget_control, global.base_thumbnail, draw_ysize = global.thumbnail_size[1]
 sh_ximage_display_all, global, /scale
end

;----------------------------------------------------------------------

pro sh_ximage_clean_widget, global
 ; clean the windows and the overplots

 if global.cube_on then begin
    base = global.base_cube
    event = {top:global.base_sh_ximage}
    sh_ximage_put_global, event, global
    widget_control, base, /destroy
    sh_ximage_get_global, event, global
 endif
 if global.stats_on then begin
    base = global.base_stats
    event = {top:global.base_sh_ximage}
    sh_ximage_put_global, event, global
    widget_control, base, /destroy
    sh_ximage_get_global, event, global
 endif
 if global.phot_on then begin
    base = global.base_phot
    event = {top:global.base_sh_ximage}
    sh_ximage_put_global, event, global
    widget_control, base, /destroy
    sh_ximage_get_global, event, global
 endif
 
end

;----------------------------------------------------------------------

pro sh_ximage_set_mousemode, event

 sh_ximage_get_global, event, global
 widget_control, event.id, get_uvalue = value
 
 widget_control, global.mousemode_id, map = 0
 
 case value(event.index) of
    'Color mode'    : global.mousemode_id = global.colormode_id
    'Data mode'     : global.mousemode_id = global.datamode_id
    'Overplot mode' : global.mousemode_id = global.overplotmode_id
 endcase
 
 widget_control, global.mousemode_id, map = 1
 widget_control, global.base_sh_ximage, tlb_get_size = size
 global.image_draw_margin = size - global.image_draw_size
 
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------
;           image scaling
;----------------------------------------------------------------------

pro sh_ximage_scale_image, global

 if global.display_autozoom_on then begin
    sh_ximage_get_image_corners, global, xs, xe, ys, ye
    image = congrid((*global.image)[xs:xe,ys:ye], 100, 100)
    index = where(image ne 0, count)
    if count gt 0 then begin
       display_min = min(image[index], max = display_max)
       global.threshold_min = display_min
       global.threshold_max = display_max
    endif
 endif else begin
    image = congrid(*global.image, 100, 100)
 endelse
 
 good_index = where(image ne 0 and image ge global.threshold_min and image le global.threshold_max, count)
 if count eq 0 then begin
    if global.display_autozoom_on then begin
       global.colorbar = 0
       (*global.scaled_image)[*] = 0b
    endif
    return
 endif

 
 case global.scale of
    'Scale.Hist. Eq.' : begin
          med = median(image[good_index])
          sig = sigma (image[good_index])
          bin = sig / 200
          min = med-3*sig > global.threshold_min
          max = med+3*sig < global.threshold_max
          if min ne max then begin
             cdf = hist_equal(image[good_index], min = min, max = max, $
                              bin = bin, /histogram_only)
             cdf=bytscl(temporary(cdf), top = global.ncolors-1)+global.rcolors
             cdf_min = min
             cdf_bin = bin
             (*global.scaled_image)[*] = cdf[(*global.image-min > 0)/bin < (n_elements(cdf)-1)]
             colorbar = findgen(n_elements(global.colorbar)) / n_elements(global.colorbar) * $
                (global.threshold_max-global.threshold_min) + global.threshold_min
             global.colorbar      = congrid(cdf[(colorbar     -min > 0)/bin < (n_elements(cdf)-1)], n_elements(global.colorbar)-1)
          endif
;          image = bytscl(hist_equalize(image, x = x, h = h, max = maxi), top = !d.table_size-1)
;          plot, x, h
       end
    'Scale.Log' : begin
             (*global.scaled_image)[*] = $
                bytscl(alog10 (bytscl(*global.image, $                       
                             min=global.threshold_min, $
                             max=global.threshold_max) + 1),  $
                       top = global.ncolors - 1) + global.rcolors
             global.colorbar = $
                bytscl(alog10(bytscl(indgen(n_elements(global.colorbar)))+1), $
                       top = global.ncolors - 1) + global.rcolors
       end
    'Scale.Linear' : begin
          (*global.scaled_image)[*] = ((*global.image-global.threshold_min) > 0) / $
             (global.threshold_max-global.threshold_min) * global.ncolors + global.rcolors
          global.colorbar = congrid(bindgen(global.ncolors)+global.rcolors, n_elements(global.colorbar))
       end
 endcase

 index = where(*global.scaled_image eq global.rcolors, count)
 if count gt 0 then (*global.scaled_image)[index] = sh_ximage_icolor('black')
 
end
 
;----------------------------------------------------------------------

pro sh_ximage_get_data_from_image, global, x_image, y_image, x_data, y_data, out = out

 x_d = global.xcorner + fix(x_image/global.first_zoom/global.zoom)
 y_d = global.ycorner + fix(y_image/global.first_zoom/global.zoom)
 x_data = x_d > 0 < ((size(*global.image))(1) - 1)
 y_data = y_d > 0 < ((size(*global.image))(2) - 1)
 if x_d ne x_data or y_d ne y_data then out = 1 else out = 0
 
end

;----------------------------------------------------------------------

pro sh_ximage_get_image_from_data, global, x_data, y_data, x_image, y_image

 x_image = fix((x_data+.5 - global.xcorner) * global.zoom * global.first_zoom)
 y_image = fix((y_data+.5 - global.ycorner) * global.zoom * global.first_zoom)

end

;----------------------------------------------------------------------

pro sh_ximage_get_image_corners, global, xs, xe, ys, ye

 sh_ximage_get_data_from_image, global, 0, 0, xs, ys
 sh_ximage_get_data_from_image, global, global.image_draw_size-1, global.image_draw_size-1, xe, ye

end

;----------------------------------------------------------------------


pro sh_ximage_display_all, global, scale = scale

 if global.no_data then return
 
 if keyword_set(scale) or global.display_autozoom_on then sh_ximage_scale_image, global
 sh_ximage_display_image, global
 sh_ximage_display_thumbnail, global
 sh_ximage_display_loupe, global
 if global.sky_views_on then sh_xraster_sky_views, global
 sh_ximage_plot_cross, global
 sh_ximage_display_colorbar, global
 
 if global.threshold_min eq -32768. then begin
    widget_control, global.display_min_id, set_value = ""
    widget_control, global.display_max_id, set_value = ""
 endif else begin
    widget_control, global.display_min_id, set_value = $
       strcompress(string(global.threshold_min, /print), /remove)
    widget_control, global.display_max_id, set_value = $
       strcompress(string(global.threshold_max, /print), /remove)
 endelse
 
end

;----------------------------------------------------------------------

pro sh_ximage_display_image, global

 sh_ximage_get_image_corners, global, xs, xe, ys, ye
 xsize = xe-xs+1
 ysize = ye-ys+1
 
 if xsize*ysize ne 0 then begin

    image = (*global.image)(xs:xe,ys:ye)
    widget_control,/hourglass
    
    big_nc = round(xsize * global.zoom * global.first_zoom)
    big_nl = round(ysize * global.zoom * global.first_zoom)

    image = (*global.scaled_image)[xs:xe,ys:ye]
    image = congrid(image, big_nc, big_nl)
    
    widget_control, global.image_draw_id, get_value=window
    wset, window
    sh_ximage_get_image_from_data, global, xs-0.5, ys-0.5, xs_image, ys_image
    sh_ximage_get_image_from_data, global, xe+0.5, ye+0.5, xe_image, ye_image
    if ys_image gt 0 then tv, bytarr(global.image_draw_size,ceil(ys_image))+sh_ximage_icolor('white')
    if xs_image gt 0 then tv, bytarr(ceil(xs_image),global.image_draw_size)+sh_ximage_icolor('white')
    tv, image, xs_image, ys_image
    if ye_image lt global.image_draw_size-1 then tv, bytarr(global.image_draw_size,ceil(global.image_draw_size-ye_image))+sh_ximage_icolor('white'), 0, ye_image
    if xe_image lt global.image_draw_size-1 then tv, bytarr(ceil(global.image_draw_size-xe_image),global.image_draw_size)+sh_ximage_icolor('white'), xe_image, 0
 endif else begin
    print, 'Erreur: pas de donnees dans la region choisie !'
 endelse

end

;----------------------------------------------------------------------

pro sh_ximage_display_colorbar, global

 wset, global.colorbar_window
; widget_control, global.label1_colorbar, set_value = global.display_min
 tv, congrid(reform(global.colorbar, n_elements(global.colorbar), 1), $
    global.image_draw_size, global.colorbar_height)
; widget_control, global.label2_colorbar, set_value = global.display_max

end

;----------------------------------------------------------------------

pro sh_ximage_load_variables, global, data, hd, ok = ok
 ; loads data into global
 
 ok = 0
 
 ;type = size(data, /type)
 type = datatype(data, 2) ; idl v5.0
 
 ;-----------
 ; structure
 ;-----------
 if type eq 8 then begin

    if tag_exist(data, 'raster') and tag_exist(data, 'astr') then begin
       ok = 1
       sh_ximage_clean_widget, global
       if data.pfov eq 1.5 then str = '1.5' else str = strn(fix(data.pfov))
       *global.image = data.raster
       ptr_free, global.raster
       sh_xraster_extract, data, mydata  ;v1.0
       global.raster = ptr_new(mydata, /no_copy)
       *global.astr = data.astr
       global.query_astr = 1
       global.title = data.target+', '+ data.fltrwhl+', '+str+'"'
       
       ; this can be replaced by raster2headerfits or raster2hdr
       mkhdr, header, *global.image
       sxaddpar, header, 'origin  ', 'ESA     ', 'European Space Agency'
       sxaddpar, header, 'telescop', 'ISO     ', 'Infrared Space Observatory'
       sxaddpar, header, 'instrume', 'CAM     ', 'Instrument used'
       sxaddpar, header, 'aot     ', data.aot       
       sxaddpar, header, 'object  ', data.target
       if tag_exist(*global.raster, 'observer') then $
       sxaddpar, header, 'observer', data.observer  
       if tag_exist(*global.raster, 'tdtosn') then $
       sxaddpar, header, 'tdtosn  ', data.tdtosn    
       sxaddpar, header, 'channel ', data.channel   
       sxaddpar, header, 'pfov    ', data.pfov      
       sxaddpar, header, 'tint    ', data.tint      
       sxaddpar, header, 'gain    ', data.gain      
       sxaddpar, header, 'fltrwhl ', data.fltrwhl   
       sxaddpar, header, 'waveleng', data.wavelength
       putast, header, *global.astr, equinox = 2000.
       sxaddpar, header, 'history', 'Fits created from a raster structure by Sh_Ximage '$
         + global.package
       
       *global.fitsheader = header

    endif else begin
       sh_ximage_dialog, global, 'This is not a raster structure !', /error
       return
    endelse
    
 ;--------
 ; number
 ;--------
 endif else if type ne 0 and type ne 6 and type ne 9 and $
            type ne 10 and type ne 11 then begin
    
    ;ndim = size(data, /n_dim)
    ndim = (size(data))(0) ; idl v5.0
    if ndim lt 2 or ndim gt 3 then begin
       sh_ximage_dialog, global, /error, 'The image has '+strn(ndim)+' dimensions !'
       return
    endif
    
    ok = 1
    sh_ximage_clean_widget, global
    *global.image = data
    ptr_free, global.raster
    global.title = ''
    
    ; astrometry
    if n_elements(hd) ne 0 then begin
       ;type2 = size(hd, /type)
       type2 = datatype(hd, 2)  ; idl v5.0
       ; string
       if type2 eq 7 then begin
          
          header = hd
          extast,header,astr,noparams
          if (noparams ge 0) then begin
             *global.astr = astr
             global.query_astr = 1
             global.equinox = sxpar(header, 'equinox')
             if global.equinox eq 0 then global.equinox = 2000.
          endif
          title = sxpar(hd, 'object')
          ;if size(title, /type) eq 2 then begin
          if datatype(title, 2) ne 7 then begin ; idl v5.0
             global.title = ''
          endif else begin
             global.title = title(0) ; idl v5.0
          endelse

       endif else begin
          mkhdr, header, *global.image
          ; structure
          if type2 eq 8 then begin
             if tag_names(hd, /structure_name) eq 'astr_struc' then begin
                global.query_astr = 1
                *global.astr=hd
                putast, header, hd, equinox = global.equinox
             endif else begin
                sh_ximage_dialog, global, /error, 'Invalid astrometric structure !'
             endelse
          endif else begin
             print, 'Invalid second argument !'
          endelse
       endelse
    endif else begin
       mkhdr, header, *global.image
    endelse
    
    sxaddpar, header, 'history', 'Fits created from idl '+strlowcase(type_of(data)) + $
      ' array by sh_ximage '+global.package
    *global.fitsheader = header
 
 ;------------
 ; wrong type
 ;------------
 endif else if type ne 0 then begin
    sh_ximage_dialog, /error, global, 'the input has an invalid type !'
    
 ;-----------
 ; undefined
 ;----------
 endif else begin
    sh_ximage_dialog, /error, global, 'the input is undefined !'
 endelse

end

;----------------------------------------------------------------------

pro sh_ximage_load_file, global, filename, data, fitsheader, ok = ok

 ok = 0
 format = 'unknown'
 if  !version.os eq 'vms' then filename = strlowcase(filename)
 ; find the image format
 if rstrpos(filename, '.fit') gt 0 then format = 'fits' else $
 if rstrpos(filename, '.bdf') gt 0 then format = 'midas' 
 case format of
    'midas': begin ; not tested
          file = str_sep(filename,'.bdf')
          file = file(0)
          mid_rd_image, file, data, naxis, npix
          mkhdr, fitsheader, data
          sxaddpar, fitsheader, 'history', 'Fits created from MIDAS file '+ $
            file+'.bdf by Sh_Ximage '+ global.package
          print, 'Filename: ', filename
          ok = 1
       end
    'fits': begin
          data = float(readfits(filename, fitsheader, silent=not(cia_debug())))
          print, 'Filename: ', filename
          ok = 1
       end
     else: begin
          restore_xdr, filename, data, group = global.base_sh_ximage
          if n_elements(data) ne 0 then ok = 1
       end
 endcase
 
 fdecomp, filename, disk, path
 global.path = path
 
end

;----------------------------------------------------------------------

function sh_ximage_get_data_info, global, x, y, out

 if keyword_set(out) then return, ''
 
 chaine=''
 intensity = (*global.image)(x,y)
 if intensity eq global.undefined_value then begin
    inten = 'undefined'
 endif else begin
    inten = strn(intensity)
 endelse
       
 if global.query_astr then begin
    xy2ad,x,y,*global.astr,a,d
    if fix(global.equinox) eq global.equinox then begin
       equ_str= strn(fix(global.equinox))
       if global.equinox eq 2000 then begin
          equ_str = 'J'+equ_str
       endif else if global.equinox eq 1950 then begin
          equ_str = 'B'+equ_str
       endif
    endif else begin
       equ_str = strn(global.equinox)
    endelse
    chaine="   ["+strtrim(adstring(a,d,1), 1)+"] " + equ_str
 endif
 
 return, '('+strn(x)+','+strn(y)+') = '+ inten+chaine

end

;----------------------------------------------------------------------
; File Menu
;----------------------------------------------------------------------

pro sh_ximage_load_event, event

 sh_ximage_get_global, event, global
 
 filename = dialog_pickfile(/read, filter='*.*', path = global.path, $
                            /must_exist, group = event.handler)
 if strlen(filename) gt 0 then begin
    widget_control, /hourglass
    sh_ximage_load_file, global, filename, data, fitsheader, ok = ok
    if ok then begin
       sh_ximage_load_variables, global, data, fitsheader, ok = ok
       if ok then begin
          sh_ximage_init_variable, global
          sh_ximage_init_widget, global
       endif
    endif
 endif
 
 sh_ximage_put_global, event, global
  
end

;----------------------------------------------------------------------

pro sh_ximage_plot_image_event, event

 sh_ximage_get_global, event, global
 if global.no_data then goto, closing
 
 widget_control, global.base_text, get_value = title
 global.title = title(0)  ; idl v5.0
 widget_control, global.image_draw_id, get_value = window
 wset, window
 sh_ximage_remove_cross, global
 display = tvrd()
 sh_ximage_plot_cross, global
 window, /free, xsize=600, ysize=600
 tviso, display, taille = 400, title = global.title, colorbar = $
   global.colorbar, min_real = global.threshold_min, $
   max_real = global.threshold_max, bottom=global.rcolors,$
   ncolors=global.ncolors
 
closing:
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_save_event, event

 sh_ximage_get_global, event, global
 if global.no_data then goto, closing
 
 widget_control, global.base_text, get_value = title
 global.title = title(0)
 
 file = concat_dir(global.path, 'xcube_frame.ps')
 answer = simple_cw_form($
    'Format : ', ['PS', 'GIF', 'TIFF', 'FITS'], $
    'Colorbar ? ', ['No', 'Yes'],       $
    dialog_parent = global.base_sh_ximage,         $
    title = 'Save frame ...')
 
 if not answer.ok then goto, closing
 
 format   = answer.(1)
 colorbar = answer.(2) eq 'Yes'
 file = 'sh_ximage'
 if !version.os_family eq 'vms' then file = strupcase(file)
 filename = concat_dir(global.path, file)
 
 if format eq 'FITS' then begin
    
    if !version.os_family ne 'vms' then prefix = strlowcase(format)
    filename = file+'.'+prefix
    filename = dialog_pickfile(/write, filter = concat_dir(global.path, '*.fits'), $
                  file = filename, $
                  group = global.base_sh_ximage)
    fdecomp, filename, new_disk, new_path, new_file
    ok = can_i_write_file(filename, group = global.base_sh_ximage, $
            overwrite = new_file eq file)
    if not ok then goto, closing
    global.path = new_path
    
    sh_ximage_get_image_corners, global, xs, xe, ys, ye
    hextract, *global.image, *global.fitsheader, save, hdr, $
              xs,xe,ys,ye, /silent
    sxaddpar, hdr, 'history', 'Fits written by Sh_Ximage '+ global.package
    writefits, filename, save, hdr
    goto, closing
    
 endif

 widget_control, /hourglass
 
 save_window, old
 widget_control, global.image_draw_id, get_value = window
 wset, window
 sh_ximage_remove_cross, global
 display = tvrd()
 sh_ximage_plot_cross, global
 if colorbar then begin
    window, /free, /pixmap, xsize=600, ysize=600
    window = !d.window
    title = global.title
    tviso, display, taille = 400, title = title, colorbar = $
       global.colorbar, min_real = global.threshold_min, $
       max_real = global.threshold_max, bottom=global.rcolors,$
   ncolors=global.ncolors
    display = tvrd()
    wdelete, window
 endif
 restore_window, old
 
 if format eq 'PS' then begin
   
    if !version.os_family ne 'vms' then prefix = strlowcase(format)
    filename = concat_dir(global.path, file+'.'+prefix)
    save_window, old
    aspect = float(global.image_draw_size) / global.image_draw_size
    forminfo = cmps_form(cancel = canceled, create = create, $
       aspect = aspect, $
       parent = global.base_sh_ximage, $
       /preserve_aspect, $
       filename = filename, $
       inches = 0, $
       xsize = 6.0, ysize = 6.0 * aspect, $
       /color, encapsulated = 0, $
       /nocommon, papersize='A4', $
       bits_per_pixel=8, $
       button_names = ['OK'])
    restore_window, old
    if canceled then goto, closing
    filename = forminfo.filename
    fdecomp, filename, new_disk, new_path, new_file
    ok = can_i_write_file(filename, overwrite = new_file eq file, group = global.base_sh_ximage)
    if not ok then goto, closing
    global.path = new_path
    set_plot, 'ps'
    device, _extra = forminfo
    tv, display
    end_ps, old
    goto, closing
  
 endif
 
 tvlct, rr, gg, bb, /get
 
 rn = congrid(rr[8:!d.table_size-1], 248)
 gn = congrid(gg[8:!d.table_size-1], 248)
 bn = congrid(bb[8:!d.table_size-1], 248)
 
 rvec = bytarr(256)
 gvec = bytarr(256)
 bvec = bytarr(256)
 
 rvec[0] = rr  ; load in the first 8 colors
 gvec[0] = gg
 bvec[0] = bb
 
 rvec[8] = temporary(rn)
 gvec[8] = temporary(gn)
 bvec[8] = temporary(bn)
 
 w = where(display GT 7, count)
 
 tmp_img = bytscl((display[w]), top = 247) + 8
 display[w] = tmp_img

 if format eq 'GIF'then begin
    
    if !version.os_family ne 'vms' then prefix = strlowcase(format)
    filename = file+'.'+prefix
    filename = dialog_pickfile(/write, filter = concat_dir(global.path, '*.gif'), $
                  file = filename, $
                  group = global.base_sh_ximage)
    fdecomp, filename, new_disk, new_path, new_file
    ok = can_i_write_file(filename, group = global.base_sh_ximage, $
            overwrite = new_file eq file)
    if not ok then goto, closing
    global.path = new_path
    write_gif, filename, display, rvec, gvec, bvec
    
 endif else if format eq 'TIFF'   then begin
   
    if !version.os_family ne 'vms' then prefix = strlowcase(format)
    filename = file+'.'+prefix
    filename = dialog_pickfile(/write, filter = concat_dir(global.path, '*.tiff'), $
                  file = filename, $
                  group = global.base_sh_ximage)
    fdecomp, filename, new_disk, new_path, new_file
    ok = can_i_write_file(filename, group = global.base_sh_ximage, $
            overwrite = new_file eq file)
    if not ok then return
    global.path = new_path
    write_tiff, filename, display, 0, red = rvec, green = gvec, blue = bvec
    
 endif
 
closing:
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_quit_event, event

 sh_ximage_get_global, event, global
 base_sh_ximage = global.base_sh_ximage
 sh_ximage_put_global, event, global  
 widget_control, base_sh_ximage, /destroy
 
end

;----------------------------------------------------------------------

pro sh_ximage_file_event, event

 case event.value of
    'File.Load'       : sh_ximage_load_event, event
    'File.Save'       : sh_ximage_save_event, event
    'File.Plot Image' : sh_ximage_plot_image_event, event
    'File.Quit'       : sh_ximage_quit_event, event
 endcase

end

;----------------------------------------------------------------------

pro sh_ximage_scale_event, event

 sh_ximage_get_global, event, global
 
 global.scale = event.value
 sh_ximage_display_all, global, /scale
 
closing:
 sh_ximage_put_global, event, global
 
end

;----------------------------------------------------------------------
 
pro sh_ximage_tools_event, event

 sh_ximage_get_global, event, global

 if global.no_data then return
 
 case event.value of 
 'Tools.Find Maximum': begin
        sh_ximage_get_image_corners, global, xs, xe, ys, ye
        image = (*global.image)[xs:xe,ys:ye]
        rank = (where(image eq max(image)))[0]
        sh_ximage_remove_cross, global
        global.ycross = ys+rank / (size(image))[1]
        global.xcross = xs+rank - (global.ycross-ys) * (size(image))[1]
        sh_ximage_plot_cross, global
        print, 'Maximum: ', sh_ximage_get_data_info(global, global.xcross, global.ycross)
   end
 'Tools.Profile': begin
   print, 'Not implemented, wait...'
   goto, closing
   print, 'Middle mouse button to toggle between rows and columns. left to display and right mouse button to exit.'
   widget_control,  global.image_draw_id, get_value=window
   wset, window
   plot_profil, *global.image, zoom=global.zoom
   end
 'Tools.Histo': begin
         widget_control, /hourglass
         wsize = .75
         sh_ximage_get_image_corners, global, xs, xe, ys, ye
         image = (*global.image)(xs:xe, ys:ye)
         index = where(image ne global.undefined_value, count)
         med = median(image[index])
         sig = sigma(image[index])
         bin = sig * 300 / count
         min = med-4*sig > min(image[index], max = max)
         max = max < med+3*sig
         nbin = fix(float(max-min)/bin)
         window,/free ,xs=wsize*640, ys=wsize*512,title='histogram'  
         h = histogram(image[index], min = min, max = max, bin = bin)
         nbin = n_elements(h)
         x = min + bin / 2. + findgen(nbin) * bin
         plot, x, h, psym = 10
         
   end
 'Tools.Contours': begin
   xcontour, *global.image
   end
 'Tools.3D Surface': begin
   x3dim, *global.image
   end
; 'Tools.FFT.Power Spectrum': begin
;       *global.image = power_spectrum(*global.image)
;       sh_ximage_display_all, global, /scale
;   end
; 'Tools.FFT.Phase': begin
;       f = dft(*save_global.image)
;       *global.image =atan(imaginary(f), float(f))
;       sh_ximage_display_all, global, /scale
;   end
; 'Tools.FFT.Real': begin
;       *global.image = float(dft(*save_global.image))
;       sh_ximage_display_all, global, /scale
;   end
; 'Tools.FFT.Imaginary': begin
;       *global.image = imaginary(dft(*save_global.image))
;       sh_ximage_display_all, global, /scale
;   end
 endcase
 
closing:
 sh_ximage_put_global, event, global
   
end

;----------------------------------------------------------------------

pro sh_ximage_zoom_event, event

 sh_ximage_get_global, event, global
 
 if global.no_data then goto, closing
 
 if event.value eq 'Zoom.Reset' then begin
    global.zoom = 1.
    global.xcorner = 0
    global.ycorner = 0
 endif else begin
    case event.value of 
       'Zoom.Zoom 2': begin
           fact = 2.
        end
       'Zoom.Zoom 4': begin
           fact= 4.
        end
       'Zoom.Zoom 1/2': begin
           fact = 0.5
        end
       'Zoom.Zoom 1/4': begin
           fact = 0.25
        end
    endcase
    if fact gt 1 then begin
       global.xcorner = global.xcorner + fix(global.image_draw_size/2.*(fact-1.)/fact/global.zoom/global.first_zoom)
       global.ycorner = global.ycorner + fix(global.image_draw_size/2.*(fact-1.)/fact/global.zoom/global.first_zoom)
    endif else begin                                               
       global.xcorner = global.xcorner - fix(global.image_draw_size/2.*(1/fact-1.)/global.zoom/global.first_zoom)
       global.ycorner = global.ycorner - fix(global.image_draw_size/2.*(1/fact-1.)/global.zoom/global.first_zoom)
    endelse
    global.zoom = global.zoom * fact
 endelse
 
 sh_ximage_display_all, global

closing:
 sh_ximage_put_global, event, global
  
end

;----------------------------------------------------------------------
;            thumbnail window
;----------------------------------------------------------------------

pro sh_ximage_get_corners_from_thumbnail, global, x, y, xc, yc

 xdim = global.image_size[0]
 ydim = global.image_size[1]
 xc = fix((x + global.thumb_xoffset < (global.thumbnail_size[0]-1) > 0 - $
           global.thumbnail_size_max/2./global.zoom)/ $
           global.thumbnail_size[0]*xdim)
 yc = fix((y + global.thumb_yoffset < (global.thumbnail_size[1]-1) > 0 - $
           global.thumbnail_size_max/2./global.zoom)/ $
           global.thumbnail_size[1]*ydim)

end

;----------------------------------------------------------------------

pro sh_ximage_get_thumb_offsets, global, x, y, xoffset, yoffset

 xdim = (size(*global.image))[1]
 ydim = (size(*global.image))[2]
 ; in the variable coordinates
 xcenter = global.xcorner + xdim / 2 / global.zoom
 ycenter = global.ycorner + ydim / 2 / global.zoom
 xoffset = fix(float(xcenter)/xdim*global.thumbnail_size[0] - x)
 yoffset = fix(float(ycenter)/ydim*global.thumbnail_size[1] - y)

end

;----------------------------------------------------------------------

pro sh_ximage_draw_square_in_thumbnail, global

 widget_control, global.base_thumbnail, get_value = window
 wset, window
 sh_ximage_get_image_corners, global, xs, xe, ys, ye
 xe = xe+1
 ye = ye+1
 px = fix([xs, xe, xe, xs, xs] * global.first_zoom / global.image_draw_size * global.thumbnail_size_max)
 py = fix([ys, ys, ye, ye, ys] * global.first_zoom / global.image_draw_size * global.thumbnail_size_max)
 plots, px-[-1,0,0,-1,-1], py-[0,0,1,1,0], col=sh_ximage_icolor('green'), /dev, thick=2, linestyle=0
 wset, window  ; idl 5.2 bug ... 

end

;----------------------------------------------------------------------

pro sh_ximage_display_thumbnail, global

 save_window, old
 widget_control, global.base_thumbnail, get_value = window
 wset, window
 thumbnail = global.thumbnail
 thumbnail = congrid(*global.scaled_image, global.thumbnail_size[0], global.thumbnail_size[1])
 tv, thumbnail
 sh_ximage_draw_square_in_thumbnail, global
 restore_window, old

end

;----------------------------------------------------------------------

pro sh_ximage_base_thumbnail_event, event

 sh_ximage_get_global, event, global
 widget_control, global.base_thumbnail, get_value = window
 wset, window
 
 if tag_exist(event, 'enter') then begin
    if event.enter then begin
       if global.no_data then curs = 88 else curs = 52
       device, cursor_standard = curs
       if global.no_data then goto, closing
       str = sh_ximage_get_data_info(global, global.xcross, global.ycross)
       widget_control, global.xy_label, set_value = str
    endif else begin
       device, /cursor_crosshair
       if global.no_data then goto, closing
    endelse
    goto, closing
 endif
 
 if global.no_data then goto, closing
 
 case event.type of
    0 : begin
           global.thumb_scroll_on = 1
           global.thumb_moved_on  = 0
           sh_ximage_get_thumb_offsets, global, event.x, event.y, xoffset, yoffset
           global.thumb_xoffset = xoffset
           global.thumb_yoffset = yoffset
        end
    1 : begin
           sh_ximage_get_thumb_offsets, global, event.x, event.y, xoffset, yoffset
           if not global.thumb_moved_on then begin
              global.thumb_xoffset = 0
              global.thumb_yoffset = 0
           endif
           sh_ximage_get_corners_from_thumbnail, global, event.x, event.y, xc, yc
           global.xcorner = xc
           global.ycorner = yc
           sh_ximage_display_all, global
           global.thumb_scroll_on = 0
           global.thumb_moved_on  = 0
        end
    2 : if global.thumb_scroll_on then begin
           sh_ximage_get_thumb_offsets, global, event.x, event.y, xoffset, yoffset
           if xoffset ne global.thumb_xoffset or yoffset ne global.thumb_yoffset then begin
              global.thumb_moved_on = 1
           endif
           device, set_graphics = 6, get_graphics = old_graph
           sh_ximage_draw_square_in_thumbnail, global
           sh_ximage_get_corners_from_thumbnail, global, event.x, event.y, xc, yc
           global.xcorner = xc
           global.ycorner = yc
           sh_ximage_draw_square_in_thumbnail, global
           device, set_graphics = old_graph
        endif
 endcase    
 
closing:
 sh_ximage_put_global, event, global
 
end

;----------------------------------------------------------------------
;                         loupe window
;---------------------------------------------------------------------

pro sh_ximage_display_loupe, global

 if global.no_data then return
 save_window, old
 wset, global.base_loupe_window

 x = round(global.x)
 y = round(global.y)

 global.base_loupe_npix = fix(global.image_draw_size / $
   global.first_zoom / global.zoom / global.loupe_zoom / 2.) * 2 + 1>  5
 boxsize = (global.base_loupe_npix - 1) / 2

 xsize = global.base_loupe_npix
 ysize = global.base_loupe_npix
 image = make_array(xsize,ysize, /byte, value = sh_ximage_icolor('white'))

 xmin = (0 > (x - boxsize))
 xmax = ((x + boxsize) < (global.image_size[0] - 1) )
 ymin = (0 > (y - boxsize) )
 ymax = ((y + boxsize) < (global.image_size[1] - 1))

 startx = abs( (x - boxsize) < 0 )
 starty = abs( (y - boxsize) < 0 ) 

 cut = (*global.scaled_image)[xmin:xmax, ymin:ymax]
 image[startx, starty] = cut

 image = congrid(image, global.thumbnail_size_max, global.thumbnail_size_max)
 tv, image

 xc = global.thumbnail_size_max / 2
 yc = global.thumbnail_size_max / 2
 plots, [xc-4, xc+4], [yc-4, yc+4], color = sh_ximage_icolor('green'), /dev, thick=1, linestyle=0
 plots, [xc-4, xc+4], [yc+4, yc-4], color = sh_ximage_icolor('green'), /dev, thick=1, linestyle=0
; wset, global.base_loupe_window
 
 restore_window, old
end

;----------------------------------------------------------------------

pro sh_ximage_loupe, event

 sh_ximage_get_global, event, global
 wset, global.base_loupe_window
 if tag_exist(event, 'enter') then begin
    if event.enter then begin
       if global.no_data then curs = 88 else curs = 38
       device, cursor_standard = curs
       if global.no_data then goto, closing
    endif else begin
       device, /cursor_crosshair
       if global.no_data then goto, closing
    endelse
 endif
 
closing:
 sh_ximage_put_global, event, global
 
end
 
;----------------------------------------------------------------------
;                         main window
;---------------------------------------------------------------------

pro sh_ximage_plot_cross, global

 sh_ximage_get_image_from_data, global, global.xcross, global.ycross, x, y
 if x-10 gt global.image_draw_size-1 or y-10 gt global.image_draw_size-1 or x+10 lt 0 or y+10 lt 0 then return
 
 widget_control, global.image_draw_id, get_value = window
 wset, window
 *global.save_cross = tvrd(x-10>0,y-10>0, x+10<global.image_draw_size-1-(x-10>0)+1, y+10<global.image_draw_size-1-(y-10>0)+1)
 plots, [x-9, x+9], [y, y], color = sh_ximage_icolor('black'), linestyle = 0, thick = 3, /device
 plots, [x, x], [y-9, y+9], color = sh_ximage_icolor('black'), linestyle = 0, thick = 3, /device
 plots, [x-9, x+9], [y, y], color = sh_ximage_icolor('white'), linestyle = 0, thick = 1, /device
 plots, [x, x], [y-9, y+9], color = sh_ximage_icolor('white'), linestyle = 0, thick = 1, /device
 
 wset, window
 
end

;----------------------------------------------------------------------

pro sh_ximage_remove_cross, global
 
 sh_ximage_get_image_from_data, global, global.xcross, global.ycross, x, y
 if  x-10 gt global.image_draw_size-1 or y-10 gt global.image_draw_size-1 or x+10 lt 0 or y+10 lt 0 then return
 
 widget_control, global.image_draw_id, get_value = window
 wset, window
 tv, *global.save_cross, x-10>0, y-10>0

end

;----------------------------------------------------------------------

pro sh_ximage_set_cross, global, x, y
 ; x,y data coordinate
 
 xdim = (size(*global.image))(1)
 ydim = (size(*global.image))(2)
 if x lt 0 or y lt 0 or x ge xdim or y ge ydim then return
 
 sh_ximage_remove_cross, global
 global.xcross = x
 global.ycross = y
 global.x = x
 global.y = y
 sh_ximage_plot_cross, global
 sh_ximage_display_loupe, global
 if global.cube_on then begin
    save_window, old
    restore_window, *global.cube_window
    sh_xraster_plot_cube, global
    restore_window, old
 endif
 if global.stats_on then sh_ximage_stats_refresh, global
 if global.phot_on then sh_ximage_mapphot_refresh, global
 
end

;----------------------------------------------------------------------

pro sh_ximage_arrow_event, event, ev

 sh_ximage_get_global, event, global
 
 if global.no_data then goto, closing
 
 case ev of
    'arrow_up'    : sh_ximage_set_cross, global, global.xcross, global.ycross + 1
    'arrow_left'  : sh_ximage_set_cross, global, global.xcross - 1, global.ycross
    'arrow_right' : sh_ximage_set_cross, global, global.xcross + 1, global.ycross
    'arrow_down'  : sh_ximage_set_cross, global, global.xcross, global.ycross - 1
 endcase
 
 str = sh_ximage_get_data_info(global, global.xcross, global.ycross)
 widget_control, global.xy_label, set_value = str
 
closing:
 sh_ximage_put_global, event, global

end

;--------------------------------------------------------------------

pro sh_ximage_base_image_event, event

 sh_ximage_get_global, event, global
 widget_control, global.image_draw_id, get_value = window
 wset, window ; necessary to get the cursor index
 
 if tag_exist(event, 'enter') then begin
    if event.enter then begin
       if global.no_data then curs = 88 else curs = 128
       device, cursor_standard = curs
       global.x_old = global.xcross
       global.y_old = global.ycross
    endif else begin
       device, /cursor_crosshair
       global.x = global.xcross
       global.y = global.ycross
       sh_ximage_display_loupe, global
       widget_control, global.xy_label, set_value=sh_ximage_get_data_info(global, global.xcross, global.ycross, out)
    endelse
    goto, closing
 endif
 
 if global.no_data then goto, closing
 
 global.xdevice = event.x
 global.ydevice = event.y
 
 case global.mousemode_id of
    global.datamode_id  : sh_ximage_base_image_datamode, global, event
    global.colormode_id : sh_ximage_base_image_colormode, global, event
    else :
 endcase
 
closing:
 sh_ximage_put_global, event, global

 
end

;----------------------------------------------------------------------

pro sh_ximage_base_image_datamode, global, event

 sh_ximage_get_data_from_image, global, global.xdevice, global.ydevice, x, y, out = out

 global.x = x
 global.y = y
 
 if not out then begin
    
    case event.press of
       
       ; no press
       0 :
       
       ; left button
       1 : 
       
       ; middle button
       2 : begin
              print, sh_ximage_get_data_info(global, global.xcross, global.ycross)
              if ptr_valid(global.raster) then sh_xraster_plot_cube, global, /print_info
           end
           
       ; right button
       4 : begin
              global.xcorner = global.xcross - fix(255/global.first_zoom/global.zoom)
              global.ycorner = global.ycross - fix(255/global.first_zoom/global.zoom)
              sh_ximage_display_all, global
           end
    endcase
    
    case event.release of
       0 : 
       1 : sh_ximage_set_cross, global, x, y
       else : 
    endcase
    
 endif 
 
 if global.x ne global.x_old or global.y ne global.y_old then begin
    sh_ximage_display_loupe, global
    widget_control, global.xy_label, set_value=sh_ximage_get_data_info(global, x, y, out)
    global.x_old = global.x
    global.y_old = global.y
 endif

end

;----------------------------------------------------------------------

pro sh_ximage_base_image_colormode, global, event

 case event.press of
    
    ; no press
    0 :
    
    ; left button
    1 : global.stretch_on = 1
    
    ; middle button
    2 : 
    
    ; right button
    4 : 
    
 endcase
    
 case event.release of
    0    : 
    1    : global.stretch_on = 0
    else : global.stretch_on = 0
 endcase
     
 if global.stretch_on then sh_ximage_stretch, global
 
end

;----------------------------------------------------------------------

pro sh_ximage_min, event

 sh_ximage_get_global, event, global
 if global.no_data then begin
    widget_control, global.display_min_id, set_value = ''
    goto, closing
 endif
 widget_control, global.display_min_id, get_value = min_str
 if not valid_num(min_str(0)) then goto, error
 if float(min_str(0)) ge global.threshold_max then goto, error
 global.threshold_min = float(min_str(0))
 if global.display_autozoom_on then begin
    global.display_autozoom_on = 0
    widget_control, global.display_autozoom_id, set_value = 'Fixed'
 endif
 sh_ximage_display_all, global, /scale
 goto, closing

error:
 min = strn(global.threshold_min)
 widget_control, global.display_min_id, set_value = strn(min)
 
closing:
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_max, event

 sh_ximage_get_global, event, global
 if global.no_data then begin
    widget_control, global.display_max_id, set_value = ''
    goto, closing
 endif
 widget_control, global.display_max_id, get_value = max_str
 if not valid_num(max_str(0)) then goto, error
 if float(max_str(0)) le global.threshold_min then goto, error
 global.threshold_max = float(max_str(0))
 if global.display_autozoom_on then begin
    global.display_autozoom_on = 0
    widget_control, global.display_autozoom_id, set_value = 'Fixed'
 endif
 sh_ximage_display_all, global, /scale
 goto, closing
 return

error:
 max = global.threshold_max
 widget_control, global.display_max_id, set_value = strn(max)

closing:
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_autozoom_display, event

 sh_ximage_get_global, event, global
 global.display_autozoom_on = 1-global.display_autozoom_on
 if global.display_autozoom_on then begin
    sh_ximage_display_all, global
    state = 'Auto'
 endif else begin
    state = 'Fixed'
 endelse
 widget_control, event.id, set_value = state
 sh_ximage_put_global, event, global
end

;----------------------------------------------------------------------

pro sh_ximage_reset_display, event

 sh_ximage_get_global, event, global
 global.threshold_min = global.image_range[0]
 global.threshold_max = global.image_range[1]
 global.display_autozoom_on = 0
 widget_control, global.display_autozoom_id, set_value = 'Fixed'
 sh_ximage_display_all, global, /scale
 sh_ximage_put_global, event, global
 
end

;----------------------------------------------------------------------
;                         statistics
;---------------------------------------------------------------------

pro sh_ximage_stats_cleanup, id
 
 widget_control, id, get_uvalue=base_global
 valid = widget_info(base_global, /valid_id)
 if not valid then return
 widget_control, base_global, get_uvalue=global, /no_copy
 widget_control, global.stats_button_id, set_button = 0 
 global.stats_on = 0
 widget_control, base_global, set_uvalue=global, /no_copy
 
end

;---------------------------------------------------------------------

pro sh_ximage_showstats, event

; brings up a widget window for displaying image statistics

 sh_ximage_get_global, event, global
 global.stats_on = event.select

 if global.stats_on then begin

    stats_base = $
      widget_base(/floating, $
                  group_leader = global.base_sh_ximage, $
                  /column, $
                  /base_align_center, $
                  title = 'image statistics', $
                  uvalue = 'stats_base')
    global.base_stats = stats_base
    
    stats_nbase = widget_base(stats_base, /row);, /base_align_center)
    stats_base1 = widget_base(stats_nbase, /column)
    stats_base1a = widget_base(stats_base1, /column)
    stats_base1b = widget_base(stats_base1, /column, frame=1)
    stats_base2 = widget_base(stats_nbase, /column)
    stats_base2a = widget_base(stats_base2, /column, frame=1)
    stats_zoombase = widget_base(stats_base, /column)

    tmp_string = strcompress('Image size:  ' + $
                             string(global.image_size[0]) + $
                             ' x ' + $
                             string(global.image_size[1]))

    size_label = widget_label(stats_base1a, value = tmp_string)

    tmp_string = strcompress('Image min:  ' + string(global.image_range[0]))
    min_label= widget_label(stats_base1a, value = tmp_string)
    tmp_string = strcompress('Image max:  ' + string(global.image_range[1]))
    max_label= widget_label(stats_base1a, value = tmp_string)

    global.base_statbox = $
      cw_field(stats_base1b, $
               /long, $
               /return_events, $
               title = 'Box size for stats:', $
               uvalue = 'statbox', $
               value = global.statboxsize, $
               xsize = 5)
    
    global.statxcenter_id = widget_label(stats_base1b, value = $
        'Box x center : '+strn(global.xcross))

    global.statycenter_id = widget_label(stats_base1b, value = $
        'Box y center : '+strn(global.ycross))

    tmp_string = strcompress('# pixels in box:  ' + string(100000))
    global.stat_npix_id = widget_label(stats_base2a, value = tmp_string)
    tmp_string = strcompress('Min:  ' + '0.00000000')
    global.statbox_min_id = widget_label(stats_base2a, value = tmp_string)
    tmp_string = strcompress('Max:  ' + '0.00000000')
    global.statbox_max_id = widget_label(stats_base2a, value = tmp_string)
    tmp_string = strcompress('Mean:  ' + '0.00000000')
    global.statbox_mean_id = widget_label(stats_base2a, value = tmp_string)
    tmp_string = strcompress('Median:  ' + '0.00000000')
    global.statbox_median_id = widget_label(stats_base2a, value = tmp_string)
    tmp_string = strcompress('Stddev:  ' + '0.00000000')
    global.statbox_stdev_id = widget_label(stats_base2a, value = tmp_string)
    
    global.base_showstatzoom = widget_button(stats_base2, $
          value = 'Show region', uvalue = 'showstatzoom')

    global.statzoom_widget_id = widget_draw(stats_zoombase, $
       scr_xsize = 1, scr_ysize = 1, retain = 2)

    child = widget_info(event.handler, /child)
    base_global = widget_info(child, /sibling)
    widget_control, stats_nbase, set_uvalue=base_global
    
    widget_control, stats_nbase, kill_notify = 'sh_ximage_stats_cleanup'
 
    widget_control, stats_base, /realize
    
    xmanager, 'sh_ximage_stats', stats_base, /no_block
    
    widget_control, global.statzoom_widget_id, get_value = tmp_val
    global.statzoom_window_id = tmp_val

    sh_ximage_stats_refresh, global
 
 endif else begin
    if xregistered('sh_ximage_stats') then begin
       base = global.base_stats
       sh_ximage_put_global, event, global
       widget_control, base, /destroy
       return
    endif
 endelse
 
 sh_ximage_put_global, event, global
 
end

;----------------------------------------------------------------------

pro sh_ximage_stats_refresh, global

; calculate box statistics and update the results

b = round((global.statboxsize - 1) / 2)

xmin = 0 > (global.xcross - b) < (global.image_size[0] - 1)
xmax = 0 > (global.xcross + b) < (global.image_size[0] - 1)
ymin = 0 > (global.ycross - b) < (global.image_size[1] - 1)
ymax = 0 > (global.ycross + b) < (global.image_size[1] - 1)

xmin = round(xmin)
xmax = round(xmax)
ymin = round(ymin)
ymax = round(ymax)

cut  = float((*global.image)[xmin:xmax, ymin:ymax])
npix = (xmax - xmin + 1) * (ymax - ymin + 1)

cutmin    = min(cut)
cutmax    = max(cut)
cutmean   = mean(cut)
cutmedian = median(cut)
if float(!version.release) ge 5.2 then cutstddev = stddev(cut) else $
   toto = moment(cut, sdev = cutstddev)  ; RG idl v5.0

widget_control, global.base_statbox, set_value=global.statboxsize
widget_control, global.statxcenter_id, set_value = 'Box x center : '+strn(global.xcross)
widget_control, global.statycenter_id, set_value = 'Box y center : '+strn(global.ycross)
tmp_string = strcompress('# pixels in box:  ' + string(npix))
widget_control, global.stat_npix_id, set_value = tmp_string
tmp_string = strcompress('Min:  ' + string(cutmin))
widget_control, global.statbox_min_id, set_value = tmp_string
tmp_string = strcompress('Max:  ' + string(cutmax))
widget_control, global.statbox_max_id, set_value = tmp_string
tmp_string = strcompress('Mean:  ' + string(cutmean))
widget_control, global.statbox_mean_id, set_value = tmp_string
tmp_string = strcompress('Median:  ' + string(cutmedian))
widget_control, global.statbox_median_id, set_value = tmp_string
tmp_string = strcompress('Stddev:  ' + string(cutstddev))
widget_control, global.statbox_stdev_id, set_value = tmp_string

sh_ximage_tvstats, global

end

;----------------------------------------------------------------------

pro sh_ximage_stats_event, event

 sh_ximage_subroutine_get_global, event, global

widget_control, event.id, get_uvalue = uvalue

case uvalue of

    'statbox': begin
        global.statboxsize = long(event.value) > 3
        if ( (global.statboxsize / 2 ) eq $
             round(global.statboxsize / 2.)) then $
          global.statboxsize = global.statboxsize + 1
        sh_ximage_stats_refresh, global
    end
    
    'showstatzoom': begin
        widget_control, global.base_showstatzoom, get_value=val
        case val of
            'Show region': begin
                widget_control, global.statzoom_widget_id, $
                  xsize=global.statzoom_size, ysize=global.statzoom_size
                widget_control, global.base_showstatzoom, $
                  set_value='Hide region'
            end
            'Hide region': begin
                widget_control, global.statzoom_widget_id, $
                  xsize=1, ysize=1
                widget_control, global.base_showstatzoom, $
                  set_value='Show region'
             end
         endcase
         sh_ximage_stats_refresh, global
    end

    else:
endcase

 sh_ximage_subroutine_put_global, event, global

end

;---------------------------------------------------------------------

pro sh_ximage_tvstats, global

; routine to display the zoomed region around a stats point

save_window, old
wset, global.statzoom_window_id
erase

x = round(global.xcross)
y = round(global.ycross)

boxsize = (global.statboxsize - 1) / 2
xsize = global.statboxsize
ysize = global.statboxsize
image = make_array(xsize,ysize, /byte, value = sh_ximage_icolor('white'))

xmin = (0 > (x - boxsize))
xmax = ((x + boxsize) < (global.image_size[0] - 1) )
ymin = (0 > (y - boxsize) )
ymax = ((y + boxsize) < (global.image_size[1] - 1))

startx = abs( (x - boxsize) < 0 )
starty = abs( (y - boxsize) < 0 ) 

cut = (*global.scaled_image)[xmin:xmax, ymin:ymax]

image[startx, starty] = cut

xs = indgen(xsize) + xmin - startx
ys = indgen(ysize) + ymin - starty

xs_delta = (xs[xsize-1] - xs[0]) / float(xsize - 1.0)
ys_delta = (ys[ysize-1] - ys[0]) / float(ysize - 1.0)
x_ran = [xs[0]-xs_delta/2.0,xs[xsize-1]+xs_delta/2.0]
y_ran = [ys[0]-ys_delta/2.0,ys[ysize-1]+ys_delta/2.0]

dev_width = 0.8 * global.statzoom_size
dev_pos = [0.15 * global.statzoom_size, $
           0.15 * global.statzoom_size, $
           0.95 * global.statzoom_size, $
           0.95 * global.statzoom_size]

x_factor = dev_width / xsize
y_factor = dev_width / ysize
x_offset = (x_factor - 1.0) / x_factor / 2.0
y_offset = (y_factor - 1.0) / y_factor / 2.0
xi = findgen(dev_width) / x_factor - x_offset ;x interp index
yi = findgen(dev_width) / y_factor - y_offset ;y interp index

image = poly_2d(image, [[0,0],[1.0/x_factor,0]], $
             [[0,1.0/y_factor],[0,0]], $
             0, dev_width, dev_width)

xsize = (size(image))[1]
ysize = (size(image))[2]
out_xs = xi * xs_delta + xs[0]
out_ys = yi * ys_delta + ys[0]

sz = size(image)
xsize = float(sz[1])       ;image width
ysize = float(sz[2])       ;image height
dev_width = dev_pos[2] - dev_pos[0] + 1
dev_width = dev_pos[3] - dev_pos[1] + 1

tv, image, /device, dev_pos[0], dev_pos[1], $
  xsize=dev_pos[2]-dev_pos[0], $
  ysize=dev_pos[3]-dev_pos[1]

plot, [0, 1], /noerase, /nodata, xstyle = 1, ystyle = 1, $
  /device, position = dev_pos, color=sh_ximage_icolor('black'), $
  xrange = x_ran, yrange = y_ran, back = sh_ximage_icolor('white')

restore_window, old

end

;----------------------------------------------------------------------
;                         sky views
;---------------------------------------------------------------------

pro sh_ximage_sky_views_event, event

 sh_ximage_get_global, event, global
 global.sky_views_on = event.select
 if not global.sky_views_on then begin
    sh_ximage_display_all, global
 endif else begin
    sh_ximage_remove_cross, global
    sh_xraster_sky_views, global
    sh_ximage_plot_cross, global
 endelse
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------
;                         help window
;---------------------------------------------------------------------

pro sh_ximage_help_event, event

 sh_ximage_get_global, event, global
 
 h = ['Sh_Ximage help', $
'', $
"Don't use sh_ximage and widget_olh at the same time !!!'", $
'It could crash your session ', $
'(SUN/OS IDL5.2)', $
'', $
'It will come out soon...']

 helptitle = strcompress('sh_ximage help')

 help_base =  widget_base(/floating, $
                          group_leader = global.base_sh_ximage, $
                          /column, $
                          /base_align_right, $
                          title = helptitle, $
                          uvalue = 'help_base')
 
 help_text = widget_text(help_base, $
                         /scroll, $
                         value = h, $
                         xsize = 85, $
                         ysize = 24)
 
 widget_control, help_base, /realize

 sh_ximage_put_global, event, global
    
end

;----------------------------------------------------------------------

pro sh_ximage_top_menu_event, event

 button = (str_sep(event.value, '.'))(0)

 case button of
    'File'   : sh_ximage_file_event, event
    'Export' : sh_ximage_export_event, event
    'Scale'  : sh_ximage_scale_event, event
    'Tools'  : sh_ximage_tools_event, event
    'Zoom'   : sh_ximage_zoom_event, event
    'Raster' : sh_ximage_raster_event, event
    'Help'   : sh_ximage_help_event, event
    else     : print, button
 endcase
 
end   

;----------------------------------------------------------------------

pro sh_ximage_colorbar, event

 common colors

 sh_ximage_get_global, event, global
 if global.no_data then goto, closing
 
 xloadct, bottom = global.rcolors, ncolors = global.ncolors, /silent, $
   group = event.top
 
closing:
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_resize_event, event

 sh_ximage_get_global, event, global
 new_size = [event.x, event.y]
 nds = max(new_size-global.image_draw_margin)
 widget_control, global.image_draw_id, draw_xsize = nds, $
   draw_ysize = nds
 widget_control, global.colorbar_id, draw_xsize = nds
 global.first_zoom      = float(nds) / global.image_draw_size * global.first_zoom
 global.image_draw_size = nds
 sh_ximage_display_all, global
 sh_ximage_put_global, event, global

end

;----------------------------------------------------------------------

pro sh_ximage_event, event

 widget_control,event.id,get_uvalue=ev
 
 sh_ximage_get_global, event, global
 if n_elements(global) eq 0 then return
 sh_ximage_put_global, event, global

 save_window, old
 
 case ev of 
 
    'base_sh_ximage'    : sh_ximage_resize_event, event
    'top menu'       : sh_ximage_top_menu_event, event
    'phot'           : sh_ximage_mapphot, event
    'stat'           : sh_ximage_showstats, event
    'sky views'      : sh_ximage_sky_views_event, event
    'base_thumbnail' : sh_ximage_base_thumbnail_event, event
    'arrow_up'       : sh_ximage_arrow_event, event, ev
    'arrow_left'     : sh_ximage_arrow_event, event, ev
    'arrow_right'    : sh_ximage_arrow_event, event, ev
    'arrow_down'     : sh_ximage_arrow_event, event, ev
    'base_text'      : 
    'base_image'     : sh_ximage_base_image_event, event
    'base_loupe'     : sh_ximage_loupe, event
    'min'            : sh_ximage_min, event
    'max'            : sh_ximage_max, event
    'color scale'    : sh_ximage_colorbar, event
    else             : print, ev
 
 endcase
 
 restore_window, old
  
end

;----------------------------------------------------------------------

pro sh_ximage, data, hd, equinox = equinox, group=group 

 ;*****************
 ; handling errors
 ;*****************

; catch, error_status
; if error_status ne 0 then begin
;    print, !err_string
;    help, /trace
;    goto, closing
; endif
 
 ;****************
 ; initialization
 ;****************
 
 sh_ximage_init_global, global
  
 defsysv, '!cir', exist=cir_exist
 if not(cir_exist) then begin
     !p.color      = 0
     !p.background = !d.table_size-1
 endif else begin
    save_window, old
 endelse
 sh_xraster_module
 ; sh_ximage_module  raw photometry
 sh_ximage_getct, global
 

 ;********************
 ; handling arguments
 ;********************

 if keyword_set(equinox) then global.equinox = equinox
 
 if n_params() gt 0 then begin
    ;if size(data, /type) eq 7 then begin
    if datatype(data, 2) eq 7 then begin  ; idl v5.0
       filename = data
       sh_ximage_load_file, global, filename, data, hd, ok = ok
       if not ok then begin
          sh_ximage_free_pointers, global
          return
       endif
    endif
    sh_ximage_load_variables, global, data, hd, ok = ok
    if ok then begin
       sh_ximage_init_variable, global
    endif else begin
       sh_ximage_free_pointers, global
       return
    endelse
 endif
 
 if n_elements(group) eq 0 then group=0
 
 
 ;*******************
 ; widget definition
 ;*******************
 
 junk   = { cw_pdmenu_s, flags:0, name:'' }
 
 base_sh_ximage = widget_base(group_leader=group, $
     /column, $
     title= global.package+'/ ESA/Cea-Saclay / Image Display', $
     uvalue='base_sh_ximage', $
     app_mbar = top_menu, $
     /tlb_size_events, $
     /align_center)
 global.base_sh_ximage = base_sh_ximage
 
 ; this is the base for which uvalue = global
 base_top = widget_base(global.base_sh_ximage, /row)
 
 base_top_left = widget_base(base_top, /col, /base_align_right)
 
 base_button   = widget_base(base_top_left, /row)
 
 top_menu_desc = [                          $
     { cw_pdmenu_s, 1, 'File' },           $
     { cw_pdmenu_s, 0, 'Load' },           $
     { cw_pdmenu_s, 0, 'Save' },      $
     { cw_pdmenu_s, 0, 'Plot Image' },     $
     { cw_pdmenu_s, 2, 'Quit' },           $
     { cw_pdmenu_s, 1, 'Scale' },          $
     { cw_pdmenu_s, 0, 'Linear' },         $
     { cw_pdmenu_s, 0, 'Log' },            $
     { cw_pdmenu_s, 2, 'Hist. Eq.' },      $
     { cw_pdmenu_s, 1, 'Tools' },          $
     { cw_pdmenu_s, 0, 'Find Maximum' },   $
     { cw_pdmenu_s, 0, 'Profile' },        $
     { cw_pdmenu_s, 0, 'Histo' },          $
     { cw_pdmenu_s, 0, 'Contours' },       $
     { cw_pdmenu_s, 2, '3D Surface' },     $
;     { cw_pdmenu_s, 1, 'fft' },            $
;     { cw_pdmenu_s, 0, 'power spectrum' }, $
;     { cw_pdmenu_s, 0, 'phase' },          $
;     { cw_pdmenu_s, 0, 'real' },           $
;     { cw_pdmenu_s, 2, 'imaginary' },       $
     { cw_pdmenu_s, 1, 'Zoom' },      $
     { cw_pdmenu_s, 0, 'Reset' }, $
     { cw_pdmenu_s, 0, 'Zoom 2' },    $
     { cw_pdmenu_s, 0, 'Zoom 4' },    $
     { cw_pdmenu_s, 0, 'Zoom 1/2' },  $
     { cw_pdmenu_s, 2, 'Zoom 1/4' },   $
     { cw_pdmenu_s, 1, 'Help' },   $
     { cw_pdmenu_s, 2, 'Sh_Ximage Help' }   $
 
 ]
 
 top_menu = cw_pdmenu(top_menu, top_menu_desc, $
                      /mbar, $
                      /help, $
                      /return_full_name, $
                      uvalue = 'top menu')

 global.display_min_id = $
   cw_field(base_top_left, /return_events, title = 'Min : ', $
            uvalue = 'min', value = '', xsize = 8)
    
 global.display_max_id = $
   cw_field(base_top_left, /return_events, title = 'Max : ', $
            uvalue = 'max', value = '', xsize = 8)
 
 base = widget_base(base_top_left, /row)
 global.display_autozoom_id = widget_button(base, value = 'Fixed', xsize = 70, event_pro = 'sh_ximage_autozoom_display')
 junk = widget_button(base, value = 'Reset', xsize = 70, event_pro = 'sh_ximage_reset_display')
 
 ; mousemode selection
 base_mousemode = widget_base(global.base_sh_ximage, /row)
 mousemodes = ['Data mode', 'Color mode', 'Overplot mode']
 mousemode_selector_id = widget_droplist(base_mousemode, value = mousemodes, uvalue = mousemodes, event_pro = 'sh_ximage_set_mousemode', /frame)
 global.mousemode_selector_id = mousemode_selector_id
 base_mousemode2 = widget_base(base_mousemode)

 ; colormode base
 global.colormode_id = widget_base(base_mousemode2, /row, map = 0)
 table_value = [0,1,3,13,15,16,26]
 table_name = ['Black / White', 'Blue / White', 'Red temperature', 'Rainbow', 'Stern special', 'Haze', 'Eos A']
 junk = widget_droplist(global.colormode_id, value = table_name, uvalue = table_value, event_pro = 'sh_ximage_set_color_table')
 widget_control, junk, set_droplist_select = 4
 junk = widget_button(global.colormode_id, value = 'Reset', event_pro = 'sh_ximage_reset_color_table')
 
 ; datamode base
 global.datamode_id     = widget_base(base_mousemode2, /row, /nonexclusive, map = 0)
 global.mousemode_id    = global.datamode_id
 global.cube_button_id  = widget_button(global.datamode_id, value='Raster', event_pro = 'sh_ximage_cube_button_event')

; global.phot_button     = widget_button(global.datamode_id, uvalue='phot', value='Photometry')
 ; raw photometry see sh_ximage_module
 
 global.stats_button_id = widget_button(global.datamode_id, uvalue='stat', value='Statistics')

 ; overplot base
 global.overplotmode_id = widget_base(base_mousemode2, /row, /nonexclusive, map = 0)
 global.skyviews_id     = widget_button(global.overplotmode_id, uvalue='sky views', value='Sky Views')
; global.compass_id      = widget_button(global.overplotmode_id, uname='compass', value='Compass', event_pro = 'sh_ximage_compass_event')
; global.scalebar_id     = widget_button(global.overplotmode_id, uname='scalebar', value='Scale',  event_pro = 'sh_ximage_scalebar_event')
 
 global.base_thumbnail = widget_draw(base_top, $
     button_events=1, $
     motion_events=1, $
     /tracking_events, $
     retain=1, $
     uvalue='base_thumbnail', $
     xsize = global.thumbnail_size_max, $
     ysize = global.thumbnail_size_max)
 
fleche = byte([                                                                                                                       $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255 ], $
 [   0,  0,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,  0 ], $
 [   0,  0,  0,255,255,255,255,255,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,255,255,255,255,255,  0,  0 ], $
 [   0,  0,  0,  0,255,255,255,255,255,255,255,255,  0,255,255,255,255,255,255,255,  0,255,255,255,255,255,255,255,255,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ], $
 [   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 ]  $
         ])
 base_arrow = widget_base(base_top)
 essai = widget_button(base_arrow, value = cvttobm(fleche),            uvalue = 'arrow_up',    xoffset = 40, yoffset = 0 +8)
 essai = widget_button(base_arrow, value = cvttobm(rotate(fleche, 1)), uvalue = 'arrow_left',  xoffset = 0,  yoffset = 37+8)
 essai = widget_button(base_arrow, value = cvttobm(rotate(fleche, 3)), uvalue = 'arrow_right', xoffset = 80, yoffset = 37+8)
 essai = widget_button(base_arrow, value = cvttobm(rotate(fleche, 2)), uvalue = 'arrow_down',  xoffset = 40, yoffset = 74+8)
 
 global.base_loupe = widget_draw(base_top, $
    button_events=1, $
    motion_events=1, $
    /tracking_events, $
    retain=1, $
    uvalue='base_loupe', $
    xsize = global.thumbnail_size_max, $
    ysize = global.thumbnail_size_max)
 
 small_font = '-*-fixed-*-*-*-*-10-*-*-*-*-*-*-*'
 my_font = '-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1'
 
 global.base_text = cw_field(base_sh_ximage, title = 'Title : ', value='', $
    uvalue='base_text',  xsize=48, ysize=1)
 
 global.xy_label = widget_label(base_sh_ximage, /align_center, $
    value = '', xsize = 500, ysize = 20, font = my_font, /frame)
 
 global.image_draw_id = widget_draw( global.base_sh_ximage, $
    button_events=1, $
    motion_events=1, $
    /tracking_events, $
    retain=1, $
    uvalue='base_image', $
    xsize = global.image_draw_size, $
    ysize = global.image_draw_size, /align_center)
 
 global.colorbar_id = widget_draw(global.base_sh_ximage, $
    retain=1, $
    xsize = global.image_draw_size, $
    ysize = global.colorbar_height, $
    button_events=1, uvalue='color scale', /align_center) 

 widget_control, base_top, kill_notify = 'sh_ximage_cleanup'
 
 widget_control, global.base_sh_ximage, /realize
 
 widget_control, global.colorbar_id, get_value = window
 global.colorbar_window = window
 widget_control, global.base_loupe, get_value = window
 global.base_loupe_window = window
 
 if not global.no_data then sh_ximage_init_widget, global
 widget_control, base_top, set_uvalue=global, /no_copy
 
widget_control, mousemode_selector_id, send_event = $
   {id: mousemode_selector_id, top:base_sh_ximage, handler: base_sh_ximage, index:0l}
 
; for debugging only  xmanager, 'sh_ximage', base_sh_ximage, catch=0
 xmanager, 'sh_ximage', base_sh_ximage, /no_block
 
; help, base_sh_ximage, mousemode_selector_id
 ; RG idl v5.0 I don't know but do it

 if cir_exist then restore_window, old  ; after xmanager !?!

end
