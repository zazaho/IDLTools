;+
; NAME:
;       CHANGE_IMAGE_SCALE
;
; PURPOSE:
;       This function changes the scale of an image.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;       new_image = CHANGE_IMAGE_SCALE(image,image_scale,new_image_scale)
;
; INPUTS:
;       image : 2D image
;       image_scale : the scale of image
;       new_image_scale : the new image scale
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       new_image : image interpolated to new_image_scale
;
; OPTIONAL OUTPUTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;       The ratio of the image scales is used to determine how to
;       interpolate image to new_image.  The program should correctly
;       handle any scale change, but it has only been tested for
;       making the scale of the image smaller (and thus the image
;       larger in pixels).
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by:	Karl Gordon w/ help from JD Smith (17 Feb 2004)
;       17 Feb 2005 : changed to use hcongrid
;
;-
function change_image_scale,image,header,image_scale_in,new_image_scale_in

size_image = size(image)

if ((size(image_scale_in))[0] EQ 0) then begin
    image_scale = [image_scale_in,image_scale_in]
endif else begin
    image_scale = image_scale_in
endelse
if ((size(new_image_scale_in))[0] EQ 0) then begin
    new_image_scale = [new_image_scale_in,new_image_scale_in]
endif else begin
    new_image_scale = new_image_scale_in
endelse

;print,image_scale
;print,new_image_scale
scale_ratio_x = float(image_scale[0])/new_image_scale[0]
scale_ratio_y = float(image_scale[1])/new_image_scale[1]
;print,scale_ratio_x,scale_ratio_y
n_x = abs(size_image[1]*scale_ratio_x)
n_y = abs(size_image[2]*scale_ratio_y)

if (scale_ratio_x*scale_ratio_y NE 1.0) then begin
;    print,n_x,n_y
    hcongrid,image,header,nimage,nheader,n_x,n_y,/half
    header = nheader
endif else begin
    nimage = image
endelse

return,nimage

end
