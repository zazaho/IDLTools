;+
; NAME:
;       CONV_IMAGE
;
; PURPOSE:
;       Convolve an image contained in a FITS file with a kernel while
;       updating the FITS header properly.  The image is assumed to be
;       in surface brightness units (e.g., MJy/sr).  If the unit is in
;       straight flux units (say Jy/pixel), then the /conserve_flux
;       keyword should be set.  This procedure was written for Karl
;       D. Gordon's personal use and there is no guarantee it will
;       work for you.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;       CONV_IMAGE,file,kernel_file,out_file
;
; INPUTS:
;       file : FITS file with image
;              assumed to be surface brightness units unless
;              conserve_flux keyword set
;       kernel_file : FITS file with convolution kernel
;       out_file : FITS file for convolved image
;
; KEYWORD PARAMETERS:
;       out_pixel_scale : pixel scale of output convolved image
;       image_scale : image scale in arcsec (otherwise use header)
;       conserve_flux : if set, image is assumed to be in flux units
;                       and flux is conserved
;       no_derotateion: if CD matrix in header of image file implies a
;                       rotation, the image is derotated unless this
;                       keyword is set
;       no_convolve : do everything but the convolution step
;       do_unc : convolve the uncertainty image as well (assuming it
;                is in the 2nd extension - probably only works for
;                                          MIPS DAT data)
;       no_shift : do not ensure that the kernel is centered
;       no_rebin : do not rebin before convolution to save memory 
;       silent : surpress all screen output (except error messages)
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; PROCEDURE:
;
; EXAMPLE:
;       conv_image,'irac_c4.fits','irac_c4_to_mips_24_100K.fits','irac_c4.conv.fits',/silent,/no_derotate
;
; MODIFICATION HISTORY:
; 	Written by  : Karl Gordon (18 Feb 2004)
;       27 Aug 2004 : Added image_scale parameter.
;       16 Aug 2005 : Added zeroing of rotation terms in CD matrix
;       27 Oct 2005 : added convolution of uncertaintly plane
;        3 Aug 2006 : Added rebinning (and no_rebin keyword) for speed
;       13 Oct 2006 : Updated documentation
;-
pro conv_image,file,kernel_file,out_file,conserve_flux=conserve_flux, $
               no_derotate=no_derotate,no_convolve=no_convolve, $
               out_pixel_scale=out_pixel_scale,silent=silent, $
               image_scale=image_scale, $
               do_unc=do_unc,no_shift=no_shift,no_rebin=no_rebin

; check we got at least 3 parameters
if (N_params() LT 3) then begin
    print, 'Syntax - conv_image,file,kernel_file,out_file'
    return
endif

; get image to convolve
if (file_test(file)) then begin
    fits_read,file,image,header
    if (keyword_set(do_unc)) then begin
        fits_read,file,image_unc,header_unc,exten_no=2
    endif
endif else begin
    print,'Image FITS file (' + file + ') not found.'
    return
endelse

; ensure image is a single plane
image = image[*,*,0]
sxaddpar,header,'NAXIS',2
sxdelpar,header,'NAXIS3'

; get astrometry
extast,header,ast_info
if (n_elements(ast_info) EQ 0) then begin
    ; add the astrometry to the header and then re-extract
    pixscale = fxpar(header,'PIXSCALE',count=count)/3600.0
    if (count EQ 0) then begin
        print,'trying SECPIX'
        pixscale = fxpar(header,'SECPIX',count=count)/3600.0
    endif
    sxaddpar,header,'CD1_1',-pixscale
    sxaddpar,header,'CD2_2',pixscale
    sxaddpar,header,'CD1_2',0.0
    sxaddpar,header,'CD2_1',0.0
    
    sxaddpar,header,'CRPIX1',fxpar(header,'X_POS')
    sxaddpar,header,'CRPIX2',fxpar(header,'Y_POS')
    
    sxaddpar,header,'CRVAL1',0.0
    sxaddpar,header,'CRVAL2',0.0
    
    sxaddpar,header,'CTYPE1','RA---TAN'
    sxaddpar,header,'CTYPE2','DEC--TAN'

    extast,header,ast_info
    if (n_elements(ast_info) EQ 0) then begin
        print,'There is not enough info in the header to determine the pixel scale.'
        print,'Aborting.'
        return
    endif
endif
getrot,header,angle,cdelt
orig_cdelt = cdelt

; derotate image if CD matrix indicates a rotation and user
; does not turn off option
if ((n_elements(no_derotate) EQ 0) AND (angle NE 0)) then begin
    if (n_elements(silent) EQ 0) then print,'image is rotated - derotating...'
    if (n_elements(silent) EQ 0) then print,'rotation angle = ', angle

    image_size = size(image)
    pad_image = fix(image_size[1]*0.20)
    if (keyword_set(pad_image)) then begin
        print,'padding image with ' + strtrim(string(pad_image),2) + ' pixels on all edges.'
        size_image = size(image)
        nimage = fltarr(size_image[1]+2*pad_image,size_image[2]+2*pad_image)
        nimage[*,*] = !values.f_nan
        nimage[pad_image:pad_image+size_image[1]-1,pad_image:pad_image+size_image[2]-1] = image
        image = nimage
        sxaddpar,header,'CRPIX1',fxpar(header,'CRPIX1')+pad_image
        sxaddpar,header,'CRPIX2',fxpar(header,'CRPIX2')+pad_image
        sxaddpar,header,'NAXIS1',fxpar(header,'NAXIS1')+pad_image*2
        sxaddpar,header,'NAXIS2',fxpar(header,'NAXIS2')+pad_image*2
    endif
    image_size = size(image)

    hrot, image, header, -1, -1, angle, image_size[1]/2, image_size[2]/2, 1, $
           mis=!values.f_nan, /pivot

    ; zero out rotation terms in CD matrix
    ;   slightly changes pixel scale and
    ;   caused problems for the cropping to a uniform size 
;    sxaddpar,header,'CD1_2',0.0
;    sxaddpar,header,'CD2_1',0.0
endif

; get astrometry
extast,header,ast_info
getrot,header,angle,cdelt
;print,'CD matrix'
;print,ast_info.cd
;print,'CDELT'
;print,ast_info.cdelt

; if the cdelts are not equal, then make them equal
if (abs(cdelt[0]) NE abs(cdelt[1])) then begin
    print,'making pixels square...'
    sq_pixscale = total(abs(cdelt))/2.0
    print,'current image scales = ',cdelt*3600.
    print,'new sq image scale = ',sq_pixscale*3600.
    new_image = change_image_scale(image,header,abs(cdelt),sq_pixscale)
    image = new_image
    
    getrot,header,angle,cdelt
    check_image_scale = abs(cdelt)*3600.0
    if (n_elements(silent) EQ 0) then print,'Sq image scale [arcsec] = ', check_image_scale

;    stop
endif

; if cdelt set to 1, use original
;print,ast_info.cd[0,0]-1,ast_info.cdelt[0]-1
if ((abs(ast_info.cd[0,0] - 1) LT 1e-10) AND (ast_info.cdelt[0] EQ 1.0)) then begin
    print,'using original CDELT (IRAC bug)'
    cdelt = orig_cdelt
    ast_info.cdelt = orig_cdelt
endif

; get the image pixel scale
if (not keyword_set(image_scale)) then begin
    image_scale = abs(cdelt)*3600.0
endif

out_pixel_scale_not_input = 0
if (n_elements(out_pixel_scale) EQ 0) then begin
    out_pixel_scale = total(image_scale)/n_elements(image_scale)
    out_pixel_scale_not_input = 1
endif

if ((not keyword_set(no_rebin)) AND ((out_pixel_scale/image_scale[0]) GE 3.0)) then begin
    print,'hrebining image before convolution for speed...'

    image_size = size(image)
    newx = 2.*image_size[1]*(image_scale[0]/out_pixel_scale)
    newy = 2.*image_size[2]*(image_scale[0]/out_pixel_scale)
    print,'old image size = ', image_size[1], image_size[2]
    print,'new image size = ', newx, newy
    print,'old image scale [arcsec] = ', image_scale

    hrebin,image,header,outsize=[newx,newy]

    extast,header,ast_info
    getrot,header,angle,cdelt
    image_scale = abs(cdelt)*3600.0
    print,'new image scale [arcsec] = ', image_scale
endif

;stop

if (n_elements(silent) EQ 0) then print,'Image scale [arcsec] = ', image_scale

if (not keyword_set(no_convolve)) then begin
    ; get kernel
    if (file_test(kernel_file)) then begin
        fits_read,kernel_file,psf_image,psf_header
    endif else begin
        print,'Kernel FITS file (' + kernel_file + ') not found.'
        return
    endelse

    ; ensure kernel has a total of 1 (conserves flux)
    psf_image = psf_image/total(psf_image)

    ; get the psf scale
;    extast,psf_header,psf_ast_info
;    stop
;    getrot,psf_ast_info,psf_rot,psf_cdelt
;    print,psf_rot
;    print,psf_cdelt
    psf_scale = fxpar(psf_header,'PIXSCALE',count=count)
    if (count EQ 0) then psf_scale = fxpar(psf_header,'SECPIX',count=count)
    if (count EQ 0) then psf_scale = abs(fxpar(psf_header,'CD1_1',count=count)*3600.0)
    if (count EQ 0) then psf_scale = abs(fxpar(psf_header,'CDELT1',count=count)*3600.0)
    if (count EQ 0) then begin
        print,'Can not find the pixel scale in the kernel file.'
        print,'Tried FITS keywords PIXSCALE, SECPIX, CD1_1, and CDELT1.'
        return
    endif

    if (n_elements(silent) EQ 0) then print,'PSF scale [arcsec] = ', psf_scale
endif else begin
    psf_scale = image_scale
endelse

; copy the header to the output header
out_header = header
if (keyword_set(do_unc)) then out_header_unc = header_unc

if (not keyword_set(no_convolve)) then begin
    ; put the psf image on the same scale as the image
    new_psf_image = change_image_scale(psf_image,psf_header,psf_scale,image_scale)

;    getrot,header,angle,cdelt
;    check_image_scale = abs(cdelt)*3600.0
;    if (n_elements(silent) EQ 0) then print,'Sq image scale [arcsec] = ', check_image_scale
;    stop

    if (not keyword_set(no_shift)) then begin
        ensure_psf_centered,new_psf_image
    endif

    ; renormalized the psf_image to have a sum of 1
    new_psf_image = new_psf_image/total(new_psf_image)
    ; put image on the same pixel scale as the kernel image
;    out_image = change_image_scale(image,image_scale,psf_scale)
    out_image = image
    if (keyword_set(do_unc)) then out_image_unc = image_unc^2

    ; remove NANs in image (FFTs don't like them!)
    indxs = where(finite(out_image) EQ 0,n_indxs)
    if (keyword_set(do_unc)) then indxs_unc = where(finite(out_image_unc) EQ 0,n_indxs_unc)

    if (n_indxs GT 0) then begin
        indxs2 = where(finite(out_image) EQ 1,n_indxs2)
        if (keyword_set(do_unc)) then indxs2_unc = where(finite(out_image_unc) EQ 1,n_indxs2_unc)
        if (n_indxs2 GT 0) then begin
            fill_val = median(out_image[indxs2])
            if (keyword_set(do_unc)) then fill_val_unc = median(out_image_unc[indxs2_unc])
        endif else begin
            fill_val = 0.0
        endelse
        out_image[indxs] = fill_val
        if (keyword_set(do_unc)) then out_image_unc[indxs_unc] = fill_val_unc
    endif

    ; convolve the image with the kernel
    out_image = convolve(out_image,new_psf_image)
    if (keyword_set(do_unc)) then out_image_unc = convolve(out_image_unc,new_psf_image)

    ; add the NANs back into image
    if (n_indxs GT 0) then begin
        out_image[indxs] = !values.f_nan
        if (keyword_set(do_unc)) then out_image_unc[indxs_unc] = !values.f_nan
    endif
;    fits_write,'test1.fits',out_image_unc
endif else begin
    out_image = image
    if (keyword_set(do_unc)) then out_image_unc = image_unc^2
endelse

; take the sqrt of the convolved image
if (keyword_set(do_unc)) then out_image_unc = sqrt(out_image_unc)

; put the image back on the original pixel scale unless otherwise
; indicated by out_pixel_scale keyword
if (out_pixel_scale_not_input) then out_pixel_scale = total(image_scale)/n_elements(image_scale)
out_image = change_image_scale(out_image,out_header,image_scale,out_pixel_scale)
if (keyword_set(do_unc)) then $
  out_image_unc = change_image_scale(out_image_unc,out_header_unc,image_scale,out_pixel_scale)
;fits_write,'test2.fits',out_image_unc

; conserve flux if asked
if (n_elements(conserve_flux) NE 0) then begin
    pix_ratio = mean(out_pixel_scale/image_scale)
    out_image = out_image*(pix_ratio)^2
endif

print,'changing image scale from ' + strtrim(string(image_scale,format='(F8.4)'),2) + $
      ' to ' + strtrim(string(out_pixel_scale,format='(F8.4)'),2)

; check the pixel scale
;extast,out_header,ast_info
getrot,out_header,angle,cdelt
check_image_scale = abs(cdelt)*3600.0
if (n_elements(silent) EQ 0) then print,'Final image scale [arcsec] = ', check_image_scale

; update output header for this scale change
;ast_info.crpix = (ast_info.crpix+0.5)*(image_scale/out_pixel_scale) + 0.5
;ast_info.crpix = (ast_info.crpix - 0.5)*(image_scale/out_pixel_scale)
;if (ast_info.cdelt[0] NE 1.0) then begin
;    ast_info.cdelt[0] = ast_info.cdelt[0]*(out_pixel_scale/image_scale[0])
;    ast_info.cdelt[1] = ast_info.cdelt[1]*(out_pixel_scale/image_scale[1])
;endif else begin
;    ast_info.cd[0,0] = ast_info.cd[0,0]*(out_pixel_scale/image_scale[0])
;    ast_info.cd[0,1] = ast_info.cd[0,1]*(out_pixel_scale/image_scale[1])
;    ast_info.cd[1,0] = ast_info.cd[1,0]*(out_pixel_scale/image_scale[0])
;    ast_info.cd[1,1] = ast_info.cd[1,1]*(out_pixel_scale/image_scale[1])
;endelse
;putast,out_header,ast_info

conv_fac = out_pixel_scale^2/(image_scale[0]*image_scale[1])
sxaddpar,out_header,'FLUXCONV',fxpar(out_header,'FLUXCONV')*conv_fac
sxaddpar,out_header,'ZEROPT',fxpar(out_header,'ZEROPT')*conv_fac

; output image
fits_open,out_file,out_fcb,/write
fits_write,out_fcb,out_image,out_header
if (keyword_set(do_unc)) then fits_write,out_fcb,out_image_unc,out_header_unc
fits_close,out_fcb

end
