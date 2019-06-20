function combine_cubes_rebin,flux,wave,newwave,sigma=sigma,isrms=isrms

  if n_elements(sigma) ne 0 then begin
     weight = 1d0/sigma
  endif else begin
     weight = flux*0d0+1d0
  endelse
  
  idx = sort(wave)
  f = flux[idx]
  w = wave[idx]
  p = weight[idx]

  if isrms then begin
     f = f^2
  endif
  
  idxnewwave = sort(newwave)
  nw = newwave[idxnewwave]
  
  ;; do a stupid loop over each bin
  nf = nw*!values.d_nan
  nnw = n_elements(nw)
  for i=0,nnw-1 do begin
     case i of
        0: begin
           idx = where(w le (nw[i]+nw[i+1])/2d0,cnt)
        end
        nnw-1: begin
           idx = where(w gt (nw[i-1]+nw[i])/2d0,cnt)
        end
        else: begin
           idx = where( $
                 (w gt (nw[i-1]+nw[i])/2d0) and $
                 (w le (nw[i]+nw[i+1])/2d0) $
                 ,cnt)
        end
     endcase
     if cnt ne 0 then begin
        nf[i] = total(f[idx]*p[idx])/total(p[idx])
     endif
  endfor

  newflux = nf[idxnewwave]

  if isrms then begin
     newflux = sqrt(newflux)
  endif

  return,newflux

end


;; ex: foo = combine_cubes('hii8/convolved_sl1.fits','hii8/convolved_sl2.fits','hii8/convolved_sl3.fits','hii8/convolved_ll1.fits','hii8/convolved_ll2.fits','hii8/convolved_ll3.fits',fitsout='hii8/combine_cube.fits')

function combine_cubes,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, $
                       pixelsize=pixelsize, $
                       writefits=writefits, $
                       fitsout=fitsout, $
                       treatsasrms=treatasrms, $
                       debug=debug, $
                       diagnostics=diagnostics, $
                       keepedge=keepedge
  
  if n_params() lt 2 then begin
     message,'need at least two fits-cube file names to combine',/info
     return,0
  endif
  
  default,pixelsize,'default'
  default,writefits,0
  default,treatasrms,0

  default,debug,0
  default,keepedge,0

  ;; use the first p1 as reference
  struct=readfitscube(p1)
  headout=struct.header
  headwaveout=struct.hwave
  
  sxaddpar,headout,'naxis',2
  sxdelpar,headout,'naxis3'
  extast,headout,astout
  
  ;; define a reference grid
  ;; do it properly.
  ;; find the extremeties of the given cubes
  ramin = 360d0
  ramax =   0d0
  demin =  90d0
  demax = -90d0
  
  minpixelsize = 1d9
  maxpixelsize = 0d0
  
  for i=1,n_params() do begin
     foo = execute('struct=readfitscube(p'+n2s(i)+')')
     cube=struct.cube
     head=struct.header
     xmax = n_elements(cube[*,0,0])-1
     ymax = n_elements(cube[0,*,0])-1
     
     xyad,head,[0,0,xmax,xmax],[0,ymax,ymax,0],ra,de
     extast,head,ast
     
     ramin = ramin < min(ra)
     ramax = ramax > max(ra)
     demin = demin < min(de)
     demax = demax > max(de)

     minpixelsize = minpixelsize < min(ast.cdelt*3600d0)
     maxpixelsize = maxpixelsize < max(ast.cdelt*3600d0)

  endfor
  
  ;; central values
  ra = (ramin+ramax)/2d0
  de = (demin+demax)/2d0
  
  case pixelsize of
     'default': begin
        pxsize = 2d0 ;; arcsec
     end
     'min': begin
        pxsize = minpixelsize
     end
     'max': begin
        pxsize = maxpixelsize
     end
     else: begin
        pxsize = double(pixelsize)
     end
  endcase
  
  pxsize_degree = pxsize/3600d0

  ;; use the largest cos(de) to calculate the Delta RA in arcsec
  maxcosde = max(cos([demin,demax]/180d0*!dpi))
  naxis1 = ceil((ramax-ramin) * maxcosde /pxsize_degree)
  naxis2 = ceil((demax-demin)            /pxsize_degree)

  astout.naxis=[naxis1,naxis2]
  astout.crpix=round([naxis1,naxis2]/2d0)
  astout.crval=[ra,de]
  astout.cd=[[-1,0],[0,1]]
  astout.cdelt=[pxsize_degree,pxsize_degree]
  putast,headout,astout,/naxis

  mask = make_array(naxis1,naxis2,value=0d0)
  out = mask

;; keep track of the number of orders that are present at each
;; location
  
  numberofvalidordersmask = mask*0
  
  waveout = 0d0
  waverange = [0d0,0d0]

  datafromparout=0

  if writefits ne 0 then begin
     case 1 of
        n_elements(fitsout) ne 0: begin
           outfitsname = strcompress(string(fitsout[0]),/remove_all)
        end
        size(writefits,/tname) eq 'STRING': begin
           outfitsname = strcompress(string(writefits[0]),/remove_all)
        end
        else: begin
           basename  = stregex(strcompress(p1,/remove_all),'(.*).fit.*',/fold_case,/extract,/subexp)
           if n_elements(basename) eq 2 then begin
              basename=basename[0]
           endif else begin
              basename='./anonymous'
           endelse
           outfitsname = basename+'_combined_cube.fits'
        end
     endcase        
  endif
        
  ;; remap all to reference grid
  for i=1,n_params() do begin
     foo = execute('struct=readfitscube(p'+n2s(i)+')')
     cube=struct.cube
     head=struct.header
     wave=struct.wave

     sxaddpar,head,'naxis',2
     sxdelpar,head,'naxis3'
     waverange = [[waverange],[min(wave),max(wave)]]
     validdatamask=mask*0d0

     ;; some logic to determine from which order the data are
     ;; so that we can apply the wavelength limits on which data to
     ;; exclude

     currentrange = [min(wave),max(wave)]

     SL2range = [ 5.23, 7.49]
     SL3range = [ 7.4 , 8.5 ]
     SL1range = [ 7.53,14.02]
     LL2range = [14.0 ,20.52]
     LL3range = [19.8 ,21.5 ]
     LL1range = [20.5 ,37.4 ]

     ; determine the order by figuring out which range matches best
     ; i.e. which order has the closest-by furthest distance

     SL2maxdist = max(abs(SL2range-currentrange))
     SL3maxdist = max(abs(SL3range-currentrange))
     SL1maxdist = max(abs(SL1range-currentrange))
     LL2maxdist = max(abs(LL2range-currentrange))
     LL3maxdist = max(abs(LL3range-currentrange))
     LL1maxdist = max(abs(LL1range-currentrange))

     ;; smallest maximum distance
     bestmaxdist = min([SL2maxdist,SL3maxdist,SL1maxdist,LL2maxdist,LL3maxdist,LL1maxdist])
     
     case 1 of
        bestmaxdist eq SL2maxdist: orderrange = SL2range
        bestmaxdist eq SL3maxdist: orderrange = SL3range
        bestmaxdist eq SL1maxdist: orderrange = SL1range
        bestmaxdist eq LL2maxdist: orderrange = LL2range
        bestmaxdist eq LL3maxdist: orderrange = LL3range
        bestmaxdist eq LL1maxdist: orderrange = LL1range
        else: orderrange = [0.,100.]
     endcase

     for j=0,n_elements(cube[0,0,*])-1 do begin
        ;; only treat the wavelength planes that are within the limits
        ;; (orderrange)
        if (wave[j] ge orderrange[0]) and (wave[j] le orderrange[1]) then begin
           ;;hastrom,reform(cube[*,*,j]),head,tmpimg,tmphead,headout,missing=!values.d_nan
           if j eq 0 then dodebug=debug else dodebug=0
           hswarp,reform(cube[*,*,j]),head,tmpimg,tmphead,headout,/fixheader,debug=dodebug,keepedge=keepedge
           ;; stick them together
           out = [[[out]],[[tmpimg]]]
           waveout = [waveout,wave[j]]
           
           ;; keep track of from which par the data at this wavelength
           ;; came from (line order)
           datafromparout=[datafromparout,i]
           
           ;; check where there are some valid pixels at this layer:
           valid = finite(tmpimg) and (tmpimg ne 0)
           ;; add the valid pixels to the validdatamask
           validdatamask=(validdatamask or valid)
        endif
     endfor
     ;; add one to any pixel which has some data define from this cube
     numberofvalidordersmask=numberofvalidordersmask+(validdatamask ne 0)
  endfor
  
  ;; take off the bogus first element
  out = reform(out[*,*,1:*])
  waveout = reform(waveout[1:*])
  datafromparout=reform(datafromparout[1:*])

;;; make the mask 1=has some data, 2=has maximum data
;  finiteout = total(finite(out[*,*,1:*]),3)
;  somegood =  finiteout ne 0
;  allgood  =  finiteout eq max(finiteout)
;
;  mask = somegood*1 + allgood*2
  mask = 1*(numberofvalidordersmask ne 0) + 2*(numberofvalidordersmask eq max(numberofvalidordersmask))

  if keyword_set(diagnostics) then begin
     ;; plot the sum of all the pixel which have all the orders
     ;; defined so that we can check how well the orders match globally
     alldefinedmask = numberofvalidordersmask eq max(numberofvalidordersmask)

     nalldefinedmask=total(alldefinedmask)

     totalspec = make_array(n_elements(waveout))
     for i=0,n_elements(totalspec)-1 do begin
        totalspec[i]=total(out[*,*,i]*alldefinedmask)/nalldefinedmask
     endfor

     psfile = fitsout+'.ps'
     sh_ps,psfile,/a4land,/color,thick=3
     
     pl,waveout,totalspec,/nodata,ymin=0,/jy
     
     for i=1,n_params() do begin
        pl,waveout,totalspec,idx=where(datafromparout eq i),/opl,ps=0
     endfor
     
     sh_ps

     ;; also write a diagnostic fits file where the orders are kept
     ;; and the rebinning in wavelength is not yet done 
     if writefits ne 0 then begin
        outstruct={cube:out,header:headout,wave:waveout,hwave:headwaveout,order:datafromparout,mask:mask}
        fext=file_extension(outfitsname,rootname=froot)
        outfitsnamediag = froot+'_diagnostic.'+fext
        writefitscube,outstruct,outfitsnamediag
     endif

  endif     
  
  ;; sort along the wavelengths
  idx=sort(waveout)
  waveout = waveout[idx]
  out = out[*,*,idx]
  
  ;; in order to a more regularly sampled grid rebin the cube in the
  ;; spectral domain.
  
  ;; make a spectral coverage map and average those parts wih coverage > 1
  
  ;; determine which parts of the wavelengths are covered how many times
  wavecoverage = waveout*0
  for i = 0,n_elements(waverange[0,*])-1 do begin
     idx = where((waveout ge waverange[0,i]) and (waveout le waverange[1,i]))
     wavecoverage[idx] = wavecoverage[idx]+1
  endfor

  ;; define an ordinal number for each wavelength such that each
  ;; ordinal corresponds to the wavelengtn point if the spectral
  ;; coverage were 1 everywhere. i.e take smaller steps if the
  ;; coverage is more that one (1/coverage)
  ordinal = total(1d0/wavecoverage,/cumulative)

  ;; interpolate the wavelength grid to the ordinate with integer
  ;; steps, effectively making a coverage of 1 every where
  
  newordinal = dindgen(ceil(max(ordinal))-1)+1d0
  
  newwaveout = interpol(waveout,ordinal,newordinal)
  
  nx = n_elements(out[*,0,0])
  ny = n_elements(out[0,*,0])
  nw = n_elements(newwaveout)
  
  newout = make_array(nx,ny,nw)
  
  ;; dont know yet how to do this properly
  for i = 0,nx-1 do begin
     for j =  0,ny-1 do begin
        newout[i,j,1:*] = combine_cubes_rebin(reform(out[i,j,1:*]),reform(waveout[1:*]),reform(newwaveout[1:*]),isrms=treatasrms)
     endfor
  endfor
  
  ;; now that we have the resampled cube replace the original one
  out = newout[*,*,1:*]
  waveout = newwaveout[1:*]

; reput the header keywords  
  sxaddpar,headout,'naxis',3
  sxaddpar,headout,'naxis3',n_elements(waveout),after='naxis2'

;; weed out images without valid data
  finitewave = total(total(finite(out),1),1)
  idx = where(finitewave ne 0)
  out = out[*,*,idx]
  waveout = waveout[idx]

  outstruct = {cube:out,header:headout,wave:waveout,hwave:headwaveout,mask:mask}

  if writefits ne 0 then begin
     writefitscube,outstruct,outfitsname
  endif
  
  ;; return full structure with header etc
  return,outstruct
  
end
