PRO plspectra,in,data=data, $
           modust=modust, $
           sws=sws, $
           unred=unred, $
           _extra=_extra

  IF n_elements(data) NE 0 THEN BEGIN
     name = object(data)
     havedata = 1
  ENDIF ELSE BEGIN
     name='Modust Model'
     havedata=0
  ENDELSE
  
  CASE 1 OF 
     keyword_set(modust): BEGIN
        pl,in,/ll,ps=0,xrange=10d0^[-1d0,4d0],yrange=10d0^[-7d0,4d0], $
           ymin=1d-7,_extra=_extra,/nodata,title=name
     END
     keyword_set(sws): BEGIN
        pl,in,ps=0,xrange=[2.3,45.],ymin=0, $
           _extra=_extra,/nodata,title=name
     END 
     ELSE: BEGIN
        pl,in,/iras,/xlog,ps=0,_extra=_extra
     END
  ENDCASE 
  
  IF havedata THEN BEGIN
     IF n_elements(unred) NE 0 THEN BEGIN
        tmpdata = spectra_unred(data,A_V=unred)
     ENDIF ELSE BEGIN
        tmpdata = data
     ENDELSE
     pl,tmpdata,/opl,ps=4,_extra=_extra
  ENDIF
  
  pl,in,/opl,ps=0,thick=3,_extra=_extra
  
END

;; This function takes the model and the data and tries to determine
;; which factor should be applied such that the flux level of the
;; model matches the observed flux level.
;;
;; Strategy:
;; Make a polynomial fit to the data and the model and determine the
;; factor which makes the model match at the reference wavelength.
;;
;; Which reference we use is just the middle of the mrange
;;
;; There are several cases to consider:
;; 1) the range contains < 2 points in the set to fit
;; 2) the range contains 2> points in the set to fit
;; ad
;; 1) do a straight linear interpolation to the reference wavelength
;; 2) fit a polynomial to the point in the range and determine the
;; value at the reference wavelength from the fit

FUNCTION read_spectra_match_data, model,data,mrange=mrange

  default,mrange,[3.5,4.5]
  
  max_mrange = max(mrange,min=min_mrange)
  
  l_ref = (max_mrange+min_mrange)/2d0
  
  ;; first we treat the model
  mwave = model.full.data.wave
  mflux = model.full.data.flux

  idx_model = where( (mwave GE min_mrange) AND $
                     (mwave LE mAX_mrange), cnt)
  
  IF cnt LT 2 THEN BEGIN
     f_ref_model = interpol(mflux,mwave,l_ref,/spline)
  ENDIF ELSE BEGIN
     fitpars_model = poly_fit(mwave[idx_model],mflux[idx_model],1)
     f_ref_model = fitpars_model[0]+l_ref*fitpars_model[1]
  ENDELSE

  ;; next the data
  dwave = data.data.wave
  dflux = data.data.flux

  idx_data = where( (dwave GE min_mrange) AND $
                    (dwave LE max_mrange), cnt)
  
  IF cnt LT 2 THEN BEGIN
     f_ref_data = interpol(dflux,dwave,l_ref,/spline)
  ENDIF ELSE BEGIN
     fitpars_data = poly_fit(dwave[idx_data],dflux[idx_data],1)
     f_ref_data = fitpars_data[0]+l_ref*fitpars_data[1]
  ENDELSE

  ;; now determine the normilisation factor
  match_factor =  f_ref_data/f_ref_model

  return,match_factor
  
END

FUNCTION read_spectra_datatable, status=status,prefix=prefix
  
  default,prefix,'/home/nele/thesispcklas/'
  template=prefix+'/startable_template.xdr'
  StarTable=prefix+'/startable.dat'
  
  ;; the status is set to -1 nothing could be read
  ;; 0 template read but not table
  ;; 1 success
  status =-1
  
  IF file_test(template,/read,/regular) THEN BEGIN
     restore,template
     status=0
  ENDIF ELSE BEGIN
     message,'The template file: '+template+' cannot be found',/info
     status = -1
     return,0
  ENDELSE

  IF file_test(StarTable,/read,/regular) THEN BEGIN
     stable = read_ascii(StarTable,template=StarTable_template)
  ENDIF ELSE BEGIN
     message,'The start tables file: '+StarTable+' cannot be found',/info
     status = 0
     return,0
  ENDELSE
  
  status = 1
  return,stable
  
END

FUNCTION read_spectra_data,starname,StarTable,prefix=prefix
  
  default,prefix,'/home/nele/thesispcklas/DATA_FILES/'
  match = where(strcompress(STRUPCASE(starname),/remove_all) EQ $
                strcompress(STRUPCASE(StarTable.naamster),/remove_all),cnt)
  
  IF cnt NE 0 THEN BEGIN
     datafile = prefix+'/'+StarTable.naamkort[match]+'.dat'
     IF file_test(datafile,/regular,/read) THEN BEGIN
        data        = read_fdat(datafile)
     ENDIF ELSE BEGIN
        message,/info,'the data file :'+datafile+' could not be found'
        data =-1
     ENDELSE
  ENDIF
  return,data
END

;; Given a list of lines with reading commands:
;; model234 = readXXX("hsaddjahgdjadgajda/sdjadhkdha/sdjadkadhada/")
;; find the first matching line and execute it. Return the read in value
FUNCTION read_spectra_from_path,readlines,path,prefix=prefix
  idx = where(strpos(readlines,path) NE -1,cnt)
  IF cnt NE 0 THEN BEGIN
     cmd = 'out'+strmid(readlines[idx[0]],strpos(readlines[idx[0]],'='))
     ;; do we need to add some part to the dir to be able to read it
     IF keyword_set(prefix) THEN BEGIN
        IF prefix NE '' THEN BEGIN
           ;; is the path relative?
           IF strmid(path,0,1) NE '/' THEN BEGIN
              part1 = strmid(cmd,0,strpos(cmd,path))
              part2 = strmid(cmd,strpos(cmd,path)+strlen(path))
              cmd = part1+prefix+'/'+path+part2
           ENDIF
        ENDIF
     ENDIF
     foo = execute(cmd)
     return,out
  ENDIF ELSE BEGIN
     return,0
  ENDELSE
END


;; This function takes two arrays of strings and returns an index of
;; those elements in the first array that has occurances of all second
;; elements in it
FUNCTION read_spectra_match_strings,sLong,sFind
  idx = make_array(n_elements(sLong),value=1)
  FOR i=0,n_elements(sFind)-1 DO BEGIN
      idx = idx AND (strpos(sLong,sFind[i]) NE -1)
  ENDFOR
  return,where(idx)
END


;; Given a parameter name find all the possible values corresponding
;; to that parameter
FUNCTION read_spectra_values_from_param,allparams,allvalues,param,numeric=numeric

  ;; do a sorting on the basis of the numeric values of the parameters.
  ;; default is on

  default,numeric,1
  idx = where(allparams EQ param[0],cnt)
  IF cnt NE 0 THEN BEGIN
      values = allvalues[where(allparams EQ param[0])]
      values = sh_uniq(values)
      IF n_elements(numeric) NE 0 THEN BEGIN
          values = values[sort(double(values))]
      ENDIF
  ENDIF ELSE BEGIN
      message,'no values found matching the requested param',/info
      values = ''
  ENDELSE
  return,values
END


;; function to read a batch file of IDL commands
FUNCTION read_spectra_read_batch,filename

  IF NOT file_test(filename,/read) THEN BEGIN
      message,'cannot read file, quitting.'
  ENDIF

;; do the actual reading of the data
  line=''
  alllines = ['']
  
  openr,lun,filename,/get_lun
  WHILE NOT eof(lun) DO BEGIN
      readf,lun,line,format='(a500)'
      alllines = [alllines,line]
  endwhile
  close,lun
  free_lun,lun
  
  IF n_elements(alllines) GT 1 THEN begin
      alllines = alllines[1:*]
  ENDIF ELSE begin
      message,'empty file, quitting.'
  endelse

  return, alllines
END

;; This procedure takes as input the contents of a batch file for
;; reading models and returns from this the various pieces of info
;; that can be found in there, like the models, readcommands, unique
;; param+valuecombinations, unique params and the paths to each model
PRO read_spectra_parse_file,alllines,separator=separator, $
  command=command, readlines=readlines, models=models, paths=paths, $
  params=params, values=values, uniq_params=uniq_params
  
  idx = where( (strpos(alllines,command) NE -1) AND (strpos(alllines,'=') NE -1) AND $
               ((strpos(alllines,'"') NE -1) OR (strpos(alllines,"'") NE -1)),cnt)
  IF cnt EQ 0 THEN BEGIN
      message,'No valid '+command+' statements in the specified file, quitting'
  ENDIF
  readlines = alllines[idx]
  
;; now extract the paths
  paths  = readlines
  models  = readlines
  
  FOR i=0,n_elements(paths)-1 DO BEGIN
      equalsign = strpos(readlines[i],'=')
      first_quote = strpos(readlines[i],'"')
      IF first_quote NE -1 THEN BEGIN
          second_quote = strpos(readlines[i],'"',first_quote+1)
      ENDIF ELSE BEGIN
          ;; assume string inside ''
          first_quote = strpos(readlines[i],"'")
          second_quote = strpos(readlines[i],"'",first_quote+1)
      ENDELSE
      paths[i] = strmid(readlines[i],first_quote+1,second_quote-first_quote-1)
      models[i] = strmid(readlines[i],0,equalsign)
  ENDFOR 
  
  ;; stick them all together for later processing
  stuck = ['']
  FOR i=0,n_elements(paths)-1 DO BEGIN
      stuck=[stuck, strsplit(paths[i],'/',/extract)]
  ENDFOR 
  
  directories=stuck[1:*]
  
  idx = where(strpos(directories,separator) NE -1,cnt)
  IF cnt EQ 0 THEN BEGIN
      message,'No '+separator+' found in the specified paths, quitting'
  ENDIF
  
  directories = directories[idx]
  
  ;; Now only take each directory once:
  directories = sh_uniq(directories)
  
  params = directories
  values = directories
  
  FOR i=0,n_elements(directories)-1 DO BEGIN
      params[i] = strsplit(directories[i],':.*',/regex,/extract)
      values[i] = strsplit(directories[i],'.*:',/regex,/extract)
  ENDFOR

  uniq_params = sh_uniq(params)
  
  ;; now eleminate those that do not have a choice i.e. only one value
  idx = make_array(n_elements(uniq_params),val=0)
  FOR i =0,n_elements(uniq_params)-1 DO BEGIN
      idx[i] = (n_elements(read_spectra_values_from_param(params,values,uniq_params[i])) GT 1)
  ENDFOR
  uniq_params = uniq_params[where(idx NE 0)]
  
END



;; The function that does all the work inside the widget.
;; Every event enter here and it branches depending on the eventUValue
PRO read_spectra_Event, sEvent
  
  ;; Which event?
  WIDGET_CONTROL, sEvent.id, GET_UVALUE=eventUValue
  
  ;; get the info structure from the top
  WIDGET_CONTROL, sEvent.top, GET_UVALUE=sInfo,/NO_COPY

  ;; First test the event value to see if it came from the combobox.
  IF strpos(eventUValue,'COMBOBOX') NE -1 THEN BEGIN
      comboidx = fix(strsplit(eventUValue,'COMBOBOX',/extract))
      eventUValue = 'COMBOBOX'
  ENDIF
  
  CASE eventUValue OF
      
     'COMBOBOX': BEGIN
        
        ;;remember the oldcriterium for the parameter
        oldcrit = sInfo.selectioncrit[comboidx]
        oldpath = sInfo.SelectedPath
        
        ;; Set the selected criterium
        IF sEvent.Index NE 0 THEN BEGIN
           sInfo.selectioncrit[comboidx] = sInfo.uniq_params[comboidx]+sInfo.separator+ $
                                           (read_spectra_values_from_param(sInfo.params,sInfo.values,sInfo.uniq_params[comboidx]))[sEvent.Index-1]
        ENDIF ELSE BEGIN
           sInfo.selectioncrit[comboidx] = ''
        ENDELSE
        
        ;; and the new criterium
        newcrit = sInfo.selectioncrit[comboidx]
        
        IF newcrit NE oldcrit THEN BEGIN
           ;; Now select only those paths that contain the criterium
           keep = read_spectra_match_strings(sInfo.paths,sInfo.selectioncrit)
           IF keep[0] NE -1 THEN begin
              WIDGET_CONTROL,sInfo.wList,set_value=sInfo.models[keep]
              WIDGET_CONTROL,sInfo.wParamList,set_value=['']
              
              ;; Now try to find the model like the originally clicked
              ;; one but with the selected parameter updated to the
              ;; current value
              IF (newcrit EQ '') THEN BEGIN
                 newpath = oldpath
              ENDIF ELSE BEGIN
                 IF oldcrit EQ '' THEN BEGIN
                    ;; Here we should change the param anyway
                    ;; like a fake oldcrit
                    foo = strsplit(oldpath,'/',/extract)
                    idx =where(strpos(foo,sInfo.uniq_params[comboidx]+sInfo.separator) EQ 0,cnt)
                    IF cnt NE 0 THEN BEGIN
                       oldcrit = foo[idx[0]]
                    ENDIF ELSE BEGIN
                       ;; something stinks we should not be
                       ;; here
                       oldcrit = 'ohoho something stinks'
                    ENDELSE
                 ENDIF
                 newpath = repstr(oldpath,oldcrit,newcrit)
              ENDELSE

              ;; find the right element to click
              ;; This does not work for non independent variables
              ;; like T_star,R_star. Do we want to fudge it? Yes
              ;; see below. We search for the best match if a
              ;; total match cannot be found.
              newidx = where(sInfo.paths[keep] EQ newpath,cnt)
              IF cnt NE 0 THEN BEGIN
                 WIDGET_CONTROL,sInfo.wList,SET_LIST_SELECT=newidx
                 newidx = LONG(newidx[0])
                 WIDGET_CONTROL,sInfo.wList, $
                                SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:newidx, CLICKS:1L}
              ENDIF ELSE BEGIN
                 ;; try to find the closest matching model
                 possible_paths = sInfo.paths[keep]
                 split_newpath = strsplit(newpath,'/',/extract)
                 matches = make_array(n_elements(possible_paths),value=0)
                 FOR i=0,n_elements(matches)-1 DO BEGIN
                    matches[i] = total(strsplit(possible_paths[i],'/',/extract) EQ split_newpath)
                 ENDFOR
                 max_match = max(matches,newidx)
                 ;; If there is only one model which matches the
                 ;; closest it means we had dependent variables
                 ;; so choose this one if there are multiple
                 ;; options select none.
                 IF total(matches EQ max_match) EQ 1 THEN BEGIN
                    WIDGET_CONTROL,sInfo.wList,SET_LIST_SELECT=newidx
                    newidx = LONG(newidx[0])
                    WIDGET_CONTROL,sInfo.wList, $
                                   SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:newidx, CLICKS:1L}
                 ENDIF
              ENDELSE
           ENDIF ELSE BEGIN
              WIDGET_CONTROL,sInfo.wList,set_value=['']
              WIDGET_CONTROL,sInfo.wParamList,set_value=['']
           ENDELSE
        ENDIF
     END
      
     'LIST': BEGIN
        keep = read_spectra_match_strings(sInfo.paths,sInfo.selectioncrit)
        IF keep[0] NE -1 THEN BEGIN
           SelectedPaths = sInfo.Paths[keep]
           IF sEvent.Clicks EQ 2 THEN BEGIN
              IF sInfo.no_block EQ 0 THEN BEGIN
                 *sInfo.ptrToFormData ={cancel:0,path:SelectedPaths[sEvent.Index]}
                 Widget_Control, sEvent.top, /Destroy
                 return
              ENDIF
           ENDIF ELSE BEGIN
              ;; Update the ParamList to show the dirs i.e. the param
              ;; values of the clicked model
              sInfo.SelectedPath = SelectedPaths[sEvent.Index]
              dirs = strsplit(SelectedPaths[sEvent.Index],'/',/extract)
              WIDGET_CONTROL,sInfo.wParamList,SET_VALUE=dirs
           ENDELSE
           WIDGET_CONTROL, sInfo.wCommandText,GET_VALUE=command
  
           ;; this seems needed because otherwise we'd get an array
           ;; of strings
           command=command[0]
           IF command NE '' THEN BEGIN
              needmodel = 0
              mpos = strpos(command,'%m')
              WHILE mpos NE -1 DO BEGIN
                 needmodel = 1
                 command = strmid(command,0,mpos)+'model'+strmid(command,mpos+2)
                 mpos = strpos(command,'%m')
              ENDWHILE
              
              IF needmodel THEN BEGIN
                 model = read_spectra_from_path(sInfo.Readlines, $
                                                   sInfo.SelectedPath, $
                                                   prefix=sInfo.PathPrefix)
                 hasmodel = size(model,/type) EQ 8
              ENDIF
              
              needdata = 0
              dpos = strpos(command,'%d')
              WHILE dpos NE -1 DO BEGIN
                 needdata = 1
                 command = strmid(command,0,dpos)+'data'+strmid(command,dpos+2)
                 dpos = strpos(command,'%d')
              ENDWHILE
              
              IF needdata AND (sInfo.SelectedStar NE '') THEN BEGIN
                 data = read_spectra_data(sInfo.SelectedStar, $
                                       sInfo.StarTable, $
                                       prefix=sInfo.StarDataPrefix)
                 hasdata = size(data,/type) EQ 8
                 IF hasdata AND hasmodel THEN BEGIN
                    match_factor = read_spectra_match_data(model,data,mrange=sInfo.mrange)
                    model.full.data.flux  = model.full.data.flux  * match_factor
                    model.full.data.stdev = model.full.data.stdev * match_factor
                    model.star.data.flux  = model.star.data.flux  * match_factor
                    model.star.data.stdev = model.star.data.stdev * match_factor
                 ENDIF
              ENDIF
              
              IF hasmodel THEN BEGIN
                 CASE sInfo.PlotType OF
                    'ps': BEGIN
                       sh_ps,sInfo.PlotName,/color,xsize=20,ysize=20
                       foo = execute(command[0])
                       sh_ps
                    END
                    ELSE: BEGIN
                       wset,sInfo.PlotID
                       foo = execute(command[0])
                    END
                 ENDCASE
                 
                 sInfo.PlotType='x'
              ENDIF ELSE BEGIN
                 message,/info,'The requested model could not be read'
              ENDELSE
           ENDIF
        ENDIF
     END
     
     'COMMAND' : BEGIN
        ;; We have a new plot command. Check to see if any model was
        ;; selected in the list. If so execute to command by
        ;; simulting a click 
        idx=WIDGET_INFO(sInfo.wList,/LIST_SELECT)
        IF idx NE -1 THEN BEGIN
           WIDGET_CONTROL,sInfo.wList, $
                          SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:idx, CLICKS:1L}
        ENDIF
     END             
     
     'STARCOMBO' : BEGIN

        OldSelectedStar = sInfo.SelectedStar

        ;; Set the selected star
        IF sEvent.Index NE 0 THEN BEGIN
           ;; combi index -1 because of none in element 0
           sInfo.SelectedStar = sInfo.StarTable.naamster[sEvent.Index-1]
        ENDIF ELSE BEGIN
           sInfo.SelectedStar = ''
        ENDELSE
        
        IF sInfo.SelectedStar NE OldSelectedStar THEN BEGIN
           idx=WIDGET_INFO(sInfo.wList,/LIST_SELECT)
           IF idx NE -1 THEN BEGIN
              WIDGET_CONTROL,sInfo.wList, $
                             SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:idx, CLICKS:1L}
           ENDIF
        ENDIF
     END             
     
     'PSFILE' : BEGIN
        idx=WIDGET_INFO(sInfo.wList,/LIST_SELECT)
        IF idx NE -1 THEN BEGIN
           outfile = dialog_pickfile(/write,file='spectra_plot.ps',filter='*.ps')
           IF outfile NE '' THEN BEGIN
              sInfo.PlotType='ps'
              sInfo.PlotName=outfile
              WIDGET_CONTROL,sInfo.wList, $
                             SEND_EVENT={WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:idx, CLICKS:1L}
           ENDIF
        ENDIF
     END                        ;       of  PSFILE
     
     'CLOSE' : BEGIN
        WIDGET_CONTROL, sEvent.top, /DESTROY
        return
     END             
     
     'HELP' : BEGIN
        htxt = 'Read Spectra Model data.'
        htxt = [htxt,'Function that takes a batch file for reading the result from a model']
        htxt = [htxt,'grid calculation. It assumes that the grid is organised into a']
        htxt = [htxt,'directory tree like thus: model1 = readmodel']
        htxt = [htxt,'("/parameter1:value1/parameter2:value2/../parameterX:valueX/") from']
        htxt = [htxt,'this it will determine which parameters have been varied and which values']
        htxt = [htxt,'they can have. Next it will built a widget which allows the user to']
        htxt = [htxt,'put constraints on the parameters and the list of matching models is']
        htxt = [htxt,'updated. By selecting a model from the list its parameters are shown and']
        htxt = [htxt,'(optionally) a command is executed using that model output. By Double']
        htxt = [htxt,'clicking the model the widget is exited and the clicked model is']
        htxt = [htxt,'returned.']
        display_text_widget,htxt,title='Read Spectra Model Help'
        
     END ;; help
     
     ELSE:                      ;  do nothing
  ENDCASE
  
  WIDGET_CONTROL, sEvent.top, SET_UVALUE=sInfo,/NO_COPY
  
END


;; Function that takes a batch file for reading the result from a
;; model grid calculation. It assumes that the grid is organised into
;; a directory tree like thus:
;; model1 = readmodel("/parameter1:value1/parameter2:value2/../parameterX:valueX/")
;; from this it will determine which params have been varied and which
;; values they can have. Next it will built a widget which allows the
;; user to put constraints on the parameters and the list of matching
;; models is updated. By selecting a from the list it's parameters are
;; shown and (optionally) a command is executed using that model
;; output.
;; By Double clicking the model the widget is exited and the clicked
;; model is returned
;; If the no_block keyword is given the function operates like a
;; procedure that does not return anything but instead can only
;; display the models. This is primarily used by the display_model
;; procedure (see display_model.pro

FUNCTION read_spectra,filename,command=command,separator=separator, $
                         no_block=no_block,plotwindow=plotwindow, $
                         prefix=prefix,mrange=mrange

;; if this is set then do not block the widget but instead return
;; right after creating the widget
  default,no_block,0

  default,prefix,'/home/nele/thesispcklas/'
  default,mrange,[3.5,4.5]
  
;; if this is set then create an draw widget to use for the plotting
;; routing instead of using the standard plotting device if any
  plotwindow = keyword_set(plotwindow)
  
  default,command,'read_modust3' ;; the command to execute to read a model
  default,separator,':' ;; the string that separates the param name from the value
                        ;; in the dir naming scheme


  default,filename,'readfiles.idl'
  IF NOT file_test(filename,/read) THEN BEGIN
      filename = dialog_pickfile(FILTER='*.*',/MUST_EXIST)
  ENDIF

  ;; now read the file
  batchcontent = read_spectra_read_batch(filename)
  
  ;; have the path to the batch file handy incase we want to read a
  ;; file which is given relative to the batch file
  pathprefix = file_dirname(filename)

;; Is this also relative than make sure we prepend with the absolute
;; path of the current working dir
  IF strmid(pathprefix,0,1) NE '/' THEN BEGIN
      cd,'.',current=currentpath
      pathprefix = currentpath+'/'+pathprefix
  ENDIF
  
  ;; split the readfile into the different parts
  read_spectra_parse_file,batchcontent,separator=separator, $
    command=command, readlines=readlines, models=models, paths=paths, $
    params=params, values=values, uniq_params=uniq_params
  
;; now create the widget
  wbase = widget_base(title="read spectra model", /column, map=0, mbar=barbase)
  
;; Now read the star name table
  StarTable = read_spectra_datatable(status=StarTable_status,prefix=prefix)
  
;;  WIDGET_CONTROL, /MANAGED, wBase
  
;;; change the default font
;  widget_control, wbase, $                            
;                  default_font="-adobe-helvetica-bold-r-normal-*-12-100-*-*-*-*-*"

;; the menu structure
  wfilebutton = widget_button(barbase, value='File')
  wprintbutton = widget_button(barbase, value='Print')
  whelpbutton = widget_button(barbase, value='Help')
  whelpbutton = widget_button(whelpbutton, value='Display help', uvalue='HELP')
  wquitbutton = widget_button(wfilebutton, value='Close', uvalue='CLOSE',/separator)
  wpsprintbutton = widget_button(wprintbutton, value='Postscript', uvalue='PSFILE')
  
  wtopbase = widget_base(wbase, /row)
  
  ;;create a framed base to hold all the param buttons
  wParambase = widget_base(wtopbase,/row,/frame)
  
  FOR i=0,n_elements(uniq_params)-1 DO BEGIN
      ;; now we allow max 10 buttons in one column therefor we create
      ;; a new wparamcolumn for each tenth i
      IF (i MOD 10) EQ 0 THEN BEGIN
          wparamcolumn = widget_base(wparambase, /column)
      ENDIF
      wlabel = widget_label(wparamcolumn, value=uniq_params[i])
      wcombobox = widget_droplist(wparamcolumn,uvalue='COMBOBOX'+n2s(i), $
                                  value=['Any', read_spectra_values_from_param(params,values,uniq_params[i])])
  ENDFOR
  
  wlist = widget_list(wtopbase, value=models,ysize=20, uvalue='LIST')
  wparamlist = widget_list(wtopbase, value=[''],ysize=20,xsize=20, uvalue='PARAMLIST')
  
  IF plotwindow THEN BEGIN
     geometry = widget_info(wList,/geometry)
     wPlot = widget_draw(wtopbase,ysize=geometry.scr_ysize,xsize=geometry.scr_ysize)
  ENDIF
  
  IF StarTable_status THEN BEGIN
     wStarbase = widget_base(wtopbase,/column,/frame)
     wlabel = widget_label(wStarbase, value='Star Name')
     wstarcombo = widget_list(wStarbase, value=['None', StarTable.naamster],ysize=20, uvalue='STARCOMBO')
;     wstarcombo = widget_droplist(wStarbase,uvalue='STARCOMBO', $
;                                  value=)
  ENDIF
  
  wcommandlabel = widget_label(wbase, value='Command to execute. %m will be replace by model,%d will be replaced by starname')
  wcommandtext = widget_text(wbase, /editable, value='plspectra,%m.full,data=%d,/sws', $
                             uvalue='COMMAND')
  
  ;;  realize the widget hierarchy.
  widget_control, wbase, /realize
  
  IF plotwindow THEN BEGIN
     WIDGET_CONTROL, wPlot, GET_VALUE=PlotID
  ENDIF ELSE BEGIN
     PlotID = !D.window>0
  ENDELSE
  
;; array of strings to hold the selection criteria from the comboboxes
  selectioncrit = make_array(n_elements(uniq_params),value='')

  ;; create structure to get info back if used as a function
  IF no_block EQ 0 THEN BEGIN
     ;; create pointer to get the return data from the user after the
     ;; widget is closed and the sinfo structure is not available any longer.
     ptrtoformdata = ptr_new({cancel:1, readcommand:''})
  ENDIF ELSE BEGIN
     ptrtoformdata = 0
  ENDELSE
  
  ;;  create the info structure to pass all info between routines
  sinfo = { $
          separator:separator, $
          values:values, $
          params:params, $
          uniq_params:uniq_params, $
          readlines:readlines, $
          paths:paths, $
          pathprefix:pathprefix, $
          models:models, $
          selectioncrit:selectioncrit, $
          selectedpath:'', $
          wlist: wlist, $
          PlotID: PlotID, $
          wparamlist: wparamlist, $
          wcommandtext: wcommandtext, $
          no_block:no_block, $
          ptrtoformdata:ptrtoformdata, $
          SelectedStar:'', $
          StarDataPrefix:prefix+'/DATA_FILES', $
          StarTable:StarTable, $
          PlotType:'x', $
          PlotName:'', $
          mrange:mrange, $
          dummy:'' $
          }
  
;; store the sinfo in the uvalue of the top widget
  widget_control, wbase, set_uvalue=sinfo, /no_copy
  
  ;;  map the top level base.
  widget_control, wbase, map=1
  
  ;; register with the big guy, xmanager!
  xmanager, "read_spectra_", wbase, $
            event_handler="read_spectra_event", $
            no_block=no_block
  
  IF no_block EQ 0 THEN BEGIN
     ;; things to get back out of the widget
     formdata = *ptrtoformdata
     
     out = 0
     IF n_elements(formdata) EQ 0 THEN begin
        ptr_free, ptrtoformdata
     ENDIF ELSE BEGIN
        ;; Did the user cancel out of the form? If so, return a 0.
        IF formdata.cancel EQ 1 THEN BEGIN
           Ptr_Free, ptrToFormData
        ENDIF ELSE BEGIN
           path = formdata.path
           Ptr_Free, ptrToFormData
           out = read_spectra_from_path(readlines,path,prefix=PathPrefix)
        ENDELSE
     ENDELSE
     return,out
  ENDIF
END                             ; of Read_Spectra
