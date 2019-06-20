function struct2mr,s,auto=auto

  default,auto,0

  tagnames=tag_names(s)
  ntags=n_tags(s)

  fields=make_array(ntags,value='')
  names=make_array(ntags,value='')
  units=make_array(ntags,value='')
  descriptions=make_array(ntags,value='')
  selected_tags=make_array(ntags,value=0)

  if auto ne 0 then begin
     names = tagnames
     units = make_array(ntags,value='---')
     selected_tags[*] = 1
     for tt=0,ntags-1 do begin
        fields[tt]=size(s.(tt),/tname)
     endfor
  endif else begin
     for tt=0,ntags-1 do begin
        print,tagnames[tt]
        print,'Include tag '+tagnames[tt]+' [yes] (quit to stop):'
        ans=''
        read,ans
        
        print,strmid(ans,0,1)
        if strlowcase(strmid(ans,0,1)) eq 'q' then break
        
        if ans eq '' or strlowcase(strmid(ans,0,1)) eq 'y' then begin
           fields[tt]=size(s.(tt),/tname)
           
           print,'Name ['+tagnames[tt]+']: '
           ans=''
           read,ans
           if ans eq '' then ans=tagnames[tt]
           names[tt] = ans
           
           print,'Units [---]: '
           ans=''
           read,ans
           if ans eq '' then ans='---'
           units[tt] = ans
           
           print,'Description []: '
           ans=''
           read,ans
           descriptions[tt] = ans
           
           selected_tags[tt] = 1
        endif
     endfor
  endelse
  
  idx_selected_tags = where(selected_tags eq 1,cnt)
  if cnt ne 0 then begin

     fields=fields[idx_selected_tags]
     names=names[idx_selected_tags]
     units=units[idx_selected_tags]
     descriptions=descriptions[idx_selected_tags]
     
     length = n_elements(s.(idx_selected_tags[0]))
     mr=make_mr(length,fields=fields,names=names,units=units,descriptions=descriptions)
     
     for ii=0,cnt-1 do begin
        mr.data.(ii) = s.(idx_selected_tags[ii])
     endfor

     return,mr

  endif else begin
     return,0
  endelse
end
