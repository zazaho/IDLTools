function tags_double2float,struct
  ;; convert double arrays to singles to save place when saving structure

  tagnames = tag_names(struct)
  struct_command = 'out={'

  for i =0,n_elements(tagnames)-1 do begin
     thistag = struct.(i)
     case size(thistag,/tname) of
        'DOUBLE': begin
           foo = execute(tagnames[i]+'=float(thistag)')
        end
        'STRUCT': begin
           foo = execute(tagnames[i]+'=tags_double2float(thistag)')
        end
        else: begin
           foo = execute(tagnames[i]+'=thistag')
        end
     endcase

     struct_command = struct_command+tagnames[i]+':'+tagnames[i]
     if i ne n_elements(tagnames)-1 then begin
        struct_command = struct_command+','
     endif else begin
        struct_command = struct_command+'}'
     endelse
     
  endfor
  foo = execute(struct_command)
  return,out
end
