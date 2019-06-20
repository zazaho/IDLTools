pro get_calibration_camera, calibration_file, obs1_config=obs1_config

@obs1_config



calibration_table=work_dir+'apexpro/'+calibration_table   ; le fichier calibration_table est place dans apexpro

openr, lun, calibration_table ,/GET_LUN                    ; ouverture du fichier
nlines=File_lines(calibration_table)                       ; lecture du nombre de lignes
table=strarr(nlines)                                       ; generation d'une table de caracteres de longueur nlines
readf, lun, table                                          ; on stocke la lecture du fichier calibration_table dans table


compteur=0 
    
number=['0','1','2','3','4','5','6','7','8','9']
    
for i=0, nlines-1  do begin 


    v=strmatch(number,strmid(strcompress(table(i)),0,1))   ; reperage si les lignes de calibration_table commence par un chiffre ou non

    if (total(v) eq 0) then begin                          ; si ca ne commence pas par un chiffre : du blanc ou du texte, on incremente
    compteur=compteur+1                                    ; la ligne compteur est la ligne a partir de laquelle on peut extraire l'information necessaire
    endif else begin 
    
    endelse 

endfor


table2=strarr(nlines-compteur,5);                         ; deuxieme table qui va ne contenir que les lignes utiles, et 4 colonnes pour scan_number, Conv, goodpix_ima 
                                                           ;flatfield et calibration_camera 


for i=0, nlines-compteur-1  do begin

table(compteur+i)=strjoin(strsplit(table(compteur+i),/EXTRACT),'-')   ; pour une ligne utile, les blancs sont elimines et remplaces par un '-'
table2(i,*)=strsplit(table(compteur+i),/EXTRACT,'-')                  ; extraction des sous chaines de caracteres (4 par ligne)


endfor


                                          ; conversion string en entier du scan_number
scan_table=intarr(nlines-compteur)                                     
scan_table=table2(*,0) 
                                           ; contient la liste des scan_number dont nous connaissons quelques informations

min_index=min(where(scan_number le scan_table))
max_index=max(where(scan_number ge scan_table))

if (where(scan_number eq scan_table) ne -1) then begin

index_c=where(scan_number eq scan_table);                              ; si le scan_number se trouve dans la scan_table 

endif else begin
                                                                       ; si le scan_number ne se trouve pas dans la scan_table (cas le plus frequent)
index_c=min(min_index,max_index)                                       ; on prend la ligne correspondant au numero de scan le plus proche inferieurement. Chaque ligne
                                                                       ; contient des fichiers de calibration differents 
                                                                       ; de calibration, prendre la valeur superieur ne changerait rien d'ailleurs
endelse

calibration_file=table2(index_c,4)




end
