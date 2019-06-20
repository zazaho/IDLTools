FUNCTION kleur,kl,longlist=longlist
  
  nkl = n_elements(kl)
  IF nkl EQ 0 THEN BEGIN
    print,'please supply (a) color value(s) or name(s)'
    return,0
  ENDIF
  
  names = ['aquamarine','azure','beige','bisque','black','blue','brown','crimson','cyan','dimgray','firebrick','gold', $
           'gray','green','hotpink','indigo','ivory','khaki','lavender','lime','magenta','maroon','navy', $
           'olive','orange','pink','purple','red','salmon','silver','teal','tomato','turquoise','violet', $
           'white','yellow','20gray','40gray','60gray','80gray']
  
; The corresponding red green and blue values
  r = [127,240,245,255,0,0,165,220,0,105,178,255,128,0,255,75,255,240,230,0,255,128,0,128,255,255,128,255,250,192,0, $
       255,64,238,255,255,51,102,153,204]
  
  g = [255,255,245,228,0,0,42,20,255,105,34,215,128,128,105,0,255,230,230,255,0,0,0,128,165,192,0,0,128,192,128,99,224, $
       130,255,255,51,102,153,204]
  
  b = [212,255,220,196,0,255,42,60,255,105,34,0,128,0,180,130,240,140,250,0,255,0,128,0,0,203,128,0,114,192,128,71,208, $
       238,255,0,51,102,153,204]
    
  ;; get the indices of the supplied colors of the standard colors 
  ;; If we get a string (array) then first find the index
  ;; Otherwise just use the index
  s = size(kl)
  IF s[s[0]+1] EQ 7 THEN BEGIN
    IF NOT keyword_set(longlist) THEN BEGIN
      ;; Take only the first 3 characters, which is already unique
      names3=strmid(names,0,3)
      kl = strlowcase(strmid(kl,0,3))
    ENDIF ELSE BEGIN
      ;; much longer table of rgb values
      restore,shell_expand('~/IA_FILES/rgb_template.sav')
      rgb = read_ascii(shell_expand('~/IA_FILES/rgb.csv'),template=rgb_template)
      
      r = rgb.red
      g = rgb.green
      b = rgb.blue
      names3 = rgb.name
    ENDELSE
;; Now find the index of the requested color
    Na = n_elements(names3)
    l = lindgen(Na,Nkl)
    AA = names3[l mod Na]
    BB = kl[l / Na]
    kl = where(AA EQ BB) MOD Na
    IF kl[0] EQ -1 THEN BEGIN
      print,"you didn't supply a valid color name"
      print,'valid names are:'
      print,names
      return,0
    ENDIF
  ENDIF ELSE BEGIN
    print,'KLEUR: getting: ',names[kl]
  ENDELSE
  
; Now read the current red,green and blue values in
  tvlct,curr,curg,curb,/get
  
;  Now find for each index the requested closest match
  out = kl
; stop
  
  FOR i=0,nkl-1 DO BEGIN
    foo = min(abs(curr-r[kl[i]])+ $
              abs(curg-g[kl[i]])+ $
              abs(curb-b[kl[i]]),minidx)
    out[i] = minidx
  ENDFOR
  ;; simple input requires simple output
  IF nkl EQ 1 THEN return,out[0]
  return,out
END
