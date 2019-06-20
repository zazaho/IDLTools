PRO WhichXY,x,y,_extra=_extra
  !err = 0
  Cursor,x,y,2,/down,_extra=_extra
  WHILE !err ne 4 DO BEGIN
    !err = 0
    print,x,y
    cursor,x,y,2,/down,_extra=_extra
 ENDWHILE
END 
